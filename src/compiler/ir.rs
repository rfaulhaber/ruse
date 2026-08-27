//! Lowering: the reader's [`Expr`] tree into the typed Core IR.
//!
//! This pass owns *syntax*: special-form recognition, form arity, and the R7RS rule that
//! a lexical binding shadows a syntactic keyword — `(let ((if list)) (if 1 2 3))` calls
//! the variable `if`. To honour that, lowering carries a stack of bound names even though
//! register assignment belongs to the emitter.
//!
//! The core forms are `quote`, `if`, `define`, `set!`, `lambda`, `begin`, `let` and
//! `letrec`/`letrec*`; everything else the M4 slice accepts — `and`, `or`, `when`,
//! `unless`, `cond`, `case`, `let*`, named `let`, `do`, `let-values`/`let*-values`,
//! `define-values`, `quasiquote` — is *derived*: lowered here by construction into the
//! core IR, exactly the desugarings R7RS §7.3 writes as macros. M8 re-expresses these as
//! a bootstrap `syntax-rules` prelude; until then two hygiene limits are accepted: the
//! generated calls to `cons`/`append`/`list`/`eqv?` resolve like ordinary code (a user
//! rebinding of those names leaks into `quasiquote` and `case`), and `else`/`=>` are
//! matched literally when not lexically bound. Every R7RS keyword of a later milestone
//! is *recognized* and refused with the milestone that will implement it, which reads
//! far better than the unbound-variable error the same head would otherwise produce at
//! runtime.

use crate::ast::Expr;
use crate::span::Span;

use super::{CompileError, ss};

/// The typed Core IR (decision C).
pub(crate) enum Ir {
    /// A literal datum: self-evaluating atoms and quoted structure. The emitter builds
    /// the runtime value and pools it.
    Datum(Expr),
    Var(String, Span),
    If {
        cond: Box<Ir>,
        then: Box<Ir>,
        els: Option<Box<Ir>>,
        span: Span,
    },
    Lambda(IrLambda),
    Call {
        head: Box<Ir>,
        args: Vec<Ir>,
        span: Span,
    },
    /// Top-level definition. Lowering guarantees this never appears inside a body —
    /// internal definitions become [`Ir::LetRec`].
    Define {
        name: String,
        value: Box<Ir>,
        span: Span,
    },
    Set {
        name: String,
        value: Box<Ir>,
        span: Span,
    },
    Begin {
        body: Vec<Ir>,
        span: Span,
    },
    Let {
        bindings: Vec<(String, Ir)>,
        body: Vec<Ir>,
        span: Span,
    },
    /// `letrec*` semantics: all names in scope over every init, inits evaluated
    /// left-to-right into black-holed registers. `letrec`, internal defines and the
    /// loops of named `let`/`do` all lower to this.
    LetRec {
        bindings: Vec<(String, Ir)>,
        body: Vec<Ir>,
        span: Span,
    },
}

pub(crate) struct IrLambda {
    /// The defined name, when `define` sugar supplied one; prototypes carry it for
    /// listings and arity errors.
    pub(crate) name: Option<String>,
    pub(crate) params: Vec<String>,
    /// The rest parameter of a variadic lambda; extra arguments arrive in it as a list.
    pub(crate) rest: Option<String>,
    pub(crate) body: Vec<Ir>,
    pub(crate) span: Span,
}

impl Ir {
    pub(crate) fn span(&self) -> Span {
        match self {
            Ir::Datum(e) => e.span(),
            Ir::Var(_, span) => *span,
            Ir::If { span, .. }
            | Ir::Call { span, .. }
            | Ir::Define { span, .. }
            | Ir::Set { span, .. }
            | Ir::Begin { span, .. }
            | Ir::Let { span, .. }
            | Ir::LetRec { span, .. } => *span,
            Ir::Lambda(l) => l.span,
        }
    }
}

/// Keywords the compiler knows it does not compile yet, and when it will.
fn unsupported_milestone(name: &str) -> Option<&'static str> {
    Some(match name {
        "guard" | "parameterize" => "M7",
        "define-syntax" | "let-syntax" | "letrec-syntax" | "syntax-rules"
        | "define-record-type" | "import" | "include" | "include-ci" | "cond-expand" => "M8",
        "delay" | "delay-force" | "case-lambda" => "M9",
        _ => return None,
    })
}

/// Lower one top-level form.
pub(crate) fn lower_toplevel(expr: &Expr) -> Result<Ir, CompileError> {
    Lowerer::default().form(expr, true)
}

/// How one internal definition binds, before its value is lowered.
enum DefBody<'a> {
    /// `(define name value)` — also `(define-values (name) value)`.
    Value(&'a Expr),
    /// `(define-values name value)`: the single value, wrapped in a fresh list.
    ValueAsList(&'a Expr),
    /// `(define (name . formals) body...)` sugar.
    Proc {
        params: Vec<String>,
        rest: Option<String>,
        body: &'a [Expr],
    },
}

struct PendingDef<'a> {
    name: String,
    body: DefBody<'a>,
    span: Span,
}

#[derive(Default)]
struct Lowerer {
    /// Names bound by enclosing binding forms, innermost scope last. Only membership
    /// matters here; the emitter assigns registers.
    scopes: Vec<Vec<String>>,
    /// Counter for compiler-introduced temporaries. Their names start with a space,
    /// which no readable identifier can, so they can never capture user code.
    tmp: usize,
}

impl Lowerer {
    fn is_bound(&self, name: &str) -> bool {
        self.scopes
            .iter()
            .any(|scope| scope.iter().any(|n| n == name))
    }

    /// A keyword position: the head is `name`, spelled literally and not shadowed by a
    /// lexical binding. Used for `else` and `=>`, which are auxiliary syntax rather
    /// than special forms.
    fn is_keyword(&self, e: &Expr, name: &str) -> bool {
        matches!(e, Expr::Symbol(s, _) if s == name && !self.is_bound(s))
    }

    fn gensym(&mut self, tag: &str) -> String {
        self.tmp += 1;
        format!(" %{tag}{}", self.tmp)
    }

    fn with_scope<T>(
        &mut self,
        names: Vec<String>,
        f: impl FnOnce(&mut Self) -> Result<T, CompileError>,
    ) -> Result<T, CompileError> {
        self.scopes.push(names);
        let result = f(self);
        self.scopes.pop();
        result
    }

    /// Lower a sequence of expressions (no definitions allowed), each for effect but
    /// the last.
    fn sequence(&mut self, exprs: &[Expr]) -> Result<Vec<Ir>, CompileError> {
        exprs.iter().map(|e| self.form(e, false)).collect()
    }

    /// Lower `expr`. `top` is true only at the top level and inside a top-level `begin`,
    /// the two places R7RS lets `define` create a *global*; body-internal defines are
    /// consumed by [`Lowerer::body_ir`] before this ever sees them.
    fn form(&mut self, expr: &Expr, top: bool) -> Result<Ir, CompileError> {
        match expr {
            Expr::Integer(..)
            | Expr::Number(..)
            | Expr::String(..)
            | Expr::Character(..)
            | Expr::Boolean(..) => Ok(Ir::Datum(expr.clone())),
            Expr::Symbol(name, span) => Ok(Ir::Var(name.clone(), *span)),
            Expr::Quote(inner, _) => Ok(Ir::Datum((**inner).clone())),
            Expr::Quasiquote(inner, _) => self.quasi(inner, 1),
            Expr::Unquote(_, span) | Expr::UnquoteSplicing(_, span) => {
                Err(CompileError::BadSyntax {
                    form: "unquote",
                    detail: "unquote is only meaningful inside quasiquote".to_string(),
                    span: ss(*span),
                })
            }
            Expr::DottedList(_, _, span) => Err(CompileError::BadSyntax {
                form: "expression",
                detail: "a dotted list is not an expression".to_string(),
                span: ss(*span),
            }),
            Expr::List(elems, span) => self.list_form(elems, *span, top),
        }
    }

    fn list_form(&mut self, elems: &[Expr], span: Span, top: bool) -> Result<Ir, CompileError> {
        let Some(head) = elems.first() else {
            return Err(CompileError::BadSyntax {
                form: "application",
                detail: "() is not an expression".to_string(),
                span: ss(span),
            });
        };
        if let Expr::Symbol(name, _) = head
            && !self.is_bound(name)
        {
            match name.as_str() {
                "quote" => return self.quote(elems, span),
                "if" => return self.if_form(elems, span),
                "define" => return self.define(elems, span, top),
                "define-values" => return self.define_values(elems, span, top),
                "set!" => return self.set(elems, span),
                "lambda" => return self.lambda(elems, span, None),
                "begin" => return self.begin(elems, span, top),
                "let" => return self.let_form(elems, span),
                "let*" => return self.let_star(elems, span),
                "letrec" | "letrec*" => return self.letrec(elems, span),
                "and" => return self.and_chain(&elems[1..], span),
                "or" => return self.or_chain(&elems[1..], span),
                "when" => return self.when_unless(elems, span, true),
                "unless" => return self.when_unless(elems, span, false),
                "cond" => return self.cond(elems, span),
                "case" => return self.case(elems, span),
                "do" => return self.do_form(elems, span),
                "let-values" => return self.let_values(elems, span, false),
                "let*-values" => return self.let_values(elems, span, true),
                "quasiquote" => return self.quasiquote_form(elems, span),
                "unquote" | "unquote-splicing" => {
                    return Err(CompileError::BadSyntax {
                        form: "unquote",
                        detail: "unquote is only meaningful inside quasiquote".to_string(),
                        span: ss(span),
                    });
                }
                other => {
                    if let Some(milestone) = unsupported_milestone(other) {
                        return Err(CompileError::Unsupported {
                            form: other.to_string(),
                            milestone,
                            span: ss(span),
                        });
                    }
                }
            }
        }
        // An ordinary application.
        let head = Box::new(self.form(head, false)?);
        let args = elems[1..]
            .iter()
            .map(|arg| self.form(arg, false))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Ir::Call { head, args, span })
    }

    fn quote(&mut self, elems: &[Expr], span: Span) -> Result<Ir, CompileError> {
        match elems {
            [_, datum] => Ok(Ir::Datum(datum.clone())),
            _ => Err(CompileError::BadSyntax {
                form: "quote",
                detail: format!("expected exactly one datum, got {}", elems.len() - 1),
                span: ss(span),
            }),
        }
    }

    fn if_form(&mut self, elems: &[Expr], span: Span) -> Result<Ir, CompileError> {
        match elems {
            [_, cond, then] => Ok(Ir::If {
                cond: Box::new(self.form(cond, false)?),
                then: Box::new(self.form(then, false)?),
                els: None,
                span,
            }),
            [_, cond, then, els] => Ok(Ir::If {
                cond: Box::new(self.form(cond, false)?),
                then: Box::new(self.form(then, false)?),
                els: Some(Box::new(self.form(els, false)?)),
                span,
            }),
            _ => Err(CompileError::BadSyntax {
                form: "if",
                detail: format!("expected 2 or 3 parts, got {}", elems.len() - 1),
                span: ss(span),
            }),
        }
    }

    // ------------------------------------------------------------ define

    fn define(&mut self, elems: &[Expr], span: Span, top: bool) -> Result<Ir, CompileError> {
        if !top {
            return Err(CompileError::BadSyntax {
                form: "define",
                detail: "definitions are only allowed at the top level or at the \
                         beginning of a body"
                    .to_string(),
                span: ss(span),
            });
        }
        let pending = self.parse_define(elems, span)?;
        let name = pending.name.clone();
        let value = self.lower_pending(pending)?;
        Ok(Ir::Define {
            name,
            value: Box::new(value),
            span,
        })
    }

    fn define_values(&mut self, elems: &[Expr], span: Span, top: bool) -> Result<Ir, CompileError> {
        if !top {
            return Err(CompileError::BadSyntax {
                form: "define-values",
                detail: "definitions are only allowed at the top level or at the \
                         beginning of a body"
                    .to_string(),
                span: ss(span),
            });
        }
        let pending = self.parse_define_values(elems, span)?;
        let name = pending.name.clone();
        let value = self.lower_pending(pending)?;
        Ok(Ir::Define {
            name,
            value: Box::new(value),
            span,
        })
    }

    /// Structurally parse one `define` form, without lowering its value — internal
    /// defines must collect every bound name before any init is lowered.
    fn parse_define<'a>(
        &mut self,
        elems: &'a [Expr],
        span: Span,
    ) -> Result<PendingDef<'a>, CompileError> {
        match elems {
            // (define name value)
            [_, Expr::Symbol(name, _), value] => Ok(PendingDef {
                name: name.clone(),
                body: DefBody::Value(value),
                span,
            }),
            // (define (name params...) body...) and (define (name . rest) body...)
            [_, Expr::List(header, hspan), ..] if elems.len() >= 3 => {
                let (name, params) = split_proc_header(header, *hspan)?;
                Ok(PendingDef {
                    name,
                    body: DefBody::Proc {
                        params,
                        rest: None,
                        body: &elems[2..],
                    },
                    span,
                })
            }
            [_, Expr::DottedList(header, tail, hspan), ..] if elems.len() >= 3 => {
                let (name, params) = split_proc_header(header, *hspan)?;
                let Expr::Symbol(rest, _) = tail.as_ref() else {
                    return Err(CompileError::BadSyntax {
                        form: "define",
                        detail: "the rest parameter must be a symbol".to_string(),
                        span: ss(*hspan),
                    });
                };
                Ok(PendingDef {
                    name,
                    body: DefBody::Proc {
                        params,
                        rest: Some(rest.clone()),
                        body: &elems[2..],
                    },
                    span,
                })
            }
            _ => Err(CompileError::BadSyntax {
                form: "define",
                detail: "expected (define name value) or (define (name params...) body...)"
                    .to_string(),
                span: ss(span),
            }),
        }
    }

    /// `define-values`, in the single-value slice M4 can honour: one required formal
    /// binds the init's (sole) value; a lone rest formal binds the list of all of them.
    /// Formals that need a different value count wait for M7's multiple values.
    fn parse_define_values<'a>(
        &mut self,
        elems: &'a [Expr],
        span: Span,
    ) -> Result<PendingDef<'a>, CompileError> {
        match elems {
            [_, Expr::List(formals, _), value] => match formals.as_slice() {
                [Expr::Symbol(name, _)] => Ok(PendingDef {
                    name: name.clone(),
                    body: DefBody::Value(value),
                    span,
                }),
                _ => Err(CompileError::Unsupported {
                    form: "define-values with other than one variable".to_string(),
                    milestone: "M7",
                    span: ss(span),
                }),
            },
            [_, Expr::Symbol(name, _), value] => Ok(PendingDef {
                name: name.clone(),
                body: DefBody::ValueAsList(value),
                span,
            }),
            _ => Err(CompileError::BadSyntax {
                form: "define-values",
                detail: "expected (define-values (formals...) value)".to_string(),
                span: ss(span),
            }),
        }
    }

    /// Lower a parsed definition's value, in the current scope.
    fn lower_pending(&mut self, pending: PendingDef<'_>) -> Result<Ir, CompileError> {
        match pending.body {
            DefBody::Value(value) => {
                let mut value = self.form(value, false)?;
                // A lambda defined this way inherits the definition's name, so listings
                // and arity errors can say `fact` instead of `#<procedure>`.
                if let Ir::Lambda(l) = &mut value
                    && l.name.is_none()
                {
                    l.name = Some(pending.name.clone());
                }
                Ok(value)
            }
            DefBody::ValueAsList(value) => {
                let arg = self.form(value, false)?;
                Ok(Ir::Call {
                    head: Box::new(Ir::Var("list".to_string(), pending.span)),
                    args: vec![arg],
                    span: pending.span,
                })
            }
            DefBody::Proc { params, rest, body } => {
                self.lambda_parts(Some(pending.name), params, rest, body, pending.span)
            }
        }
    }

    // ------------------------------------------------------------ bodies

    /// Lower a ⟨body⟩: a define prefix (spliced through all-definition `begin`s)
    /// becomes one `letrec*` around the remaining expressions, per R7RS §5.3.2.
    fn body_ir(&mut self, exprs: &[Expr], span: Span) -> Result<Vec<Ir>, CompileError> {
        let mut defs = Vec::new();
        let mut consumed = 0;
        for e in exprs {
            if !self.append_define(e, &mut defs)? {
                break;
            }
            consumed += 1;
        }
        if defs.is_empty() {
            return self.sequence(exprs);
        }
        let rest = &exprs[consumed..];
        if rest.is_empty() {
            return Err(CompileError::BadSyntax {
                form: "body",
                detail: "a body needs at least one expression after its definitions".to_string(),
                span: ss(span),
            });
        }
        for (i, d) in defs.iter().enumerate() {
            if defs[..i].iter().any(|other| other.name == d.name) {
                return Err(CompileError::BadSyntax {
                    form: "define",
                    detail: format!("`{}` is defined twice in this body", d.name),
                    span: ss(d.span),
                });
            }
        }
        let names: Vec<String> = defs.iter().map(|d| d.name.clone()).collect();
        self.with_scope(names, |me| {
            let mut bindings = Vec::with_capacity(defs.len());
            for d in defs {
                let name = d.name.clone();
                let value = me.lower_pending(d)?;
                bindings.push((name, value));
            }
            let body = me.sequence(rest)?;
            Ok(vec![Ir::LetRec {
                bindings,
                body,
                span,
            }])
        })
    }

    /// If `e` is a definition, parse it into `defs` and return true; false ends the
    /// define prefix. A `(begin ...)` holding only definitions splices (it *is* a
    /// definition, grammatically); one holding any expression is an expression.
    fn append_define<'a>(
        &mut self,
        e: &'a Expr,
        defs: &mut Vec<PendingDef<'a>>,
    ) -> Result<bool, CompileError> {
        let Expr::List(elems, span) = e else {
            return Ok(false);
        };
        let Some(Expr::Symbol(head, _)) = elems.first() else {
            return Ok(false);
        };
        if self.is_bound(head) {
            return Ok(false);
        }
        match head.as_str() {
            "define" => {
                defs.push(self.parse_define(elems, *span)?);
                Ok(true)
            }
            "define-values" => {
                defs.push(self.parse_define_values(elems, *span)?);
                Ok(true)
            }
            "begin" if elems.len() > 1 && elems[1..].iter().all(|s| self.looks_like_define(s)) => {
                for sub in &elems[1..] {
                    self.append_define(sub, defs)?;
                }
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn looks_like_define(&self, e: &Expr) -> bool {
        let Expr::List(elems, _) = e else {
            return false;
        };
        let Some(Expr::Symbol(head, _)) = elems.first() else {
            return false;
        };
        if self.is_bound(head) {
            return false;
        }
        match head.as_str() {
            "define" | "define-values" => true,
            "begin" => elems.len() > 1 && elems[1..].iter().all(|s| self.looks_like_define(s)),
            _ => false,
        }
    }

    // ------------------------------------------------------------ set! and lambda

    fn set(&mut self, elems: &[Expr], span: Span) -> Result<Ir, CompileError> {
        match elems {
            [_, Expr::Symbol(name, _), value] => Ok(Ir::Set {
                name: name.clone(),
                value: Box::new(self.form(value, false)?),
                span,
            }),
            _ => Err(CompileError::BadSyntax {
                form: "set!",
                detail: "expected (set! name value)".to_string(),
                span: ss(span),
            }),
        }
    }

    fn lambda(
        &mut self,
        elems: &[Expr],
        span: Span,
        name: Option<String>,
    ) -> Result<Ir, CompileError> {
        match elems {
            [_, Expr::List(params, pspan), ..] if elems.len() >= 3 => {
                let params = symbol_names(params, "lambda", *pspan)?;
                self.lambda_parts(name, params, None, &elems[2..], span)
            }
            // (lambda args body...): everything lands in the rest list.
            [_, Expr::Symbol(rest, _), ..] if elems.len() >= 3 => {
                self.lambda_parts(name, Vec::new(), Some(rest.clone()), &elems[2..], span)
            }
            [_, Expr::DottedList(params, tail, pspan), ..] if elems.len() >= 3 => {
                let params = symbol_names(params, "lambda", *pspan)?;
                let Expr::Symbol(rest, _) = tail.as_ref() else {
                    return Err(CompileError::BadSyntax {
                        form: "lambda",
                        detail: "the rest parameter must be a symbol".to_string(),
                        span: ss(*pspan),
                    });
                };
                self.lambda_parts(name, params, Some(rest.clone()), &elems[2..], span)
            }
            _ => Err(CompileError::BadSyntax {
                form: "lambda",
                detail: "expected (lambda (params...) body...)".to_string(),
                span: ss(span),
            }),
        }
    }

    fn lambda_parts(
        &mut self,
        name: Option<String>,
        params: Vec<String>,
        rest: Option<String>,
        body: &[Expr],
        span: Span,
    ) -> Result<Ir, CompileError> {
        let mut all: Vec<String> = params.clone();
        all.extend(rest.clone());
        for (i, p) in all.iter().enumerate() {
            if all[..i].contains(p) {
                return Err(CompileError::BadSyntax {
                    form: "lambda",
                    detail: format!("duplicate parameter `{p}`"),
                    span: ss(span),
                });
            }
        }
        self.with_scope(all, |me| {
            let body = me.body_ir(body, span)?;
            Ok(Ir::Lambda(IrLambda {
                name,
                params,
                rest,
                body,
                span,
            }))
        })
    }

    fn begin(&mut self, elems: &[Expr], span: Span, top: bool) -> Result<Ir, CompileError> {
        if elems.len() == 1 && !top {
            return Err(CompileError::BadSyntax {
                form: "begin",
                detail: "(begin) needs at least one expression".to_string(),
                span: ss(span),
            });
        }
        // A top-level begin's defines are top-level defines (R7RS §5.1). Full splice
        // *sequencing* — each subform compiled only after its predecessors ran, so a
        // define here can revoke a later subform's inlining licence — is Vm::eval_expr's,
        // which splits a top-level begin into separate compilation units before this
        // lowering ever sees one. Compiling a whole begin through this path treats it as
        // one unit: binding visibility is right, licence revocation lags by one form.
        let body = elems[1..]
            .iter()
            .map(|e| self.form(e, top))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Ir::Begin { body, span })
    }

    // ------------------------------------------------------------ the let family

    /// Parse a `((name init) ...)` binding list, rejecting duplicates.
    fn binding_pairs<'a>(
        &mut self,
        forms: &'a [Expr],
        form: &'static str,
    ) -> Result<Vec<(String, &'a Expr)>, CompileError> {
        let mut pairs: Vec<(String, &Expr)> = Vec::with_capacity(forms.len());
        for f in forms {
            let Expr::List(pair, pspan) = f else {
                return Err(CompileError::BadSyntax {
                    form,
                    detail: "each binding must be (name init)".to_string(),
                    span: ss(f.span()),
                });
            };
            let [Expr::Symbol(name, _), init] = pair.as_slice() else {
                return Err(CompileError::BadSyntax {
                    form,
                    detail: "each binding must be (name init)".to_string(),
                    span: ss(*pspan),
                });
            };
            if pairs.iter().any(|(n, _)| n == name) {
                return Err(CompileError::BadSyntax {
                    form,
                    detail: format!("duplicate binding `{name}`"),
                    span: ss(*pspan),
                });
            }
            pairs.push((name.clone(), init));
        }
        Ok(pairs)
    }

    fn let_form(&mut self, elems: &[Expr], span: Span) -> Result<Ir, CompileError> {
        match elems {
            [_, Expr::Symbol(..), ..] => self.named_let(elems, span),
            [_, Expr::List(binding_forms, _), ..] if elems.len() >= 3 => {
                let pairs = self.binding_pairs(binding_forms, "let")?;
                // Plain let: inits see only the outer scope.
                let mut bindings = Vec::with_capacity(pairs.len());
                for (name, init) in pairs {
                    let init = self.form(init, false)?;
                    bindings.push((name, init));
                }
                let names = bindings.iter().map(|(n, _)| n.clone()).collect();
                let body = self.with_scope(names, |me| me.body_ir(&elems[2..], span))?;
                Ok(Ir::Let {
                    bindings,
                    body,
                    span,
                })
            }
            _ => Err(CompileError::BadSyntax {
                form: "let",
                detail: "expected (let ((name init)...) body...)".to_string(),
                span: ss(span),
            }),
        }
    }

    /// Named `let` is its R7RS §7.3 desugaring: a one-binding `letrec` over a lambda,
    /// applied to the inits (which are lowered first, in the *outer* scope).
    fn named_let(&mut self, elems: &[Expr], span: Span) -> Result<Ir, CompileError> {
        let [
            _,
            Expr::Symbol(name, nspan),
            Expr::List(binding_forms, _),
            body @ ..,
        ] = elems
        else {
            return Err(CompileError::BadSyntax {
                form: "let",
                detail: "expected (let name ((var init)...) body...)".to_string(),
                span: ss(span),
            });
        };
        if body.is_empty() {
            return Err(CompileError::BadSyntax {
                form: "let",
                detail: "a named let needs a body".to_string(),
                span: ss(span),
            });
        }
        let pairs = self.binding_pairs(binding_forms, "let")?;
        let mut inits = Vec::with_capacity(pairs.len());
        let mut params = Vec::with_capacity(pairs.len());
        for (var, init) in pairs {
            inits.push(self.form(init, false)?);
            params.push(var);
        }
        let lambda = self.with_scope(vec![name.clone()], |me| {
            me.lambda_parts(Some(name.clone()), params, None, body, span)
        })?;
        Ok(Ir::LetRec {
            bindings: vec![(name.clone(), lambda)],
            body: vec![Ir::Call {
                head: Box::new(Ir::Var(name.clone(), *nspan)),
                args: inits,
                span,
            }],
            span,
        })
    }

    fn let_star(&mut self, elems: &[Expr], span: Span) -> Result<Ir, CompileError> {
        let [_, Expr::List(binding_forms, _), body @ ..] = elems else {
            return Err(CompileError::BadSyntax {
                form: "let*",
                detail: "expected (let* ((name init)...) body...)".to_string(),
                span: ss(span),
            });
        };
        if body.is_empty() {
            return Err(CompileError::BadSyntax {
                form: "let*",
                detail: "let* needs a body".to_string(),
                span: ss(span),
            });
        }
        // Unlike let, duplicates are legal here — each binding just shadows.
        let mut pairs = Vec::with_capacity(binding_forms.len());
        for f in binding_forms {
            let Expr::List(pair, pspan) = f else {
                return Err(CompileError::BadSyntax {
                    form: "let*",
                    detail: "each binding must be (name init)".to_string(),
                    span: ss(f.span()),
                });
            };
            let [Expr::Symbol(name, _), init] = pair.as_slice() else {
                return Err(CompileError::BadSyntax {
                    form: "let*",
                    detail: "each binding must be (name init)".to_string(),
                    span: ss(*pspan),
                });
            };
            pairs.push((name.clone(), init));
        }
        self.let_star_chain(&pairs, body, span)
    }

    fn let_star_chain(
        &mut self,
        pairs: &[(String, &Expr)],
        body: &[Expr],
        span: Span,
    ) -> Result<Ir, CompileError> {
        let Some(((name, init), rest)) = pairs.split_first() else {
            let body = self.body_ir(body, span)?;
            return Ok(Ir::Let {
                bindings: Vec::new(),
                body,
                span,
            });
        };
        let init = self.form(init, false)?;
        let inner =
            self.with_scope(vec![name.clone()], |me| me.let_star_chain(rest, body, span))?;
        Ok(Ir::Let {
            bindings: vec![(name.clone(), init)],
            body: vec![inner],
            span,
        })
    }

    fn letrec(&mut self, elems: &[Expr], span: Span) -> Result<Ir, CompileError> {
        let [_, Expr::List(binding_forms, _), body @ ..] = elems else {
            return Err(CompileError::BadSyntax {
                form: "letrec",
                detail: "expected (letrec ((name init)...) body...)".to_string(),
                span: ss(span),
            });
        };
        if body.is_empty() {
            return Err(CompileError::BadSyntax {
                form: "letrec",
                detail: "letrec needs a body".to_string(),
                span: ss(span),
            });
        }
        let pairs = self.binding_pairs(binding_forms, "letrec")?;
        let names: Vec<String> = pairs.iter().map(|(n, _)| n.clone()).collect();
        self.with_scope(names, |me| {
            let mut bindings = Vec::with_capacity(pairs.len());
            for (name, init) in pairs {
                let mut init = me.form(init, false)?;
                if let Ir::Lambda(l) = &mut init
                    && l.name.is_none()
                {
                    l.name = Some(name.clone());
                }
                bindings.push((name, init));
            }
            let body = me.body_ir(body, span)?;
            Ok(Ir::LetRec {
                bindings,
                body,
                span,
            })
        })
    }

    /// The M4 slice of `let-values`: each formals list must bind exactly one value —
    /// `((x) init)` binds it directly, `(x init)` binds the one-element list. Formals
    /// needing any other value count are M7's multiple values.
    fn let_values(&mut self, elems: &[Expr], span: Span, star: bool) -> Result<Ir, CompileError> {
        let form: &'static str = if star { "let*-values" } else { "let-values" };
        let [_, Expr::List(binding_forms, _), body @ ..] = elems else {
            return Err(CompileError::BadSyntax {
                form,
                detail: "expected (let-values ((formals init)...) body...)".to_string(),
                span: ss(span),
            });
        };
        if body.is_empty() {
            return Err(CompileError::BadSyntax {
                form,
                detail: "let-values needs a body".to_string(),
                span: ss(span),
            });
        }
        if star {
            return self.let_values_chain(binding_forms, body, span, form);
        }
        // Plain let-values: every init sees only the outer scope.
        let mut bindings = Vec::with_capacity(binding_forms.len());
        for f in binding_forms {
            bindings.push(self.one_value_binding(f, form)?);
        }
        let names = bindings.iter().map(|(n, _)| n.clone()).collect();
        let body = self.with_scope(names, |me| me.body_ir(body, span))?;
        Ok(Ir::Let {
            bindings,
            body,
            span,
        })
    }

    /// `let*-values` nesting: each binding's init is lowered with the previous names in
    /// scope, exactly like `let*`.
    fn let_values_chain(
        &mut self,
        binding_forms: &[Expr],
        body: &[Expr],
        span: Span,
        form: &'static str,
    ) -> Result<Ir, CompileError> {
        let Some((first, rest)) = binding_forms.split_first() else {
            let body = self.body_ir(body, span)?;
            return Ok(Ir::Let {
                bindings: Vec::new(),
                body,
                span,
            });
        };
        let (name, init) = self.one_value_binding(first, form)?;
        let inner = self.with_scope(vec![name.clone()], |me| {
            me.let_values_chain(rest, body, span, form)
        })?;
        Ok(Ir::Let {
            bindings: vec![(name, init)],
            body: vec![inner],
            span,
        })
    }

    /// One `(formals init)` binding of the let-values family, in the single-value slice.
    fn one_value_binding(
        &mut self,
        binding: &Expr,
        form: &'static str,
    ) -> Result<(String, Ir), CompileError> {
        let Expr::List(pair, pspan) = binding else {
            return Err(CompileError::BadSyntax {
                form,
                detail: "each binding must be (formals init)".to_string(),
                span: ss(binding.span()),
            });
        };
        let [formals, init] = pair.as_slice() else {
            return Err(CompileError::BadSyntax {
                form,
                detail: "each binding must be (formals init)".to_string(),
                span: ss(*pspan),
            });
        };
        match formals {
            Expr::List(names, _) => match names.as_slice() {
                [Expr::Symbol(name, _)] => Ok((name.clone(), self.form(init, false)?)),
                _ => Err(CompileError::Unsupported {
                    form: format!("{form} formals binding other than one value"),
                    milestone: "M7",
                    span: ss(*pspan),
                }),
            },
            // A lone rest formal receives the list of all (here: the one) values.
            Expr::Symbol(name, _) => {
                let arg = self.form(init, false)?;
                Ok((
                    name.clone(),
                    Ir::Call {
                        head: Box::new(Ir::Var("list".to_string(), *pspan)),
                        args: vec![arg],
                        span: *pspan,
                    },
                ))
            }
            _ => Err(CompileError::Unsupported {
                form: format!("{form} formals binding other than one value"),
                milestone: "M7",
                span: ss(*pspan),
            }),
        }
    }

    // ------------------------------------------------------------ conditionals

    /// `(and ...)`: `#f` is the only false value, so the R7RS desugaring needs no
    /// temporary — a failing test's value *is* `#f`.
    fn and_chain(&mut self, arms: &[Expr], span: Span) -> Result<Ir, CompileError> {
        match arms {
            [] => Ok(Ir::Datum(Expr::Boolean(true, span))),
            [e] => self.form(e, false),
            [e, rest @ ..] => Ok(Ir::If {
                cond: Box::new(self.form(e, false)?),
                then: Box::new(self.and_chain(rest, span)?),
                els: Some(Box::new(Ir::Datum(Expr::Boolean(false, span)))),
                span,
            }),
        }
    }

    /// `(or ...)`: a passing test's value is returned, so each non-final arm binds a
    /// compiler temporary (unreadable name — no capture possible).
    fn or_chain(&mut self, arms: &[Expr], span: Span) -> Result<Ir, CompileError> {
        match arms {
            [] => Ok(Ir::Datum(Expr::Boolean(false, span))),
            [e] => self.form(e, false),
            [e, rest @ ..] => {
                let t = self.gensym("or");
                let init = self.form(e, false)?;
                let rest = self.or_chain(rest, span)?;
                Ok(Ir::Let {
                    bindings: vec![(t.clone(), init)],
                    body: vec![Ir::If {
                        cond: Box::new(Ir::Var(t.clone(), span)),
                        then: Box::new(Ir::Var(t, span)),
                        els: Some(Box::new(rest)),
                        span,
                    }],
                    span,
                })
            }
        }
    }

    fn when_unless(&mut self, elems: &[Expr], span: Span, when: bool) -> Result<Ir, CompileError> {
        let [_, test, seq @ ..] = elems else {
            return Err(CompileError::BadSyntax {
                form: if when { "when" } else { "unless" },
                detail: "expected a test and at least one expression".to_string(),
                span: ss(span),
            });
        };
        if seq.is_empty() {
            return Err(CompileError::BadSyntax {
                form: if when { "when" } else { "unless" },
                detail: "expected a test and at least one expression".to_string(),
                span: ss(span),
            });
        }
        let cond = Box::new(self.form(test, false)?);
        let body = Ir::Begin {
            body: self.sequence(seq)?,
            span,
        };
        Ok(if when {
            Ir::If {
                cond,
                then: Box::new(body),
                els: None,
                span,
            }
        } else {
            // The empty Begin is the unspecified value.
            Ir::If {
                cond,
                then: Box::new(Ir::Begin {
                    body: Vec::new(),
                    span,
                }),
                els: Some(Box::new(body)),
                span,
            }
        })
    }

    fn cond(&mut self, elems: &[Expr], span: Span) -> Result<Ir, CompileError> {
        if elems.len() < 2 {
            return Err(CompileError::BadSyntax {
                form: "cond",
                detail: "cond needs at least one clause".to_string(),
                span: ss(span),
            });
        }
        self.cond_clauses(&elems[1..], span)
    }

    fn cond_clauses(&mut self, clauses: &[Expr], span: Span) -> Result<Ir, CompileError> {
        let Some((clause, rest)) = clauses.split_first() else {
            // No clause matched: unspecified.
            return Ok(Ir::Begin {
                body: Vec::new(),
                span,
            });
        };
        let Expr::List(parts, cspan) = clause else {
            return Err(CompileError::BadSyntax {
                form: "cond",
                detail: "each clause must be (test expr...)".to_string(),
                span: ss(clause.span()),
            });
        };
        if let Some(head) = parts.first()
            && self.is_keyword(head, "else")
        {
            if !rest.is_empty() {
                return Err(CompileError::BadSyntax {
                    form: "cond",
                    detail: "else must be the last clause".to_string(),
                    span: ss(*cspan),
                });
            }
            if parts.len() < 2 {
                return Err(CompileError::BadSyntax {
                    form: "cond",
                    detail: "an else clause needs at least one expression".to_string(),
                    span: ss(*cspan),
                });
            }
            return Ok(Ir::Begin {
                body: self.sequence(&parts[1..])?,
                span: *cspan,
            });
        }
        match parts.as_slice() {
            [] => Err(CompileError::BadSyntax {
                form: "cond",
                detail: "a clause may not be empty".to_string(),
                span: ss(*cspan),
            }),
            // (test): the test's own value when true.
            [test] => {
                let t = self.gensym("cond");
                let test = self.form(test, false)?;
                let rest = self.cond_clauses(rest, span)?;
                Ok(Ir::Let {
                    bindings: vec![(t.clone(), test)],
                    body: vec![Ir::If {
                        cond: Box::new(Ir::Var(t.clone(), *cspan)),
                        then: Box::new(Ir::Var(t, *cspan)),
                        els: Some(Box::new(rest)),
                        span: *cspan,
                    }],
                    span: *cspan,
                })
            }
            [test, arrow, recv] if self.is_keyword(arrow, "=>") => {
                let t = self.gensym("cond");
                let test = self.form(test, false)?;
                let recv = self.form(recv, false)?;
                let rest = self.cond_clauses(rest, span)?;
                Ok(Ir::Let {
                    bindings: vec![(t.clone(), test)],
                    body: vec![Ir::If {
                        cond: Box::new(Ir::Var(t.clone(), *cspan)),
                        then: Box::new(Ir::Call {
                            head: Box::new(recv),
                            args: vec![Ir::Var(t, *cspan)],
                            span: *cspan,
                        }),
                        els: Some(Box::new(rest)),
                        span: *cspan,
                    }],
                    span: *cspan,
                })
            }
            [test, seq @ ..] => Ok(Ir::If {
                cond: Box::new(self.form(test, false)?),
                then: Box::new(Ir::Begin {
                    body: self.sequence(seq)?,
                    span: *cspan,
                }),
                els: Some(Box::new(self.cond_clauses(rest, span)?)),
                span: *cspan,
            }),
        }
    }

    fn case(&mut self, elems: &[Expr], span: Span) -> Result<Ir, CompileError> {
        if elems.len() < 3 {
            return Err(CompileError::BadSyntax {
                form: "case",
                detail: "expected (case key clause...)".to_string(),
                span: ss(span),
            });
        }
        let key = self.form(&elems[1], false)?;
        let k = self.gensym("case");
        let dispatch = self.case_clauses(&k, &elems[2..], span)?;
        Ok(Ir::Let {
            bindings: vec![(k, key)],
            body: vec![dispatch],
            span,
        })
    }

    fn case_clauses(&mut self, k: &str, clauses: &[Expr], span: Span) -> Result<Ir, CompileError> {
        let Some((clause, rest)) = clauses.split_first() else {
            return Ok(Ir::Begin {
                body: Vec::new(),
                span,
            });
        };
        let Expr::List(parts, cspan) = clause else {
            return Err(CompileError::BadSyntax {
                form: "case",
                detail: "each clause must be ((datum...) expr...)".to_string(),
                span: ss(clause.span()),
            });
        };
        if let Some(head) = parts.first()
            && self.is_keyword(head, "else")
        {
            if !rest.is_empty() {
                return Err(CompileError::BadSyntax {
                    form: "case",
                    detail: "else must be the last clause".to_string(),
                    span: ss(*cspan),
                });
            }
            return self.case_result(k, &parts[1..], *cspan);
        }
        let Some(Expr::List(datums, _)) = parts.first() else {
            return Err(CompileError::BadSyntax {
                form: "case",
                detail: "each clause must be ((datum...) expr...)".to_string(),
                span: ss(*cspan),
            });
        };
        let matched = self.case_result(k, &parts[1..], *cspan)?;
        let unmatched = self.case_clauses(k, rest, span)?;
        // (eqv? k 'd1) or (eqv? k 'd2) or ... — each test is a boolean, so the or-chain
        // needs no temporaries.
        let mut test = Ir::Datum(Expr::Boolean(false, *cspan));
        for datum in datums.iter().rev() {
            test = Ir::If {
                cond: Box::new(Ir::Call {
                    head: Box::new(Ir::Var("eqv?".to_string(), *cspan)),
                    args: vec![Ir::Var(k.to_string(), *cspan), Ir::Datum(datum.clone())],
                    span: *cspan,
                }),
                then: Box::new(Ir::Datum(Expr::Boolean(true, *cspan))),
                els: Some(Box::new(test)),
                span: *cspan,
            };
        }
        Ok(Ir::If {
            cond: Box::new(test),
            then: Box::new(matched),
            els: Some(Box::new(unmatched)),
            span: *cspan,
        })
    }

    /// A case clause's right-hand side: a sequence, or `=> receiver` applied to the key.
    fn case_result(&mut self, k: &str, parts: &[Expr], span: Span) -> Result<Ir, CompileError> {
        match parts {
            [arrow, recv] if self.is_keyword(arrow, "=>") => Ok(Ir::Call {
                head: Box::new(self.form(recv, false)?),
                args: vec![Ir::Var(k.to_string(), span)],
                span,
            }),
            [] => Err(CompileError::BadSyntax {
                form: "case",
                detail: "a clause needs at least one result expression".to_string(),
                span: ss(span),
            }),
            seq => Ok(Ir::Begin {
                body: self.sequence(seq)?,
                span,
            }),
        }
    }

    // ------------------------------------------------------------ do

    /// `do` is its R7RS §7.3 desugaring: a named-let-shaped loop whose recursive call
    /// passes each variable's step (or the variable itself when no step is given).
    fn do_form(&mut self, elems: &[Expr], span: Span) -> Result<Ir, CompileError> {
        let [
            _,
            Expr::List(specs, _),
            Expr::List(test_clause, tspan),
            commands @ ..,
        ] = elems
        else {
            return Err(CompileError::BadSyntax {
                form: "do",
                detail: "expected (do ((var init step)...) (test expr...) command...)".to_string(),
                span: ss(span),
            });
        };
        let [test, results @ ..] = test_clause.as_slice() else {
            return Err(CompileError::BadSyntax {
                form: "do",
                detail: "the test clause may not be empty".to_string(),
                span: ss(*tspan),
            });
        };
        let mut vars: Vec<(String, Span)> = Vec::with_capacity(specs.len());
        let mut init_exprs: Vec<&Expr> = Vec::with_capacity(specs.len());
        let mut step_exprs: Vec<Option<&Expr>> = Vec::with_capacity(specs.len());
        for spec in specs {
            let Expr::List(parts, pspan) = spec else {
                return Err(CompileError::BadSyntax {
                    form: "do",
                    detail: "each variable spec must be (var init) or (var init step)".to_string(),
                    span: ss(spec.span()),
                });
            };
            let (name, nspan, init, step) = match parts.as_slice() {
                [Expr::Symbol(n, ns), init] => (n, ns, init, None),
                [Expr::Symbol(n, ns), init, step] => (n, ns, init, Some(step)),
                _ => {
                    return Err(CompileError::BadSyntax {
                        form: "do",
                        detail: "each variable spec must be (var init) or (var init step)"
                            .to_string(),
                        span: ss(*pspan),
                    });
                }
            };
            if vars.iter().any(|(n, _)| n == name) {
                return Err(CompileError::BadSyntax {
                    form: "do",
                    detail: format!("duplicate variable `{name}`"),
                    span: ss(*pspan),
                });
            }
            vars.push((name.clone(), *nspan));
            init_exprs.push(init);
            step_exprs.push(step);
        }

        let inits = init_exprs
            .iter()
            .map(|e| self.form(e, false))
            .collect::<Result<Vec<_>, _>>()?;
        let loop_name = self.gensym("do");
        let params: Vec<String> = vars.iter().map(|(n, _)| n.clone()).collect();

        let lambda = self.with_scope(vec![loop_name.clone()], |me| {
            me.with_scope(params.clone(), |me| {
                let cond = Box::new(me.form(test, false)?);
                let result = Ir::Begin {
                    body: me.sequence(results)?,
                    span: *tspan,
                };
                let mut els_body = me.sequence(commands)?;
                let steps = vars
                    .iter()
                    .zip(&step_exprs)
                    .map(|((name, nspan), step)| match step {
                        Some(step) => me.form(step, false),
                        None => Ok(Ir::Var(name.clone(), *nspan)),
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                els_body.push(Ir::Call {
                    head: Box::new(Ir::Var(loop_name.clone(), span)),
                    args: steps,
                    span,
                });
                Ok(Ir::Lambda(IrLambda {
                    name: Some("do-loop".to_string()),
                    params: params.clone(),
                    rest: None,
                    body: vec![Ir::If {
                        cond,
                        then: Box::new(result),
                        els: Some(Box::new(Ir::Begin {
                            body: els_body,
                            span,
                        })),
                        span,
                    }],
                    span,
                }))
            })
        })?;

        Ok(Ir::LetRec {
            bindings: vec![(loop_name.clone(), lambda)],
            body: vec![Ir::Call {
                head: Box::new(Ir::Var(loop_name, span)),
                args: inits,
                span,
            }],
            span,
        })
    }

    // ------------------------------------------------------------ quasiquote

    fn quasiquote_form(&mut self, elems: &[Expr], span: Span) -> Result<Ir, CompileError> {
        match elems {
            [_, datum] => self.quasi(datum, 1),
            _ => Err(CompileError::BadSyntax {
                form: "quasiquote",
                detail: format!("expected exactly one datum, got {}", elems.len() - 1),
                span: ss(span),
            }),
        }
    }

    /// Expand one quasiquoted datum at nesting `depth` (the outermost quasiquote is
    /// depth 1). A subtree with no unquotes at all is a plain constant; everything else
    /// builds its structure with `cons`/`append`/`list` calls, R7RS §4.2.8's semantics
    /// with proper depth tracking: an inner `quasiquote` raises the depth, `unquote`
    /// lowers it, and only depth-1 unquotes evaluate.
    fn quasi(&mut self, expr: &Expr, depth: usize) -> Result<Ir, CompileError> {
        if !contains_unquote(expr) {
            return Ok(Ir::Datum(expr.clone()));
        }
        match expr {
            Expr::Unquote(inner, span) => {
                if depth == 1 {
                    self.form(inner, false)
                } else {
                    self.qq_wrap("unquote", inner, depth - 1, *span)
                }
            }
            Expr::UnquoteSplicing(_, span) => Err(CompileError::BadSyntax {
                form: "unquote-splicing",
                detail: "`,@` is only meaningful inside a quasiquoted list".to_string(),
                span: ss(*span),
            }),
            Expr::Quasiquote(inner, span) => self.qq_wrap("quasiquote", inner, depth + 1, *span),
            // Quasiquote pays no attention to quote: `'(,x) is `(quote (,x)).
            Expr::Quote(inner, span) => self.qq_wrap("quote", inner, depth, *span),
            Expr::List(elems, span) => {
                if let [head, payload] = elems.as_slice()
                    && let Expr::Symbol(s, _) = head
                {
                    // The longhand spellings count as the sugar (R7RS §4.2.8).
                    match s.as_str() {
                        "unquote" => {
                            return if depth == 1 {
                                self.form(payload, false)
                            } else {
                                self.qq_wrap("unquote", payload, depth - 1, *span)
                            };
                        }
                        "quasiquote" => {
                            return self.qq_wrap("quasiquote", payload, depth + 1, *span);
                        }
                        _ => {}
                    }
                }
                self.qq_list(elems, None, depth, *span)
            }
            Expr::DottedList(elems, tail, span) => self.qq_list(elems, Some(tail), depth, *span),
            atom => Ok(Ir::Datum(atom.clone())),
        }
    }

    /// Rebuild `(sym payload)` as data: `(list 'sym <expansion of payload at depth>)`.
    fn qq_wrap(
        &mut self,
        sym: &str,
        payload: &Expr,
        depth: usize,
        span: Span,
    ) -> Result<Ir, CompileError> {
        let inner = self.quasi(payload, depth)?;
        Ok(Ir::Call {
            head: Box::new(Ir::Var("list".to_string(), span)),
            args: vec![Ir::Datum(Expr::Symbol(sym.to_string(), span)), inner],
            span,
        })
    }

    /// A quasiquoted (possibly dotted) list, folded right: ordinary elements `cons`
    /// onto the accumulator, depth-1 splices `append` before it.
    fn qq_list(
        &mut self,
        elems: &[Expr],
        tail: Option<&Expr>,
        depth: usize,
        span: Span,
    ) -> Result<Ir, CompileError> {
        let mut acc = match tail {
            Some(tail) => self.quasi(tail, depth)?,
            None => Ir::Datum(Expr::List(Vec::new(), span)),
        };
        for elem in elems.iter().rev() {
            let splice = match elem {
                Expr::UnquoteSplicing(payload, _) => Some(payload.as_ref()),
                Expr::List(sub, _) => match sub.as_slice() {
                    [Expr::Symbol(s, _), payload] if s == "unquote-splicing" => Some(payload),
                    _ => None,
                },
                _ => None,
            };
            acc = match splice {
                Some(payload) if depth == 1 => Ir::Call {
                    head: Box::new(Ir::Var("append".to_string(), span)),
                    args: vec![self.form(payload, false)?, acc],
                    span,
                },
                Some(payload) => Ir::Call {
                    head: Box::new(Ir::Var("cons".to_string(), span)),
                    args: vec![
                        self.qq_wrap("unquote-splicing", payload, depth - 1, span)?,
                        acc,
                    ],
                    span,
                },
                None => Ir::Call {
                    head: Box::new(Ir::Var("cons".to_string(), span)),
                    args: vec![self.quasi(elem, depth)?, acc],
                    span,
                },
            };
        }
        Ok(acc)
    }
}

/// Whether any unquote — sugar or longhand — appears anywhere in `e`. Conservative on
/// purpose: a hit only routes the subtree through the building path, which handles
/// depth correctly; a miss makes it a constant.
fn contains_unquote(e: &Expr) -> bool {
    match e {
        Expr::Unquote(..) | Expr::UnquoteSplicing(..) => true,
        Expr::Quote(inner, _) | Expr::Quasiquote(inner, _) => contains_unquote(inner),
        Expr::List(elems, _) => {
            if let Some(Expr::Symbol(s, _)) = elems.first()
                && (s == "unquote" || s == "unquote-splicing")
            {
                return true;
            }
            elems.iter().any(contains_unquote)
        }
        Expr::DottedList(elems, tail, _) => {
            elems.iter().any(contains_unquote) || contains_unquote(tail)
        }
        _ => false,
    }
}

/// A `(define (name . params) ...)` header: the name, then the fixed parameters.
fn split_proc_header(header: &[Expr], span: Span) -> Result<(String, Vec<String>), CompileError> {
    let Some(Expr::Symbol(name, _)) = header.first() else {
        return Err(CompileError::BadSyntax {
            form: "define",
            detail: "the defined procedure needs a symbol name".to_string(),
            span: ss(span),
        });
    };
    Ok((name.clone(), symbol_names(&header[1..], "define", span)?))
}

fn symbol_names(
    exprs: &[Expr],
    form: &'static str,
    span: Span,
) -> Result<Vec<String>, CompileError> {
    exprs
        .iter()
        .map(|e| match e {
            Expr::Symbol(s, _) => Ok(s.clone()),
            other => Err(CompileError::BadSyntax {
                form,
                detail: format!("parameters must be symbols, got `{other}`"),
                span: ss(span),
            }),
        })
        .collect()
}
