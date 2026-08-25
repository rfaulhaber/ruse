//! Lowering: the reader's [`Expr`] tree into the typed Core IR.
//!
//! This pass owns *syntax*: special-form recognition, form arity, and the R7RS rule that
//! a lexical binding shadows a syntactic keyword — `(let ((if list)) (if 1 2 3))` calls
//! the variable `if`. To honour that, lowering carries a stack of bound names even though
//! register assignment belongs to the emitter.
//!
//! The M3 special forms are `quote`, `if`, `define` (top level only), `set!`, `lambda`
//! (fixed arity), `begin` and `let`. Every other R7RS keyword is *recognized* and refused
//! with the milestone that will implement it, which reads far better than the unbound
//! -variable error the same head would otherwise produce at runtime.

use crate::ast::Expr;
use crate::span::Span;

use super::{CompileError, ss};

/// The typed Core IR (decision C). Deliberately small: derived forms stay unsupported
/// until M4 rather than being desugared ad hoc here.
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
    /// Top-level definition. Lowering guarantees this never appears inside a body.
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
}

pub(crate) struct IrLambda {
    /// The defined name, when `define` sugar supplied one; prototypes carry it for
    /// listings and arity errors.
    pub(crate) name: Option<String>,
    pub(crate) params: Vec<String>,
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
            | Ir::Let { span, .. } => *span,
            Ir::Lambda(l) => l.span,
        }
    }
}

/// Keywords the compiler knows it does not compile yet, and when it will.
fn unsupported_milestone(name: &str) -> Option<&'static str> {
    Some(match name {
        "and" | "or" | "when" | "unless" | "cond" | "case" | "do" | "let*" | "letrec"
        | "letrec*" | "quasiquote" | "let-values" | "let*-values" | "define-values" => "M4",
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

#[derive(Default)]
struct Lowerer {
    /// Names bound by enclosing `lambda`/`let` forms, innermost scope last. Only
    /// membership matters here; the emitter assigns registers.
    scopes: Vec<Vec<String>>,
}

impl Lowerer {
    fn is_bound(&self, name: &str) -> bool {
        self.scopes
            .iter()
            .any(|scope| scope.iter().any(|n| n == name))
    }

    /// Lower `expr`. `top` is true only at the top level and inside a top-level `begin`,
    /// the two places R7RS lets `define` appear until M4's internal definitions.
    fn form(&mut self, expr: &Expr, top: bool) -> Result<Ir, CompileError> {
        match expr {
            Expr::Integer(..)
            | Expr::Number(..)
            | Expr::String(..)
            | Expr::Character(..)
            | Expr::Boolean(..) => Ok(Ir::Datum(expr.clone())),
            Expr::Symbol(name, span) => Ok(Ir::Var(name.clone(), *span)),
            Expr::Quote(inner, _) => Ok(Ir::Datum((**inner).clone())),
            Expr::Quasiquote(_, span) => Err(CompileError::Unsupported {
                form: "quasiquote".to_string(),
                milestone: "M4",
                span: ss(*span),
            }),
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
                "set!" => return self.set(elems, span),
                "lambda" => return self.lambda(elems, span, None),
                "begin" => return self.begin(elems, span, top),
                "let" => return self.let_form(elems, span),
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

    fn define(&mut self, elems: &[Expr], span: Span, top: bool) -> Result<Ir, CompileError> {
        if !top {
            return Err(CompileError::Unsupported {
                form: "internal define".to_string(),
                milestone: "M4",
                span: ss(span),
            });
        }
        match elems {
            // (define name value)
            [_, Expr::Symbol(name, _), value] => {
                let mut value = self.form(value, false)?;
                // A lambda defined this way inherits the definition's name, so listings
                // and arity errors can say `fact` instead of `#<procedure>`.
                if let Ir::Lambda(l) = &mut value
                    && l.name.is_none()
                {
                    l.name = Some(name.clone());
                }
                Ok(Ir::Define {
                    name: name.clone(),
                    value: Box::new(value),
                    span,
                })
            }
            // (define (name params...) body...)
            [_, Expr::List(header, hspan), ..] if elems.len() >= 3 => {
                let Some(Expr::Symbol(name, _)) = header.first() else {
                    return Err(CompileError::BadSyntax {
                        form: "define",
                        detail: "the defined procedure needs a symbol name".to_string(),
                        span: ss(*hspan),
                    });
                };
                let params = symbol_names(&header[1..], "define", *hspan)?;
                let value = self.lambda_parts(Some(name.clone()), params, &elems[2..], span)?;
                Ok(Ir::Define {
                    name: name.clone(),
                    value: Box::new(value),
                    span,
                })
            }
            [_, Expr::DottedList(_, _, dspan), ..] => Err(CompileError::Unsupported {
                form: "variadic parameters".to_string(),
                milestone: "M4",
                span: ss(*dspan),
            }),
            _ => Err(CompileError::BadSyntax {
                form: "define",
                detail: "expected (define name value) or (define (name params...) body...)"
                    .to_string(),
                span: ss(span),
            }),
        }
    }

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
                self.lambda_parts(name, params, &elems[2..], span)
            }
            [_, Expr::Symbol(_, pspan), ..] | [_, Expr::DottedList(_, _, pspan), ..] => {
                Err(CompileError::Unsupported {
                    form: "variadic parameters".to_string(),
                    milestone: "M4",
                    span: ss(*pspan),
                })
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
        body: &[Expr],
        span: Span,
    ) -> Result<Ir, CompileError> {
        for (i, p) in params.iter().enumerate() {
            if params[..i].contains(p) {
                return Err(CompileError::BadSyntax {
                    form: "lambda",
                    detail: format!("duplicate parameter `{p}`"),
                    span: ss(span),
                });
            }
        }
        self.scopes.push(params.clone());
        let body = body
            .iter()
            .map(|e| self.form(e, false))
            .collect::<Result<Vec<_>, _>>();
        self.scopes.pop();
        Ok(Ir::Lambda(IrLambda {
            name,
            params,
            body: body?,
            span,
        }))
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

    fn let_form(&mut self, elems: &[Expr], span: Span) -> Result<Ir, CompileError> {
        match elems {
            [_, Expr::Symbol(_, nspan), ..] => Err(CompileError::Unsupported {
                form: "named let".to_string(),
                milestone: "M4",
                span: ss(*nspan),
            }),
            [_, Expr::List(binding_forms, bspan), ..] if elems.len() >= 3 => {
                let mut bindings = Vec::with_capacity(binding_forms.len());
                for form in binding_forms {
                    let Expr::List(pair, pspan) = form else {
                        return Err(CompileError::BadSyntax {
                            form: "let",
                            detail: "each binding must be (name init)".to_string(),
                            span: ss(form.span()),
                        });
                    };
                    let [Expr::Symbol(name, _), init] = pair.as_slice() else {
                        return Err(CompileError::BadSyntax {
                            form: "let",
                            detail: "each binding must be (name init)".to_string(),
                            span: ss(*pspan),
                        });
                    };
                    if bindings.iter().any(|(n, _)| n == name) {
                        return Err(CompileError::BadSyntax {
                            form: "let",
                            detail: format!("duplicate binding `{name}`"),
                            span: ss(*pspan),
                        });
                    }
                    // Plain let: inits see only the outer scope.
                    bindings.push((name.clone(), self.form(init, false)?));
                }
                self.scopes
                    .push(bindings.iter().map(|(n, _)| n.clone()).collect());
                let body = elems[2..]
                    .iter()
                    .map(|e| self.form(e, false))
                    .collect::<Result<Vec<_>, _>>();
                self.scopes.pop();
                Ok(Ir::Let {
                    bindings,
                    body: body?,
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
