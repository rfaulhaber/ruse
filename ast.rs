use std::fmt;
use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // Atoms
    Number(f64, Span),
    Integer(i64, Span),
    String(String, Span),
    Character(char, Span),
    Boolean(bool, Span),
    Symbol(String, Span),
    
    // Lists and pairs
    List(Vec<Expr>, Span),
    DottedList(Vec<Expr>, Box<Expr>, Span), // (a b c . d)
    Quote(Box<Expr>, Span),
    Quasiquote(Box<Expr>, Span),
    Unquote(Box<Expr>, Span),
    UnquoteSplicing(Box<Expr>, Span),
    
    // Special forms will be handled during evaluation
    // but we represent them as lists during parsing
}

impl Expr {
    pub fn symbol(s: &str, span: Span) -> Self {
        Expr::Symbol(s.to_string(), span)
    }
    
    pub fn list(exprs: Vec<Expr>, span: Span) -> Self {
        Expr::List(exprs, span)
    }
    
    pub fn span(&self) -> Span {
        match self {
            Expr::Number(_, span) => *span,
            Expr::Integer(_, span) => *span,
            Expr::String(_, span) => *span,
            Expr::Character(_, span) => *span,
            Expr::Boolean(_, span) => *span,
            Expr::Symbol(_, span) => *span,
            Expr::List(_, span) => *span,
            Expr::DottedList(_, _, span) => *span,
            Expr::Quote(_, span) => *span,
            Expr::Quasiquote(_, span) => *span,
            Expr::Unquote(_, span) => *span,
            Expr::UnquoteSplicing(_, span) => *span,
        }
    }
    
    pub fn is_atom(&self) -> bool {
        !matches!(self, Expr::List(_, _) | Expr::DottedList(_, _, _))
    }
    
    pub fn is_list(&self) -> bool {
        matches!(self, Expr::List(_, _))
    }
    
    pub fn is_symbol(&self) -> bool {
        matches!(self, Expr::Symbol(_, _))
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Number(n, _) => write!(f, "{}", n),
            Expr::Integer(i, _) => write!(f, "{}", i),
            Expr::String(s, _) => write!(f, "\"{}\"", s),
            Expr::Character(c, _) => {
                match *c {
                    ' ' => write!(f, "#\\space"),
                    '\n' => write!(f, "#\\newline"),
                    '\t' => write!(f, "#\\tab"),
                    _ => write!(f, "#\\{}", c),
                }
            },
            Expr::Boolean(b, _) => write!(f, "#{}", if *b { "t" } else { "f" }),
            Expr::Symbol(s, _) => write!(f, "{}", s),
            Expr::List(exprs, _) => {
                write!(f, "(")?;
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", expr)?;
                }
                write!(f, ")")
            },
            Expr::DottedList(exprs, tail, _) => {
                write!(f, "(")?;
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", expr)?;
                }
                if !exprs.is_empty() {
                    write!(f, " ")?;
                }
                write!(f, ". {})", tail)
            },
            Expr::Quote(expr, _) => write!(f, "'{}", expr),
            Expr::Quasiquote(expr, _) => write!(f, "`{}", expr),
            Expr::Unquote(expr, _) => write!(f, ",{}", expr),
            Expr::UnquoteSplicing(expr, _) => write!(f, ",@{}", expr),
        }
    }
}