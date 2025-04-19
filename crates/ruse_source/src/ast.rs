use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug)]
pub struct Expr<'e> {
    kind: ExprKind,
    span: LocatedSpan<&'e str>,
}

#[derive(Debug)]
pub enum ExprKind {
    Boolean(bool),
}
