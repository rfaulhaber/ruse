pub enum Value {
    Number(Number),
    Nil,
}

pub enum Number {
    Float(f64),
    Int(i64),
}
