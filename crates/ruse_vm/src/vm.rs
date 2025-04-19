use ruse_core::value::Value;

pub struct VM<'vm> {
    registers: &'vm [Value; 250],
}
