use std::fmt;

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
#[cfg_attr(test, derive(serde::Serialize))]
pub enum Type {
    // todo for later: be generic over bitness?
    Int32,
    /// Unit type, `()` in Rust, `void` in C
    Unit,
    // basically just an int1
    // Boolean,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int32 => f.write_str("i32"),
            Type::Unit => f.write_str("()"),
        }
    }
}
