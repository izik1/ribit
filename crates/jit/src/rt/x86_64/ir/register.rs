use super::Id;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RegisterTag {
    Integer,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct SymbolicRegister {
    pub tag: RegisterTag,
    pub id: Id,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Register {
    Symbol(SymbolicRegister),
    Register(crate::Register, bool),
}

impl Register {
    pub fn symbolic(id: Id) -> Self {
        Self::Symbol(SymbolicRegister { tag: RegisterTag::Integer, id })
    }
}
