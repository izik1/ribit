use std::fmt;

use ribit_core::Width;

use crate::reference::Reference;
use crate::ty::Bitness;
use crate::{Id, Type};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExtInt {
    pub dest: Id,
    pub width: Width,
    pub src: Reference,
    pub signed: bool,
}

impl ExtInt {
    #[must_use]
    pub fn ty(&self) -> Type {
        assert!(matches!(self.src.ty, Type::Int(_) | Type::Boolean));
        Type::Int(Bitness::from(self.width))
    }

    #[must_use]
    pub fn id(&self) -> Id {
        self.dest
    }
}

impl fmt::Display for ExtInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let instr = match self.signed {
            true => "sext",
            false => "zext",
        };
        write!(f, "{} = {} {} {}", self.dest, instr, self.width, self.src)
    }
}
