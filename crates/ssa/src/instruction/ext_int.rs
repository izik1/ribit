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

        if let Type::Int(ty) = self.src.ty {
            assert!(ty.to_width() >= self.width);
        }

        Type::Int(Bitness::from(self.width))
    }

    #[must_use]
    pub fn id(&self) -> Id {
        self.dest
    }
}

impl fmt::Display for ExtInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { dest, width, src, signed } = self;
        let instr = match signed {
            true => "sext",
            false => "zext",
        };

        write!(f, "{dest} = {instr} {width} {src}")
    }
}
