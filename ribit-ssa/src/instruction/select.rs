use std::fmt;

use crate::ty::BoolTy;
use crate::{AnySource, Id, Type, TypedRef};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Select {
    pub dest: Id,
    pub cond: TypedRef<BoolTy>,
    pub if_true: AnySource,
    pub if_false: AnySource,
}

impl fmt::Display for Select {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = select {}, {}, {}", self.dest, self.cond, self.if_true, self.if_false)
    }
}

impl Select {
    #[must_use]
    pub fn ty(&self) -> Type {
        let ty = self.if_true.ty();

        assert_eq!(ty, self.if_false.ty());

        ty
    }

    #[must_use]
    pub fn id(&self) -> Id {
        self.dest
    }
}
