use std::fmt;

use crate::{ty, AnySource, Id, Ref, Type};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Select {
    pub dest: Id,
    pub cond: Ref<ty::Bool>,
    pub if_true: AnySource,
    pub if_false: AnySource,
}

impl fmt::Display for Select {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { dest, cond, if_true, if_false } = self;
        write!(f, "{dest} = select {cond}, {if_true}, {if_false}")
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
