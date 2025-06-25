use std::fmt;

use crate::{AnySource, Id, Ref, Type, ty};

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
        ty::assert_types_eq!(self.if_true.ty(), self.if_false.ty());

        self.if_true.ty()
    }
}
