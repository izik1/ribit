use std::fmt;

use crate::{Id, Type};

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Reference {
    pub ty: Type,
    pub id: Id,
}

impl fmt::Display for Reference {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{} ", self.ty)?;
        }

        write!(f, "{}", self.id)
    }
}
