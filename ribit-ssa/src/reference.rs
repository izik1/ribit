use std::fmt;
use std::marker::PhantomData;

use crate::ty::ConstTy;
use crate::{Id, Type};

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
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

#[derive(Eq, PartialEq)]
pub struct TypedRef<T> {
    pub id: Id,
    _phantom: PhantomData<fn(T) -> Type>,
}

impl<T> Clone for TypedRef<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for TypedRef<T> {}

impl<T: ConstTy> TypedRef<T> {
    #[must_use]
    pub fn new(id: Id) -> Self {
        Self { id, _phantom: PhantomData }
    }
}

impl<T: ConstTy> fmt::Debug for TypedRef<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TypedRef").field("id", &self.id).field("ty", &T::TY).finish()
    }
}

impl<T: ConstTy> fmt::Display for TypedRef<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{} ", T::TY)?;
        }

        write!(f, "{}", self.id)
    }
}
