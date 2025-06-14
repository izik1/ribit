use std::fmt;
use std::hash::Hash;
use std::marker::PhantomData;

use crate::ty::ConstTy;
use crate::{Id, Type};

#[derive(PartialEq, Eq, Debug, Copy, Clone, Hash)]
pub struct Reference {
    pub ty: Type,
    pub id: Id,
}

impl Reference {
    #[must_use]
    pub const fn new(ty: Type, id: Id) -> Self {
        Self { ty, id }
    }
}

impl fmt::Display for Reference {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{} ", self.ty)?;
        }

        self.id.fmt(f)
    }
}

pub struct Ref<T> {
    pub id: Id,
    _phantom: PhantomData<fn(T) -> Type>,
}

impl<T> Eq for Ref<T> {}

impl<T> PartialEq for Ref<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<T> Hash for Ref<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<T> Clone for Ref<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Ref<T> {}

impl<T: ConstTy> Ref<T> {
    #[must_use]
    pub fn new(id: Id) -> Self {
        Self { id, _phantom: PhantomData }
    }

    #[must_use]
    pub const fn ty(self) -> Type {
        T::TY
    }

    #[must_use]
    pub const fn to_source(self) -> crate::Source<T> {
        crate::Source::Ref(self.id)
    }
}

impl<T: ConstTy> fmt::Debug for Ref<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TypedRef").field("id", &self.id).field("ty", &T::TY).finish()
    }
}

impl<T: ConstTy> fmt::Display for Ref<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{} ", T::TY)?;
        }

        self.id.fmt(f)
    }
}
