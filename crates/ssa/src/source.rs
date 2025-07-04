use std::fmt;

use crate::reference::{Ref, Reference};
use crate::ty::ConstTy;
use crate::{Constant, Id, Type};

/// Like source, but for specific types.
///
/// Such as `Int` or `bool`.
#[derive(PartialEq, Eq, Hash)]
pub enum Source<T: ConstTy> {
    /// A constant of type `T`
    Const(T::Const),
    /// A reference to a value of type `T`
    Ref(Id),
}

impl<T: ConstTy> From<Ref<T>> for Source<T> {
    fn from(r: Ref<T>) -> Self {
        Self::Ref(r.id)
    }
}

impl<T: ConstTy> Copy for Source<T> where T::Const: Copy {}

impl<T: ConstTy> Clone for Source<T>
where
    T::Const: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Const(arg0) => Self::Const(arg0.clone()),
            Self::Ref(arg0) => Self::Ref(*arg0),
        }
    }
}

impl<T: ConstTy> Source<T> {
    #[must_use]
    pub fn constant(self) -> Option<T::Const> {
        match self {
            Self::Const(v) => Some(v),
            Self::Ref(_) => None,
        }
    }

    #[must_use]
    pub fn reference(self) -> Option<Ref<T>> {
        match self {
            Self::Ref(it) => Some(Ref::new(it)),
            Self::Const(_) => None,
        }
    }
}

impl<T: ConstTy> Source<T>
where
    T::Const: Into<Constant>,
{
    pub fn upcast(self) -> AnySource {
        match self {
            Self::Const(konst) => AnySource::Const(konst.into()),
            Self::Ref(id) => AnySource::Ref(Reference { ty: T::TY, id }),
        }
    }
}

impl<T: ConstTy> fmt::Display for Source<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Const(v) => T::fmt_const(v, f),
            Self::Ref(r) => r.fmt(f),
        }
    }
}

impl<T: ConstTy> fmt::Debug for Source<T>
where
    T::Const: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Const(arg0) => f.debug_tuple("Const").field(arg0).finish(),
            Self::Ref(arg0) => f.debug_tuple("Ref").field(arg0).finish(),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone, Hash)]
pub enum AnySource {
    Const(Constant),
    Ref(Reference),
}

impl AnySource {
    #[must_use]
    pub fn constant(self) -> Option<Constant> {
        match self {
            Self::Const(v) => Some(v),
            Self::Ref(_) => None,
        }
    }

    #[must_use]
    pub fn reference(self) -> Option<Reference> {
        match self {
            Self::Ref(r) => Some(r),
            Self::Const(_) => None,
        }
    }

    #[must_use]
    pub fn ty(self) -> Type {
        match self {
            AnySource::Const(konst) => konst.ty(),
            AnySource::Ref(r) => r.ty,
        }
    }

    #[must_use]
    pub fn downcast<T: ConstTy>(self) -> Option<Source<T>> {
        match self {
            AnySource::Const(c) => T::downcast(c).map(Source::Const),
            AnySource::Ref(r) => (r.ty == T::TY).then_some(Source::Ref(r.id)),
        }
    }
}

impl fmt::Display for AnySource {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Const(v) => v.fmt(f),
            Self::Ref(r) => r.fmt(f),
        }
    }
}

/// A pair of constants/references that specifically *can't* be `const,const`
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum SourcePair {
    RefRef(Reference, Reference),
    RefConst(Reference, Constant),
    ConstRef(Constant, Reference),
}

impl SourcePair {
    #[must_use]
    pub fn lhs(self) -> AnySource {
        match self {
            Self::RefRef(lhs, _) | Self::RefConst(lhs, _) => AnySource::Ref(lhs),
            Self::ConstRef(lhs, _) => AnySource::Const(lhs),
        }
    }

    #[must_use]
    pub fn rhs(self) -> AnySource {
        match self {
            Self::RefRef(_, rhs) | Self::ConstRef(_, rhs) => AnySource::Ref(rhs),
            Self::RefConst(_, rhs) => AnySource::Const(rhs),
        }
    }
}

impl TryFrom<(AnySource, AnySource)> for SourcePair {
    type Error = (Constant, Constant);

    fn try_from(value: (AnySource, AnySource)) -> Result<Self, Self::Error> {
        match value {
            (AnySource::Const(lhs), AnySource::Const(rhs)) => Err((lhs, rhs)),
            (AnySource::Const(lhs), AnySource::Ref(rhs)) => Ok(Self::ConstRef(lhs, rhs)),
            (AnySource::Ref(lhs), AnySource::Const(rhs)) => Ok(Self::RefConst(lhs, rhs)),
            (AnySource::Ref(lhs), AnySource::Ref(rhs)) => Ok(Self::RefRef(lhs, rhs)),
        }
    }
}
