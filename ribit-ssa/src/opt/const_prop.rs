use std::collections::HashMap;

use crate::ty::{ConstTy, Constant};
use crate::{AnySource, Id, TypedRef};

mod basic;

pub use basic::run as run_basic;

#[must_use]
fn typed_const_ref_lookup<T: ConstTy>(
    consts: &HashMap<Id, Constant>,
    src: TypedRef<T>,
) -> Option<T::Const> {
    consts.get(&src.id).copied().and_then(T::downcast)
}

#[must_use]
fn lookup(consts: &HashMap<Id, Constant>, src: AnySource) -> AnySource {
    match src {
        AnySource::Const(c) => AnySource::Const(c),
        AnySource::Ref(it) => match consts.get(&it.id).copied() {
            Some(konst) => AnySource::Const(konst),
            None => AnySource::Ref(it),
        },
    }
}

// todo: `pub fn complex_const_prop(graph: &mut [Instruction])`
