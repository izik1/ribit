use std::collections::HashMap;

use crate::ty::{ConstTy, Constant};
use crate::{AnySource, Id, Source};

mod basic;

pub use basic::run as run_basic;

#[must_use]
fn lookup(consts: &HashMap<Id, Constant>, src: AnySource) -> AnySource {
    match src {
        src @ AnySource::Const(_) => src,
        AnySource::Ref(it) => match consts.get(&it.id).copied() {
            Some(konst) => AnySource::Const(konst),
            None => AnySource::Ref(it),
        },
    }
}

#[must_use]
fn typed_lookup<T: ConstTy>(consts: &HashMap<Id, Constant>, src: Source<T>) -> Source<T> {
    match src {
        src @ Source::Const(_) => src,
        Source::Ref(it) => match consts.get(&it).copied() {
            Some(c) => Source::Const(T::downcast(c).unwrap()),
            None => Source::Ref(it),
        },
    }
}

// todo: `pub fn complex_const_prop(graph: &mut [Instruction])`
