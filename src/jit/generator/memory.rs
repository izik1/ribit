use crate::{jit, register};

use rasen::params::mem::Mem;

pub fn src_rv_reg(base: jit::Source, reg: register::RiscV) -> Mem {
    match base {
        jit::Source::Val(base) => Mem::displacement((base + (reg.get() * 4) as u32) as i32),
        jit::Source::Register(base) => Mem::base_displacement(base, (reg.get() * 4) as i32),
    }
}

pub fn src_src(src1: jit::Source, src2: jit::Source) -> Mem {
    match (src1, src2) {
        (jit::Source::Val(v1), jit::Source::Val(v2)) => Mem::displacement((v1 + v2) as i32),

        (jit::Source::Register(base), jit::Source::Val(displacement))
        | (jit::Source::Val(displacement), jit::Source::Register(base)) => {
            Mem::base_displacement(base, displacement as i32)
        }

        (jit::Source::Register(base), jit::Source::Register(index)) => {
            Mem::base_index(base, index).unwrap()
        }
    }
}
