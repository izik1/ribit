use rasen::params::Register;
use rasen::params::mem::Mem;
use ribit_core::register;
use ribit_ssa::StackIndex;

pub fn src_rv_reg(base: crate::Source, reg: register::RiscV) -> Mem {
    let offset = (reg.get() - 1) * 4;

    match base {
        crate::Source::Val(base) => Mem::displacement((base + u32::from(offset)).cast_signed()),
        crate::Source::Register(base) => Mem::base_displacement(base, i32::from(offset)),
    }
}

pub fn src_src(src1: crate::Source, src2: crate::Source) -> Mem {
    match (src1, src2) {
        (crate::Source::Val(v1), crate::Source::Val(v2)) => {
            Mem::displacement((v1 + v2).cast_signed())
        }

        (crate::Source::Register(base), crate::Source::Val(displacement))
        | (crate::Source::Val(displacement), crate::Source::Register(base)) => {
            Mem::base_displacement(base, displacement as i32)
        }

        (crate::Source::Register(base), crate::Source::Register(index)) => {
            Mem::base_index(base, index).unwrap()
        }
    }
}

pub fn stack(idx: StackIndex, redzone: bool) -> Mem {
    Mem::base_displacement(Register::Zsp, idx.offset(redzone))
}
