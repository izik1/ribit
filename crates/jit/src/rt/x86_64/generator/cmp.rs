use std::io;

use rasen::params::Register;
use rasen::params::imm::Imm32;
use rasen::params::reg::Reg32;
use ribit_ssa::{CmpKind, Inequality};

use super::BlockBuilder;
use crate::Source;

pub fn select(
    builder: &mut BlockBuilder<'_, '_>,
    dest: Register,
    cond: Register,
    if_true: Source,
    if_false: Source,
    clobber_reg: Option<Register>,
) -> io::Result<()> {
    // fixme: handle not having clobbers when we want them, it technically isn't a bug in this case.
    // First we need to compare  `cond` to 0
    // note: there are some optimizations that could be done by doing this lower, todo: do this later.
    builder.stream.test_reg_reg(Reg32(cond), Reg32(cond))?;

    let (mov_reg, equal) = match (if_true, if_false) {
        // clobber case
        (Source::Val(a), Source::Val(b)) => {
            // todo: optimize by checking for 0 and doing a xor if `dest != cond` (involves skipping the above `test` instr.)
            builder.stream.mov_reg_imm(dest, Imm32(b))?;

            let clobber_reg =
                clobber_reg.expect("todo: handle clobber not existing here (via branches)");
            builder.stream.mov_reg_imm(clobber_reg, Imm32(a))?;
            (clobber_reg, false)
        }

        // clobber case
        (Source::Register(r), Source::Val(v)) | (Source::Val(v), Source::Register(r))
            if r == dest =>
        {
            let clobber_reg =
                clobber_reg.expect("todo: handle clobber not existing here (via branches)");
            builder.stream.mov_reg_imm(clobber_reg, Imm32(v))?;
            (clobber_reg, if_true.is_val())
        }

        // we don't need to clobber here, since we can `mov dest, v` and then `cmov{n}e` the other one
        (Source::Register(r), Source::Val(v)) | (Source::Val(v), Source::Register(r)) => {
            // todo: optimize by checking for 0 and doing a xor if `dest != cond` (involves skipping the above `test` instr.)
            builder.stream.mov_reg_imm(dest, Imm32(v))?;
            (r, if_true.is_reg())
        }

        (Source::Register(if_true), Source::Register(if_false)) => {
            if dest == if_true {
                (if_false, false)
            } else {
                if dest != if_false {
                    builder.stream.mov_reg_reg(Reg32(dest), Reg32(if_false))?;
                }

                (if_true, true)
            }
        }
    };

    match equal {
        true => builder.stream.cmove_reg_reg(Reg32(dest), Reg32(mov_reg)),
        false => builder.stream.cmovne_reg_reg(Reg32(dest), Reg32(mov_reg)),
    }
}

fn bool_cmp(
    src1: Register,
    src2: Source,
    false_value: u32,
    true_value: u32,
    mode: CmpKind,
) -> CmpValue {
    match src2 {
        Source::Val(0) => match zero(false_value, true_value, mode) {
            Some(const_value) => CmpValue::Const(const_value),
            None => CmpValue::Test(src1),
        },
        Source::Val(val) => CmpValue::CmpImm(src1, val),

        Source::Register(src2) if src1 == src2 => {
            CmpValue::Const(same_val(false_value, true_value, mode))
        }

        Source::Register(src2) => CmpValue::CmpReg(src1, src2),
    }
}

pub fn same_val<T>(false_val: T, true_val: T, cmp_mode: CmpKind) -> T {
    match cmp_mode {
        CmpKind::Eq => true_val,
        CmpKind::Ne => false_val,
        CmpKind::Inequality(it) if it.include_eq() => true_val,
        CmpKind::Inequality(_) => false_val,
    }
}

// returns one of the input Ts if the branch becomes unconditional
pub fn zero<T>(false_val: T, true_val: T, cmp_mode: CmpKind) -> Option<T> {
    match cmp_mode {
        CmpKind::Inequality(Inequality::Uge) => Some(true_val),
        CmpKind::Inequality(Inequality::Ult) => Some(false_val),
        _ => None,
    }
}

#[derive(Copy, Clone)]
enum CmpValue {
    Const(u32),
    /// A register to `test` with itself, and if the result should be flipped.
    // Equivielent to `and register, 0`.
    /// Which means flip is equivilent to `and 0, register`- an invalid instruction,
    /// so the arguments get flipped. When the arguments *aren't* eq/ne,
    /// this means that the comparision gets flipped as well,
    /// so we need to invert the test at the end.
    /// However, eq/ne *don't* flip in that situation.
    Test(Register),
    CmpReg(Register, Register),
    /// A register to compare, an immediate to compare it to, and whether or not the result
    /// of this comparision should be flipped.
    CmpImm(Register, u32),
}

fn cmp_bit(builder: &mut BlockBuilder, dest: Register, kind: CmpKind) -> io::Result<()> {
    match kind {
        CmpKind::Eq => builder.stream.sete_reg8(dest),
        CmpKind::Ne => builder.stream.setne_reg8(dest),
        CmpKind::Inequality(kind) => match kind {
            Inequality::Ult => builder.stream.setb_reg8(dest),
            Inequality::Ule => builder.stream.setbe_reg8(dest),
            Inequality::Ugt => builder.stream.seta_reg8(dest),
            Inequality::Uge => builder.stream.setae_reg8(dest),
            Inequality::Slt => builder.stream.setl_reg8(dest),
            Inequality::Sle => builder.stream.setle_reg8(dest),
            Inequality::Sgt => builder.stream.setg_reg8(dest),
            Inequality::Sge => builder.stream.setge_reg8(dest),
        },
    }
}

pub fn set_bool_conditional(
    builder: &mut BlockBuilder,
    dest: Register,
    src1: Register,
    src2: Source,
    mode: CmpKind,
) -> io::Result<()> {
    let cmp = bool_cmp(src1, src2, 0, 1, mode);

    let xor = match cmp {
        CmpValue::Const(_) => false,
        CmpValue::Test(r) | CmpValue::CmpImm(r, _) => r != dest,
        CmpValue::CmpReg(r1, r2) => r1 != dest && r2 != dest,
    };

    if xor {
        builder.stream.xor_reg_reg(Reg32(dest), Reg32(dest))?;
    }

    match cmp {
        CmpValue::Const(v) => {
            builder.mov_r32_imm32(dest, v)?;
            return Ok(());
        }

        CmpValue::Test(r) => {
            builder.stream.test_reg_reg(Reg32(r), Reg32(r))?;
        }

        CmpValue::CmpImm(src, val) => {
            builder.stream.cmp_reg_imm(Reg32(src), Imm32(val))?;
        }

        CmpValue::CmpReg(src1, src2) => {
            builder.stream.cmp_reg_reg(Reg32(src1), Reg32(src2))?;
        }
    };

    if !xor {
        // todo: add clobbler to be more efficent (ability to xor)
        builder.stream.mov_reg_imm(Reg32(dest), Imm32(0))?;
    }

    cmp_bit(builder, dest, mode)
}
