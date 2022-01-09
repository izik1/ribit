use std::io;

use rasen::params::imm::Imm32;
use rasen::params::reg::Reg32;
use rasen::params::Register;
use ribit_ssa::{eval, CmpKind};

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
    src1: Source,
    src2: Source,
    false_value: u32,
    true_value: u32,
    mode: CmpKind,
) -> CmpValue {
    match (src1, src2) {
        (rs1, rs2) if rs1 == rs2 => CmpValue::Const(same_val(false_value, true_value, mode)),

        (Source::Val(a), Source::Val(b)) => {
            CmpValue::Const(if eval::cmp(a, b, mode) { true_value } else { false_value })
        }

        (Source::Register(src), Source::Val(0)) | (Source::Val(0), Source::Register(src)) => {
            // if `src` came from src2 we need to invert the values
            let (true_value, false_value) = match src1.is_reg() {
                true => (true_value, false_value),
                false => (false_value, true_value),
            };

            let inversable = matches!(mode, CmpKind::Eq | CmpKind::Ne);

            match zero(false_value, true_value, mode) {
                Some(const_value) => CmpValue::Const(const_value),
                None => CmpValue::Test(src, src2.is_reg() && !inversable),
            }
        }

        (Source::Register(src), Source::Val(val)) | (Source::Val(val), Source::Register(src)) => {
            // if `src` came from src2 we need to invert the values
            CmpValue::CmpImm(src, val, src2.is_reg())
        }

        (Source::Register(src1), Source::Register(src2)) => CmpValue::CmpReg(src1, src2),
    }
}

pub fn same_val<T>(false_val: T, true_val: T, cmp_mode: CmpKind) -> T {
    match cmp_mode {
        CmpKind::Eq | CmpKind::Sge | CmpKind::Uge => true_val,
        CmpKind::Ne | CmpKind::Sl | CmpKind::Ul => false_val,
    }
}

// returns one of the input Ts if the branch becomes unconditional
pub fn zero<T>(false_val: T, true_val: T, cmp_mode: CmpKind) -> Option<T> {
    match cmp_mode {
        CmpKind::Uge => Some(true_val),
        CmpKind::Ul => Some(false_val),
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
    Test(Register, bool),
    CmpReg(Register, Register),
    /// A register to compare, an immediate to compare it to, and whether or not the result
    /// of this comparision should be flipped.
    CmpImm(Register, u32, bool),
}

fn cmp_bit(
    builder: &mut BlockBuilder,
    dest: Register,
    mode: CmpKind,
    flip: bool,
) -> io::Result<()> {
    match (mode, flip) {
        (CmpKind::Eq, false) | (CmpKind::Ne, true) => builder.stream.sete_reg8(dest),
        (CmpKind::Ne, false) | (CmpKind::Eq, true) => builder.stream.setne_reg8(dest),
        (CmpKind::Sl, false) => builder.stream.setl_reg8(dest),
        (CmpKind::Sge, false) => builder.stream.setge_reg8(dest),
        (CmpKind::Ul, false) => builder.stream.setb_reg8(dest),
        (CmpKind::Uge, false) => builder.stream.setae_reg8(dest),
        (CmpKind::Sl, true) => builder.stream.setnl_reg8(dest),
        (CmpKind::Sge, true) => builder.stream.setnge_reg8(dest),
        (CmpKind::Ul, true) => builder.stream.setnb_reg8(dest),
        (CmpKind::Uge, true) => builder.stream.setnae_reg8(dest),
    }
}

pub fn set_bool_conditional(
    builder: &mut BlockBuilder,
    dest: Register,
    src1: Source,
    src2: Source,
    mode: CmpKind,
) -> io::Result<()> {
    let cmp = bool_cmp(src1, src2, 0, 1, mode);
    let xor = match cmp {
        CmpValue::Const(_) => false,
        CmpValue::Test(r, _) | CmpValue::CmpImm(r, _, _) => r != dest,
        CmpValue::CmpReg(r1, r2) => r1 != dest && r2 != dest,
    };

    if xor {
        builder.stream.xor_reg_reg(Reg32(dest), Reg32(dest))?;
    }

    let flip = match cmp {
        CmpValue::Const(v) => {
            builder.mov_r32_imm32(dest, v)?;
            return Ok(());
        }

        CmpValue::Test(r, flip) => {
            builder.stream.test_reg_reg(Reg32(r), Reg32(r))?;
            flip
        }

        CmpValue::CmpImm(src, val, flip) => {
            builder.stream.cmp_reg_imm(Reg32(src), Imm32(val))?;
            flip
        }

        CmpValue::CmpReg(src1, src2) => {
            builder.stream.cmp_reg_reg(Reg32(src1), Reg32(src2))?;
            false
        }
    };

    if !xor {
        // todo: add clobbler to be more efficent (ability to xor)
        builder.stream.mov_reg_imm(Reg32(dest), Imm32(0))?;
    }

    cmp_bit(builder, dest, mode, flip)
}
