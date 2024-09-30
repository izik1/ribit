use std::io;

use rasen::params::imm::Imm32;
use rasen::params::reg::Reg32;
use rasen::params::Register;
use ribit_ssa::{CommutativeBinOp, ShiftKind};

use super::BlockBuilder;
use crate::{Source, SourcePair};

pub fn commutative_binop(
    builder: &mut BlockBuilder,
    dest: Register,
    src1: Register,
    src2: Source,
    op: CommutativeBinOp,
) -> io::Result<()> {
    // legalization will always put src1 (if either) into dest, so, src2 can't be dest here.
    // *unless* src1 == src2
    if let Source::Register(src2) = src2 {
        debug_assert!(src2 != dest || src1 == src2);
    }

    let dest = Reg32(dest);
    let src1 = Reg32(src1);

    if src1 != dest {
        builder.stream.mov_reg_reg(dest, src1)?;
    }

    match src2 {
        Source::Val(src2) => match (op, dest == Reg32::ZAX) {
            (CommutativeBinOp::Add, _) => add_r32_imm(builder, dest, src2),
            (CommutativeBinOp::Xor, _) if src2 == u32::MAX => builder.stream.not_reg(dest),

            (CommutativeBinOp::And, true) => builder.stream.and_zax_imm(Imm32(src2)),
            (CommutativeBinOp::Or, true) => builder.stream.or_zax_imm(Imm32(src2)),
            (CommutativeBinOp::Xor, true) => builder.stream.xor_zax_imm(Imm32(src2)),
            (CommutativeBinOp::And, false) => builder.stream.and_reg_imm(dest, Imm32(src2)),
            (CommutativeBinOp::Or, false) => builder.stream.or_reg_imm(dest, Imm32(src2)),
            (CommutativeBinOp::Xor, false) => builder.stream.xor_reg_imm(dest, Imm32(src2)),
        },

        Source::Register(src2) => {
            let src2 = Reg32(src2);

            match op {
                CommutativeBinOp::And => builder.stream.and_reg_reg(dest, src2),
                CommutativeBinOp::Add => builder.stream.add_reg_reg(dest, src2),
                CommutativeBinOp::Or => builder.stream.or_reg_reg(dest, src2),
                CommutativeBinOp::Xor => builder.stream.xor_reg_reg(dest, src2),
            }
        }
    }
}

pub fn sub(
    builder: &mut BlockBuilder,
    dest: Register,
    lhs: Source,
    rhs: Register,
) -> io::Result<()> {
    let lhs = match lhs {
        Source::Register(lhs) => {
            match (lhs == dest, rhs == dest) {
                (true, _) => {}
                // painful: need a 3rd register to swap lhs and rhs and then restore rhs.
                (false, true) => unimplemented!(),
                (false, false) => {
                    builder.stream.mov_reg_reg(Reg32(dest), Reg32(lhs))?;
                }
            };

            return builder.stream.sub_reg_reg(Reg32(dest), Reg32(rhs));
        }

        Source::Val(v) => v,
    };

    if lhs == 0 {
        if rhs != dest {
            builder.stream.mov_reg_reg(Reg32(dest), Reg32(rhs))?;
        }

        return builder.stream.neg_reg(Reg32(dest));
    }

    if rhs != dest {
        builder.mov_r32_imm32(dest, lhs)?;
        return builder.stream.sub_reg_reg(Reg32(dest), Reg32(rhs));
    }

    // we can turn `a - b` into `a + (-b)` into `(-b) + a`
    builder.stream.neg_reg(Reg32(dest))?;
    add_r32_imm(builder, Reg32(dest), lhs)
}

fn shift_3arg(
    builder: &mut BlockBuilder,
    dest: Register,
    lhs: Register,
    rhs: Register,
    op: ShiftKind,
) -> io::Result<()> {
    let dest = Reg32(dest);
    let lhs = Reg32(lhs);
    let rhs = Reg32(rhs);
    match op {
        ShiftKind::Sll => builder.stream.shlx_reg_reg_reg(dest, lhs, rhs),
        ShiftKind::Srl => builder.stream.shrx_reg_reg_reg(dest, lhs, rhs),
        ShiftKind::Sra => builder.stream.sarx_reg_reg_reg(dest, lhs, rhs),
    }
}

pub fn shift(
    builder: &mut BlockBuilder,
    dest: Register,
    src: SourcePair,
    op: ShiftKind,
) -> io::Result<()> {
    // todo: if not 3-arg

    match src {
        SourcePair::RegReg(lhs, rhs) => shift_3arg(builder, dest, lhs, rhs, op),
        SourcePair::RegVal(lhs, rhs) => {
            if lhs != dest {
                builder.stream.mov_reg_reg(Reg32(dest), Reg32(lhs))?;
            }

            match op {
                ShiftKind::Sll => builder.stream.shl_reg_imm8(Reg32(dest), rhs as u8),
                ShiftKind::Srl => builder.stream.shr_reg_imm8(Reg32(dest), rhs as u8),
                ShiftKind::Sra => builder.stream.sar_reg_imm8(Reg32(dest), rhs as u8),
            }
        }

        // shifts of 0 by n are trivially 0.
        SourcePair::ValReg(0, _) => builder.mov_r32_imm32(dest, 0),

        // we need to be able to move an immediate into `dest`,
        // if `rhs` is there we need a scratch register.
        SourcePair::ValReg(lhs, rhs) if rhs != dest => {
            builder.mov_r32_imm32(dest, lhs)?;
            shift_3arg(builder, dest, dest, rhs, op)
        }

        // Sadly, this needs a clobber.
        SourcePair::ValReg(_, _) => panic!(),
    }
}

fn add_r32_imm(builder: &mut BlockBuilder, dest: Reg32, v: u32) -> Result<(), io::Error> {
    // inc/dec is cheapest (on most modern cpus)
    if v == 1 {
        return builder.stream.inc_reg(dest);
    }

    if v == u32::MAX {
        return builder.stream.dec_reg(dest);
    }

    // closely followed by (or sometimes surpased by) `sximm8`
    if let Ok(v) = i8::try_from(v as i32) {
        return builder.stream.add_reg_sximm8(dest, v);
    }

    // *one* value where we can try doing the opposite
    if let Ok(v) = i8::try_from(-(v as i32)) {
        return builder.stream.sub_reg_sximm8(dest, v);
    }

    // then, dodging the `mod r/m`
    if dest == Reg32::ZAX {
        return builder.stream.add_zax_imm(Imm32(v));
    }

    // fallback.
    builder.stream.add_reg_imm(dest, Imm32(v))
}
