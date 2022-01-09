use std::io;

use rasen::params::imm::Imm32;
use rasen::params::reg::Reg32;
use rasen::params::Register;
use ribit_ssa::{BinOp, CommutativeBinOp};

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

pub fn binop(
    builder: &mut BlockBuilder,
    dest: Register,
    src: SourcePair,
    op: BinOp,
) -> io::Result<()> {
    // todo: if not 3-arg

    match src {
        SourcePair::RegReg(lhs, rhs) => match op {
            BinOp::Sll => builder.stream.shlx_reg_reg_reg(Reg32(dest), Reg32(lhs), Reg32(rhs)),
            BinOp::Srl => builder.stream.shrx_reg_reg_reg(Reg32(dest), Reg32(lhs), Reg32(rhs)),
            BinOp::Sra => builder.stream.sarx_reg_reg_reg(Reg32(dest), Reg32(lhs), Reg32(rhs)),
            BinOp::Sub => {
                match (lhs == dest, rhs == dest) {
                    (true, true) => {}
                    (true, false) => {}
                    // painful: need a 3rd register to swap lhs and rhs and then restore rhs.
                    (false, true) => unimplemented!(),
                    (false, false) => {
                        builder.stream.mov_reg_reg(Reg32(dest), Reg32(lhs))?;
                    }
                };

                builder.stream.sub_reg_reg(Reg32(dest), Reg32(rhs))
            }
        },
        SourcePair::RegVal(lhs, rhs) => {
            if lhs != dest {
                builder.stream.mov_reg_reg(Reg32(dest), Reg32(lhs))?;
            }

            match op {
                BinOp::Sll => builder.stream.shl_reg_imm8(Reg32(dest), rhs as u8),
                BinOp::Srl => builder.stream.shr_reg_imm8(Reg32(dest), rhs as u8),
                BinOp::Sra => builder.stream.sar_reg_imm8(Reg32(dest), rhs as u8),
                BinOp::Sub => sub(builder, Reg32(dest), rhs),
            }
        }

        SourcePair::ValReg(0, rhs) if op == BinOp::Sub => {
            if rhs != dest {
                builder.stream.mov_reg_reg(Reg32(dest), Reg32(rhs))?;
            }

            builder.stream.neg_reg(Reg32(dest))
        }

        // shifts of 0 by n are trivially 0.
        SourcePair::ValReg(0, _) => builder.mov_r32_imm32(dest, 0),

        // we need to be able to move an immediate into `dest`,
        // if `rhs` is there we need a scratch register.
        SourcePair::ValReg(lhs, rhs) if rhs != dest => {
            builder.mov_r32_imm32(dest, lhs)?;
            match op {
                BinOp::Sll => builder.stream.shlx_reg_reg_reg(Reg32(dest), Reg32(dest), Reg32(rhs)),
                BinOp::Srl => builder.stream.shrx_reg_reg_reg(Reg32(dest), Reg32(dest), Reg32(rhs)),
                BinOp::Sra => builder.stream.sarx_reg_reg_reg(Reg32(dest), Reg32(dest), Reg32(rhs)),
                BinOp::Sub => builder.stream.sub_reg_reg(Reg32(dest), Reg32(rhs)),
            }
        }

        // there's still hope!
        // we can turn `a - b` into `a + (-b)` into `(-b) + a`
        SourcePair::ValReg(lhs, _) if op == BinOp::Sub => {
            builder.stream.neg_reg(Reg32(dest))?;
            add_r32_imm(builder, Reg32(dest), lhs)
        }

        // we tried really hard. This *really* needs a clobber.
        SourcePair::ValReg(_, _) => panic!(),
    }

    // match (op, src) {
    //     (BinOp::Sll, SourcePair::RegReg(_, _)) => todo!(),
    //     (BinOp::Sll, SourcePair::RegVal(_, _)) => todo!(),
    //     (BinOp::Sll, SourcePair::ValReg(_, _)) => todo!(),
    //     (BinOp::Srl, SourcePair::RegReg(_, _)) => todo!(),
    //     (BinOp::Srl, SourcePair::RegVal(_, _)) => todo!(),
    //     (BinOp::Srl, SourcePair::ValReg(_, _)) => todo!(),
    //     (BinOp::Sra, SourcePair::RegReg(_, _)) => todo!(),
    //     (BinOp::Sra, SourcePair::RegVal(_, _)) => todo!(),
    //     (BinOp::Sra, SourcePair::ValReg(_, _)) => todo!(),
    //     (BinOp::Sub, SourcePair::RegReg(_, _)) => todo!(),
    //     (BinOp::Sub, SourcePair::RegVal(_, _)) => todo!(),
    //     (BinOp::Sub, SourcePair::ValReg(_, _)) => todo!(),
    // }

    // // fixme: MISCOMPILE: This doesn't function right if src2 != src1 and is already in dest.
    // builder.mov_reg_src(dest, src1)?;

    // let dest = Reg32(dest);

    // match (op, src2) {
    //     (BinOp::Sll, Source::Register(r)) => builder.stream.shlx_reg_reg_reg(dest, dest, Reg32(r)),
    //     (BinOp::Srl, Source::Register(r)) => builder.stream.shrx_reg_reg_reg(dest, dest, Reg32(r)),
    //     (BinOp::Sra, Source::Register(r)) => builder.stream.sarx_reg_reg_reg(dest, dest, Reg32(r)),

    //     (BinOp::Sll, Source::Val(v)) => builder.stream.shl_reg_imm8(dest, v as u8),
    //     (BinOp::Srl, Source::Val(v)) => builder.stream.shr_reg_imm8(dest, v as u8),
    //     (BinOp::Sra, Source::Val(v)) => builder.stream.sar_reg_imm8(dest, v as u8),

    //     (BinOp::Sub, src2) => sub(builder, dest, src2),
    // }
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

fn sub(builder: &mut BlockBuilder, dest: Reg32, v: u32) -> Result<(), io::Error> {
    add_r32_imm(builder, dest, -(v as i32) as u32)
}
