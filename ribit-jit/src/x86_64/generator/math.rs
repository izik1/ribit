use std::io;

use rasen::params::imm::Imm32;
use rasen::params::reg::Reg32;
use rasen::params::Register;
use ribit_ssa::{BinOp, CommutativeBinOp};

use super::BlockBuilder;
use crate::Source;

pub fn commutative_binop(
    builder: &mut BlockBuilder,
    dest: Register,
    src1: Register,
    src2: Source,
    op: CommutativeBinOp,
) -> io::Result<()> {
    let dest = Reg32(dest);
    let src1 = Reg32(src1);

    // legalization will always put src1 (if either) into dest, so, src2 can't be dest here.
    // *unless* src1 == src2
    if src1 != dest {
        builder.stream.mov_reg_reg(dest, src1)?;
    }

    match src2 {
        Source::Val(src2) => match (op, dest == Reg32::ZAX) {
            (CommutativeBinOp::Add, _) if src2 == 1 => builder.stream.inc_reg(dest),
            (CommutativeBinOp::Xor, _) if src2 == u32::MAX => builder.stream.not_reg(dest),

            (CommutativeBinOp::And, true) => builder.stream.and_zax_imm(Imm32(src2)),
            (CommutativeBinOp::Add, true) => builder.stream.add_zax_imm(Imm32(src2)),
            (CommutativeBinOp::Or, true) => builder.stream.or_zax_imm(Imm32(src2)),
            (CommutativeBinOp::Xor, true) => builder.stream.xor_zax_imm(Imm32(src2)),
            (CommutativeBinOp::And, false) => builder.stream.and_reg_imm(dest, Imm32(src2)),
            (CommutativeBinOp::Add, false) => builder.stream.add_reg_imm(dest, Imm32(src2)),
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
    src1: Source,
    src2: Source,
    op: BinOp,
) -> io::Result<()> {
    // todo: if not 3-arg
    // fixme: MISCOMPILE: This doesn't function right if src2 != src1 and is already in dest.
    builder.mov_reg_src(dest, src1)?;

    let dest = Reg32(dest);

    match (op, src2) {
        // todo: optimize by avoiding the move earlier.
        (BinOp::Sll, Source::Register(r)) => builder.stream.shlx_reg_reg_reg(dest, dest, Reg32(r)),
        (BinOp::Srl, Source::Register(r)) => builder.stream.shrx_reg_reg_reg(dest, dest, Reg32(r)),
        (BinOp::Sra, Source::Register(r)) => builder.stream.sarx_reg_reg_reg(dest, dest, Reg32(r)),

        (BinOp::Sll, Source::Val(v)) => builder.stream.shl_reg_imm8(dest, v as u8),
        (BinOp::Srl, Source::Val(v)) => builder.stream.shr_reg_imm8(dest, v as u8),
        (BinOp::Sra, Source::Val(v)) => builder.stream.sar_reg_imm8(dest, v as u8),

        (BinOp::Sub, Source::Register(r)) => builder.stream.sub_reg_reg(dest, Reg32(r)),
        (BinOp::Sub, Source::Val(1)) => builder.stream.dec_reg(dest),
        (BinOp::Sub, Source::Val(v)) if dest == Reg32::ZAX => builder.stream.sub_zax_imm(Imm32(v)),
        (BinOp::Sub, Source::Val(v)) => builder.stream.sub_reg_imm(dest, Imm32(v)),
    }
}
