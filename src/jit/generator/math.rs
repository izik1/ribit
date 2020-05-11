use super::BlockBuilder;
use crate::jit::Source;

use crate::ssa::BinOp;
use rasen::params::{imm::Imm32, reg::Reg32, Register};

use std::io;

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
        (BinOp::Add, Source::Register(r)) => builder.stream.add_reg_reg(dest, Reg32(r)),
        (BinOp::And, Source::Register(r)) => builder.stream.and_reg_reg(dest, Reg32(r)),
        (BinOp::Or, Source::Register(r)) => builder.stream.or_reg_reg(dest, Reg32(r)),

        // todo: optimize by avoiding the move earlier.
        (BinOp::Sll, Source::Register(r)) => builder.stream.shlx_reg_reg_reg(dest, dest, Reg32(r)),
        (BinOp::Srl, Source::Register(r)) => builder.stream.shrx_reg_reg_reg(dest, dest, Reg32(r)),
        (BinOp::Sra, Source::Register(r)) => builder.stream.sarx_reg_reg_reg(dest, dest, Reg32(r)),

        (BinOp::Sub, Source::Register(r)) => builder.stream.sub_reg_reg(dest, Reg32(r)),
        (BinOp::Xor, Source::Register(r)) => builder.stream.xor_reg_reg(dest, Reg32(r)),

        (BinOp::Add, Source::Val(1)) => builder.stream.inc_reg(dest),
        (BinOp::Sub, Source::Val(1)) => builder.stream.dec_reg(dest),
        (BinOp::Xor, Source::Val(u32::MAX)) => builder.stream.not_reg(dest),

        (BinOp::Add, Source::Val(v)) if dest == Reg32::ZAX => builder.stream.add_zax_imm(Imm32(v)),
        (BinOp::And, Source::Val(v)) if dest == Reg32::ZAX => builder.stream.and_zax_imm(Imm32(v)),
        (BinOp::Or, Source::Val(v)) if dest == Reg32::ZAX => builder.stream.or_zax_imm(Imm32(v)),
        (BinOp::Sub, Source::Val(v)) if dest == Reg32::ZAX => builder.stream.sub_zax_imm(Imm32(v)),
        (BinOp::Xor, Source::Val(v)) if dest == Reg32::ZAX => builder.stream.xor_zax_imm(Imm32(v)),

        (BinOp::Add, Source::Val(v)) => builder.stream.add_reg_imm(dest, Imm32(v)),
        (BinOp::And, Source::Val(v)) => builder.stream.and_reg_imm(dest, Imm32(v)),
        (BinOp::Or, Source::Val(v)) => builder.stream.or_reg_imm(dest, Imm32(v)),
        (BinOp::Sub, Source::Val(v)) => builder.stream.sub_reg_imm(dest, Imm32(v)),
        (BinOp::Xor, Source::Val(v)) => builder.stream.xor_reg_imm(dest, Imm32(v)),

        (BinOp::Sll, Source::Val(v)) => builder.stream.shl_reg_imm8(dest, v as u8),
        (BinOp::Srl, Source::Val(v)) => builder.stream.shr_reg_imm8(dest, v as u8),
        (BinOp::Sra, Source::Val(v)) => builder.stream.sar_reg_imm8(dest, v as u8),
    }
}
