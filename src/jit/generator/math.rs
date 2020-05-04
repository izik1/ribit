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
    builder.mov_reg_src(dest, src1)?;

    let dest = Reg32(dest);

    match (op, src2, dest.0 == Register::Zax) {
        (BinOp::Add, Source::Register(r), _) => builder.stream.add_reg_reg(dest, Reg32(r)),
        (BinOp::And, Source::Register(r), _) => builder.stream.and_reg_reg(dest, Reg32(r)),
        (BinOp::Or, Source::Register(r), _) => builder.stream.or_reg_reg(dest, Reg32(r)),

        // todo: optimize by avoiding the move earlier.
        (BinOp::Sll, Source::Register(r), _) => {
            builder.stream.shlx_reg_reg_reg(dest, dest, Reg32(r))
        }
        (BinOp::Srl, Source::Register(r), _) => {
            builder.stream.shrx_reg_reg_reg(dest, dest, Reg32(r))
        }
        (BinOp::Sra, Source::Register(r), _) => {
            builder.stream.sarx_reg_reg_reg(dest, dest, Reg32(r))
        }

        (BinOp::Sub, Source::Register(r), _) => builder.stream.sub_reg_reg(dest, Reg32(r)),
        (BinOp::Xor, Source::Register(r), _) => builder.stream.xor_reg_reg(dest, Reg32(r)),

        (BinOp::Add, Source::Val(v), false) => builder.stream.add_reg_imm(dest, Imm32(v)),
        (BinOp::And, Source::Val(v), false) => builder.stream.and_reg_imm(dest, Imm32(v)),
        (BinOp::Or, Source::Val(v), false) => builder.stream.or_reg_imm(dest, Imm32(v)),
        (BinOp::Sll, Source::Val(v), _) => builder.stream.shl_reg_imm8(dest, v as u8),
        (BinOp::Srl, Source::Val(v), _) => builder.stream.shr_reg_imm8(dest, v as u8),
        (BinOp::Sra, Source::Val(v), _) => builder.stream.sar_reg_imm8(dest, v as u8),
        (BinOp::Sub, Source::Val(v), false) => builder.stream.sub_reg_imm(dest, Imm32(v)),
        (BinOp::Xor, Source::Val(v), false) => builder.stream.xor_reg_imm(dest, Imm32(v)),

        (BinOp::Add, Source::Val(v), true) => builder.stream.add_zax_imm(Imm32(v)),
        (BinOp::And, Source::Val(v), true) => builder.stream.and_zax_imm(Imm32(v)),
        (BinOp::Or, Source::Val(v), true) => builder.stream.or_zax_imm(Imm32(v)),
        (BinOp::Sub, Source::Val(v), true) => builder.stream.sub_zax_imm(Imm32(v)),
        (BinOp::Xor, Source::Val(v), true) => builder.stream.xor_zax_imm(Imm32(v)),
    }
}
