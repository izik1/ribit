use super::{BlockBuilder, LoadProfile, Memory, StoreProfile};
use crate::register;
use assembler::mnemonic_parameter_types::registers::Register32Bit;

// this fn is a special cookie and uses `lea`, so it doesn't compose well with `additive_mathi`
pub fn addi(
    builder: &mut BlockBuilder,
    imm: u32,
    rd: register::RiscV,
    rs: Option<register::RiscV>,
) {
    match (rs, imm) {
        (None, _) => builder.write_register_imm(rd, imm, Some(StoreProfile::Allocate)),
        (Some(rs), 0) => builder.register_mov(rd, rs),
        (Some(rs), _) if rd != rs => {
            let (native_rd, rs) = builder.register_manager.alloc_2(
                (rd, rs),
                &[rd, rs],
                &mut builder.stream,
                (LoadProfile::Lazy, LoadProfile::Eager),
            );

            builder.register_manager.set_dirty(rd);

            builder.stream.lea_Register32Bit_Any32BitMemory(
                native_rd.as_asm_reg32(),
                Memory::base_64_displacement(rs.as_asm_reg64(), imm.into()),
            );
        }

        (Some(rs), _) => {
            let rs = builder.ez_alloc(rs);
            builder.register_manager.set_dirty(rd);
            builder
                .stream
                .add_Register32Bit_Immediate32Bit(rs.as_asm_reg32(), imm.into());
        }
    }
}

pub fn xori(
    builder: &mut BlockBuilder,
    imm: u32,
    rd: register::RiscV,
    rs: Option<register::RiscV>,
) {
    additive_mathi(builder, imm, rd, rs, |stream, r, imm| {
        stream.xor_Register32Bit_Immediate32Bit(r, imm.into())
    });
}

pub fn ori(builder: &mut BlockBuilder, imm: u32, rd: register::RiscV, rs: Option<register::RiscV>) {
    additive_mathi(builder, imm, rd, rs, |stream, r, imm| {
        stream.or_Register32Bit_Immediate32Bit(r, imm.into())
    });
}

fn additive_mathi<F>(
    builder: &mut BlockBuilder,
    imm: u32,
    rd: register::RiscV,
    rs: Option<register::RiscV>,
    op: F,
) where
    F: FnOnce(&mut assembler::InstructionStream, Register32Bit, u32),
{
    match (rs, imm) {
        (None, _) => builder.write_register_imm(rd, imm, Some(StoreProfile::Allocate)),
        (Some(rs), 0) => builder.register_mov(rd, rs),
        (Some(rs), _) => {
            if rd != rs {
                builder.register_mov(rd, rs)
            }

            let rd = builder.ez_alloc(rd);
            op(&mut builder.stream, rd.as_asm_reg32(), imm);
        }
    }
}

pub fn andi(
    builder: &mut BlockBuilder,
    imm: u32,
    rd: register::RiscV,
    rs: Option<register::RiscV>,
) {
    match (rs, imm) {
        (None, _) | (_, 0) => builder.write_register_imm(rd, 0, Some(StoreProfile::Allocate)),
        (Some(rs), _) => {
            if rd != rs {
                builder.register_mov(rd, rs)
            }

            let rd = builder.ez_alloc(rd);
            builder
                .stream
                .and_Register32Bit_Immediate32Bit(rd.as_asm_reg32(), imm.into())
        }
    }
}
