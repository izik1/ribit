use super::{BlockBuilder, LoadProfile, Memory, StoreProfile};
use crate::register;
use assembler::mnemonic_parameter_types::registers::Register32Bit;

pub enum ShiftKind {
    LL,
    RL,
    RA,
}

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

pub fn shifti(
    builder: &mut BlockBuilder,
    imm: u32,
    rd: register::RiscV,
    rs: Option<register::RiscV>,
    kind: ShiftKind,
) {
    let rs = if let Some(rs) = rs {
        rs
    } else {
        // no rs -> always 0
        builder.write_register_imm(rd, 0, Some(StoreProfile::Allocate));
        return;
    };

    // high level:
    // mov dest, src ; ommited if not needed
    // <shift> dest, shamt

    // before shifting we need to move src -> dest
    builder.register_mov(rd, rs);

    let shamt = ((imm as u8) & 0x1f).into();

    let dest = builder.ez_alloc(rd).as_asm_reg32();

    match kind {
        // todo: figure out if lea would be better for 1 < shamt < 4.
        // todo: use lea for shamt == 1 IFF rd != rs
        ShiftKind::LL => builder.stream.shl_Register32Bit_Immediate8Bit(dest, shamt),
        ShiftKind::RL => builder.stream.shr_Register32Bit_Immediate8Bit(dest, shamt),
        ShiftKind::RA => builder.stream.sar_Register32Bit_Immediate8Bit(dest, shamt),
    }
}
