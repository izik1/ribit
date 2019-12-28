use super::{BlockBuilder, LoadProfile, StoreProfile};
use crate::jit::Assembler;
use crate::register;
use rasen::params::mem::{Mem, Mem32};
use rasen::params::{Imm32, Reg32};

#[derive(Copy, Clone)]
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

            builder
                .stream
                .lea_reg_mem(native_rd, Mem32(Mem::base_displacement(rs, imm as i32)))
                .unwrap();
        }

        (Some(rs), _) => {
            let rs = builder.ez_alloc(rs);
            builder.register_manager.set_dirty(rd);
            builder.stream.add_reg_imm(rs, Imm32(imm)).unwrap();
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
        stream.xor_reg_imm(r, Imm32(imm)).unwrap()
    });
}

pub fn ori(builder: &mut BlockBuilder, imm: u32, rd: register::RiscV, rs: Option<register::RiscV>) {
    additive_mathi(builder, imm, rd, rs, |stream, r, imm| {
        stream.or_reg_imm(r, Imm32(imm)).unwrap()
    });
}

fn additive_mathi<F>(
    builder: &mut BlockBuilder,
    imm: u32,
    rd: register::RiscV,
    rs: Option<register::RiscV>,
    op: F,
) where
    F: FnOnce(&mut Assembler, rasen::params::Register, u32),
{
    match (rs, imm) {
        (None, _) => builder.write_register_imm(rd, imm, Some(StoreProfile::Allocate)),
        (Some(rs), 0) => builder.register_mov(rd, rs),
        (Some(rs), _) => {
            if rd != rs {
                builder.register_mov(rd, rs)
            }

            let rd = builder.ez_alloc(rd);
            op(&mut builder.stream, rd, imm);
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
            builder.stream.and_reg_imm(rd, Imm32(imm)).unwrap()
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

    let shamt = (imm as u8) & 0x1f;

    let dest = Reg32(builder.ez_alloc(rd));

    match kind {
        // todo: figure out if lea would be better for 1 < shamt < 4.
        // todo: use lea for shamt == 1 IFF rd != rs
        ShiftKind::LL => builder.stream.shl_reg_imm8(dest, shamt).unwrap(),
        ShiftKind::RL => builder.stream.shr_reg_imm8(dest, shamt).unwrap(),
        ShiftKind::RA => builder.stream.sar_reg_imm8(dest, shamt).unwrap(),
    }
}
