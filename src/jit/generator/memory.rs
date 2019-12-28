use super::BlockBuilder;
use crate::register;
use crate::Width;

use rasen::params::mem::Mem;
use rasen::params::{
    mem::{Mem16, Mem32, Mem8},
    Imm16, Imm32, Imm8, Register, W32,
};

#[derive(Clone)]
pub enum Memory {
    Byte(Mem8),
    Word(Mem16),
    DWord(Mem32),
}

impl Memory {
    pub fn new(width: Width, imm: u16) -> Self {
        Self::with_mem_width(
            width,
            Mem::base_displacement(Register::Zdx, imm_16_to_addr(imm) as i32),
        )
    }

    fn with_mem_width(width: Width, mem: Mem) -> Self {
        match width {
            Width::Byte => Self::Byte(Mem8(mem)),
            Width::Word => Self::Word(Mem16(mem)),
            Width::DWord => Self::DWord(Mem32(mem)),
        }
    }

    pub fn mem_eax(width: Width) -> Self {
        Self::with_mem_width(
            width,
            Mem::base_index(Register::Zdx, Register::Zax).unwrap(),
        )
    }
}

// fixme: handle memory size changing in the future.
pub const fn imm_16_to_addr(imm: u16) -> u32 {
    let imm = imm as i16 as u32;

    // ensure that imm gets "wrapped" around memory overflows.
    imm & (crate::MEMORY_SIZE - 1)
}

pub fn store_src_0(builder: &mut BlockBuilder, displacement: Memory) {
    match displacement {
        Memory::Byte(displacement) => builder.stream.mov_mem_imm(displacement, Imm8(0)).unwrap(),

        Memory::Word(displacement) => builder.stream.mov_mem_imm(displacement, Imm16(0)).unwrap(),

        Memory::DWord(displacement) => builder.stream.mov_mem_imm(displacement, Imm32(0)).unwrap(),
    };
}

pub fn store(builder: &mut BlockBuilder, base: Memory, src: register::Native) {
    match base {
        Memory::Byte(displacement) => builder
            .stream
            .mov_mem_reg(displacement, src.as_rasen_reg())
            .unwrap(),

        Memory::Word(displacement) => builder
            .stream
            .mov_mem_reg(displacement, src.as_rasen_reg())
            .unwrap(),

        Memory::DWord(displacement) => builder
            .stream
            .mov_mem_reg(displacement, src.as_rasen_reg())
            .unwrap(),
    };
}

pub fn load_rs_imm(
    builder: &mut BlockBuilder,
    rd: register::RiscV,
    base: Option<register::RiscV>,
    imm: u16,
    width: Width,
    sign_extend: bool,
) {
    use super::LoadProfile;

    let displacement = match base {
        None => Memory::new(width, imm),

        Some(base) => {
            let (_, base) = builder.register_manager.alloc_2(
                (rd, base),
                &[rd, base],
                &mut builder.stream,
                (LoadProfile::Lazy, LoadProfile::Eager),
            );

            dyn_address(builder, base, imm);

            Memory::mem_eax(width)
        }
    };

    let rd = builder
        .register_manager
        .alloc(rd, &[], &mut builder.stream, LoadProfile::Lazy);

    match sign_extend {
        true => load(builder, displacement, rd),
        false => loadu(builder, displacement, rd),
    };
}

fn load(builder: &mut BlockBuilder, base: Memory, dest: register::Native) {
    match base {
        Memory::Byte(displacement) => builder
            .stream
            .movsx_reg_mem8::<W32, _, _>(dest.as_rasen_reg(), displacement)
            .unwrap(),
        Memory::Word(displacement) => builder
            .stream
            .movsx_reg_mem16::<W32, _, _>(dest.as_rasen_reg(), displacement)
            .unwrap(),
        Memory::DWord(displacement) => builder
            .stream
            .mov_reg_mem(dest.as_rasen_reg(), displacement)
            .unwrap(),
    }
}

fn loadu(builder: &mut BlockBuilder, base: Memory, dest: register::Native) {
    match base {
        Memory::Byte(displacement) => builder
            .stream
            .movzx_reg_mem8::<W32, _, _>(dest.as_rasen_reg(), displacement)
            .unwrap(),
        Memory::Word(displacement) => builder
            .stream
            .movzx_reg_mem16::<W32, _, _>(dest.as_rasen_reg(), displacement)
            .unwrap(),
        Memory::DWord(displacement) => builder
            .stream
            .mov_reg_mem(dest.as_rasen_reg(), displacement)
            .unwrap(),
    }
}

pub fn dyn_address(builder: &mut BlockBuilder, base: register::Native, imm: u16) {
    builder
        .stream
        .lea_reg_mem(
            Register::Zax,
            Mem32(Mem::base_displacement(
                base.as_rasen_reg(),
                (imm as i16 as u32) as i32,
            )),
        )
        .unwrap();

    builder
        .stream
        .and_zax_imm(Imm32(crate::MEMORY_SIZE - 1))
        .unwrap();
}
