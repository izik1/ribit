use super::BlockBuilder;
use crate::opcode::Width;
use crate::register;

use assembler::mnemonic_parameter_types::memory::{
    Any16BitMemory, Any32BitMemory, Any8BitMemory, Memory as AssemblerMemory,
};
use assembler::mnemonic_parameter_types::registers::{Register32Bit, Register64Bit};

#[derive(Copy, Clone)]
pub enum Memory {
    Byte(Any8BitMemory),
    Word(Any16BitMemory),
    DWord(Any32BitMemory),
}

impl Memory {
    pub fn new(width: Width, imm: u16) -> Self {
        let addr = imm_16_to_addr(imm);
        match width {
            Width::Byte => Self::Byte(AssemblerMemory::base_64_displacement(
                Register64Bit::RDX,
                addr.into(),
            )),
            Width::Word => Self::Word(AssemblerMemory::base_64_displacement(
                Register64Bit::RDX,
                addr.into(),
            )),
            Width::DWord => Self::DWord(AssemblerMemory::base_64_displacement(
                Register64Bit::RDX,
                addr.into(),
            )),
        }
    }

    pub fn mem_eax(width: Width) -> Self {
        match width {
            Width::Byte => Self::Byte(AssemblerMemory::base_64_index_64(
                Register64Bit::RDX,
                Register64Bit::RAX,
            )),
            Width::Word => Self::Word(AssemblerMemory::base_64_index_64(
                Register64Bit::RDX,
                Register64Bit::RAX,
            )),
            Width::DWord => Self::DWord(AssemblerMemory::base_64_index_64(
                Register64Bit::RDX,
                Register64Bit::RAX,
            )),
        }
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
        Memory::Byte(displacement) => builder
            .stream
            .mov_Any8BitMemory_Immediate8Bit(displacement, 0_u8.into()),

        Memory::Word(displacement) => builder
            .stream
            .mov_Any16BitMemory_Immediate16Bit(displacement, 0_u16.into()),

        Memory::DWord(displacement) => builder
            .stream
            .mov_Any32BitMemory_Immediate32Bit(displacement, 0_u32.into()),
    };
}

pub fn store(builder: &mut BlockBuilder, base: Memory, src: register::Native) {
    match base {
        Memory::Byte(displacement) => builder
            .stream
            .mov_Any8BitMemory_Register8Bit(displacement, src.as_asm_reg8()),

        Memory::Word(displacement) => builder
            .stream
            .mov_Any16BitMemory_Register16Bit(displacement, src.as_asm_reg16()),

        Memory::DWord(displacement) => builder
            .stream
            .mov_Any32BitMemory_Register32Bit(displacement, src.as_asm_reg32()),
    };
}

pub fn dyn_address(builder: &mut BlockBuilder, base: register::Native, imm: u16) {
    builder.stream.lea_Register32Bit_Any32BitMemory(
        Register32Bit::EAX,
        AssemblerMemory::base_64_displacement(base.as_asm_reg64(), (imm as i16 as u32).into()),
    );

    builder
        .stream
        .and_EAX_Immediate32Bit((crate::MEMORY_SIZE - 1).into());
}
