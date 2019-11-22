use super::BlockBuilder;
use crate::opcode::Width;
use crate::register;

use assembler::mnemonic_parameter_types::memory::{
    Any16BitMemory, Any32BitMemory, Any8BitMemory, Memory as AssemblerMemory,
};
use assembler::mnemonic_parameter_types::registers::{
    Register16Bit, Register32Bit, Register64Bit, Register8Bit,
};

pub enum Memory {
    Byte(Any8BitMemory),
    Word(Any16BitMemory),
    DWord(Any32BitMemory),
}

impl Memory {
    pub fn new(width: Width, addr: u32) -> Self {
        match width {
            Width::Byte => Memory::Byte(AssemblerMemory::base_64_displacement(
                Register64Bit::RDX,
                addr.into(),
            )),
            Width::Word => Memory::Word(AssemblerMemory::base_64_displacement(
                Register64Bit::RDX,
                addr.into(),
            )),
            Width::DWord => Memory::DWord(AssemblerMemory::base_64_displacement(
                Register64Bit::RDX,
                addr.into(),
            )),
        }
    }

    pub fn mem_eax(width: Width) -> Self {
        match width {
            Width::Byte => Memory::Byte(AssemblerMemory::base_64_index_64(
                Register64Bit::RDX,
                Register64Bit::RAX,
            )),
            Width::Word => Memory::Word(AssemblerMemory::base_64_index_64(
                Register64Bit::RDX,
                Register64Bit::RAX,
            )),
            Width::DWord => Memory::DWord(AssemblerMemory::base_64_index_64(
                Register64Bit::RDX,
                Register64Bit::RAX,
            )),
        }
    }
}

pub enum Register {
    Byte(Register8Bit),
    Word(Register16Bit),
    DWord(Register32Bit),
}

// fixme: handle memory size changing in the future.
pub const fn imm_16_to_addr(imm: u16) -> u32 {
    let imm = imm as i16 as u32;

    // ensure that imm gets "wrapped" around memory overflows.
    imm & (1024 * 1024 * 16 - 1)
}

pub const fn u32_to_addr(val: u32) -> u32 {
    val & (1024 * 1024 * 16 - 1)
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
