#![feature(vec_remove_item, bool_to_option)]
#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    clippy::cast_lossless
)]

#[macro_use]
extern crate static_assertions;

use std::fmt;

pub mod decode;
pub mod instruction;
pub mod jit;
pub mod opcode;
pub mod register;
pub mod ssa;

// note: RISC-V would have these be: B, H(W), W
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Width {
    Byte,
    Word,
    DWord,
}

impl fmt::Display for Width {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Byte => f.write_str("byte"),
            Self::Word => f.write_str("word"),
            Self::DWord => f.write_str("dword"),
        }
    }
}

#[repr(u32)]
enum ReturnCode {
    #[allow(dead_code)]
    Normal = 0,
    EBreak,
    ECall,
}

const XLEN: usize = 32;

const MEMORY_SIZE: u32 = (1024 * 1024 * 16);

// ensure that memory size is a power of two.
const_assert_eq!(MEMORY_SIZE.count_ones(), 1);

// todo: enum DecodeError { InvalidInstruction, Reserved, UnimplementedExtension(extension) }
#[derive(Debug)]
pub enum DecodeError {
    Other,
    InvalidInstruction(u32),
    Compressed(CompressedDecodeError),
    UnimplementedExtension(Extension, u32),
}

impl From<CompressedDecodeError> for DecodeError {
    fn from(e: CompressedDecodeError) -> Self {
        Self::Compressed(e)
    }
}

#[derive(Debug)]
pub enum CompressedDecodeError {
    InvalidInstruction(u16),
    UnimplementedExtension(Extension, u16),
}

#[derive(Debug, Copy, Clone)]
pub enum Extension {
    D,
    F,
}

impl std::fmt::Display for Extension {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::D => write!(f, "RV32-D"),
            Self::F => write!(f, "RV32-F"),
        }
    }
}

pub struct Cpu {
    // xregs[0] is fixed to 0
    xregs: [u32; XLEN],
    pc: u32,
    memory: Box<[u8]>,
    jit: jit::context::Runtime,
}

impl Cpu {
    pub fn pc(&self) -> u32 {
        self.pc
    }

    pub fn new(program: &[u8]) -> Self {
        assert!(MEMORY_SIZE as usize >= program.len());
        let mut memory = vec![0; MEMORY_SIZE as usize].into_boxed_slice();
        memory[0x10000..][..program.len()].copy_from_slice(program);

        let xregs = [0; XLEN];
        let pc = 0x10000;
        let jit = jit::context::Runtime::new();

        Self {
            xregs,
            pc,
            memory,
            jit,
        }
    }

    fn parse_compressed(&mut self) -> Result<instruction::Info, DecodeError> {
        use std::convert::TryInto;

        if self.pc as usize + 1 >= self.memory.len() {
            return Err(DecodeError::Other);
        }

        let instr = u16::from_be_bytes(
            self.memory[(self.pc as usize)..][..2]
                .try_into()
                .expect("bad slice size expected 2???"),
        );


        let instr = decode::compressed::decode_instruction(instr)?;
        let info = instruction::Info::new(instr, self.pc, 2);
        self.pc += 2;
        Ok(info)
    }

    fn parse_instruction(&mut self) -> Result<instruction::Info, DecodeError> {
        use std::convert::TryInto;

        if self.pc as usize + 3 >= self.memory.len() {
            return Err(DecodeError::Other);
        }

        let instr = u32::from_be_bytes(
            self.memory[(self.pc as usize)..][..4]
                .try_into()
                .expect("bad slice size expected 4???"),
        );

        if instr & 0b11 == 0b11 {
            let instr = decode::instruction(instr)?;
            let info = instruction::Info::new(instr, self.pc, 4);
            self.pc += 4;
            Ok(info)
        } else {
            self.parse_compressed()
        }
    }

    fn create_block(&mut self) -> Result<(), DecodeError> {
        let pc = self.pc;
        let mut block_instrs = Vec::new();

        let terminator;
        loop {
            log::debug!("PC: ${:04x}", self.pc);
            let inst_info = self.parse_instruction()?;
            if inst_info.instruction.is_terminator() {
                terminator = inst_info;
                break;
            } else {
                block_instrs.push(inst_info);
            }
        }

        self.pc = pc;
        self.jit.generate_basic_block(block_instrs, terminator);

        Ok(())
    }

    pub fn run(&mut self) -> Result<(), DecodeError> {
        if !self.jit.lookup_block(self.pc) {
            self.create_block()?;
        }

        self.jit
            .execute_basic_block(&mut self.pc, &mut self.xregs, &mut self.memory);

        Ok(())
    }
}
