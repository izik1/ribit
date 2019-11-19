use crate::instruction::Instruction;

use assembler::{ExecutableAnonymousMemoryMap, InstructionStream};
use std::ops::Range;

use assembler::mnemonic_parameter_types::registers::Register32Bit as AssemblerReg32;

mod alloc;
mod generator;

type BasicBlock = unsafe extern "sysv64" fn(regs: *mut u32, ctx: &mut JitContext, memory: *mut u8) -> u32;

pub struct InstructionInfo {
    instruction: Instruction,
    start_address: u32,
    len: u32,
}

impl InstructionInfo {
    pub fn end_address(&self) -> u32 {
        self.start_address.wrapping_add(self.len)
    }

    pub fn new(instruction: Instruction, start_address: u32, len: u32) -> Self {
        Self {
            instruction,
            start_address,
            len,
        }
    }
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
enum NativeRegister {
    RDX,
    RCX,
    R8,
    R9,
    // RAX,
}

impl NativeRegister {
    fn as_assembly_reg32(&self) -> AssemblerReg32 {
        match self {
            Self::RDX => AssemblerReg32::EDX,
            Self::RCX => AssemblerReg32::ECX,
            Self::R8 => AssemblerReg32::R8D,
            Self::R9 => AssemblerReg32::R9D,
        }
    }
}

pub struct JitContext {
    buffer: ExecutableAnonymousMemoryMap,
    blocks: Vec<BasicBlock>,
    ranges: Vec<Range<u32>>,
}

impl JitContext {
    // todo: clean
    pub fn execute_basic_block(&mut self, pc: &mut u32, regs: &mut [u32; crate::XLEN], memory: &mut [u8]) {
        // assert 16 MiB of memory for now
        assert_eq!(memory.len(), 1024*1024*16);

        if let Some(block_num) = self.ranges.iter().position(|range| range.start == *pc) {
            let block = &self.blocks[block_num];
            *pc = unsafe { block(regs.as_mut_ptr(), self, memory.as_mut_ptr()) }
        } else {
            todo!("put an error here")
        }
    }

    pub fn new() -> Self {
        Self {
            buffer: ExecutableAnonymousMemoryMap::new(4096 * 16, false, true).unwrap(),
            blocks: vec![],
            ranges: vec![],
        }
    }

    // todo: signature
    pub fn generate_basic_block(
        &mut self,
        block_instrs: Vec<InstructionInfo>,
        branch: InstructionInfo,
    ) {
        generator::generate_basic_block(self, block_instrs, branch);
    }
}

#[test]
fn jal_basic() {
    use crate::instruction::{JTypeInstruction, JTypeOpcode};
    let mut ctx = JitContext::new();

    ctx.generate_basic_block(
        vec![],
        InstructionInfo::new(
            Instruction::JType(JTypeInstruction {
                imm: 4096,
                rd: 4,
                opcode: JTypeOpcode::JAL,
            }),
            0,
            4,
        ),
    );

    let mut regs = [0xaaaaaaaa; 32];
    regs[0] = 0;
    let mut pc = 0;

    ctx.execute_basic_block(&mut pc, &mut regs);
    assert_eq!(pc, 4096 + 4);

    assert_eq!(regs[0], 0);

    for idx in (1..regs.len()) {
        if idx == 4 {
            assert_eq!(regs[idx], 4);
        } else {
            assert_eq!(regs[idx], 0xaaaaaaaa);
        }
    }
}

#[test]
fn jalr_basic() {
    use crate::instruction::{ITypeInstruction, ITypeOpcode};
    let mut ctx = JitContext::new();

    ctx.generate_basic_block(
        vec![],
        InstructionInfo::new(
            Instruction::IType(ITypeInstruction {
                imm: 4096,
                rd: 4,
                rs1: 1,
                opcode: ITypeOpcode::JALR,
            }),
            4,
            4,
        ),
    );

    let mut regs = [0xaaaaaaaa; 32];
    regs[0] = 0;
    regs[1] = 1024;
    let mut pc = 4;

    ctx.execute_basic_block(&mut pc, &mut regs);
    assert_eq!(pc, 4096 + 1024);

    for idx in (0..regs.len()) {
        match idx {
            0 => assert_eq!(regs[idx], 0),
            1 => assert_eq!(regs[idx], 1024),
            4 => assert_eq!(regs[idx], 8),
            _ => assert_eq!(regs[idx], 0xaaaaaaaa),
        }
    }
}

#[test]
fn reg0_unwritable_imm() {
    use crate::instruction::{JTypeInstruction, JTypeOpcode};
    let mut ctx = JitContext::new();

    ctx.generate_basic_block(
        vec![],
        InstructionInfo::new(
            Instruction::JType(JTypeInstruction {
                imm: 4096,
                rd: 0,
                opcode: JTypeOpcode::JAL,
            }),
            0,
            4,
        ),
    );

    let mut regs = [0xaaaaaaaa; 32];
    regs[0] = 0;
    let mut pc = 0;

    ctx.execute_basic_block(&mut pc, &mut regs);
    assert_eq!(pc, 4096 + 4);

    assert_eq!(regs[0], 0);

    for idx in (1..regs.len()) {
        assert_eq!(regs[idx], 0xaaaaaaaa);
    }
}
