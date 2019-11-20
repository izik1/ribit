use crate::instruction::Instruction;

use assembler::ExecutableAnonymousMemoryMap;
use std::ops::Range;

mod alloc;
mod generator;

type BasicBlock =
    unsafe extern "sysv64" fn(regs: *mut u32, ctx: &mut JitContext, memory: *mut u8) -> u32;

pub struct InstructionInfo {
    instruction: Instruction,
    start_address: u32,
    len: u32,
}

impl InstructionInfo {
    #[must_use]
    pub fn end_address(&self) -> u32 {
        self.start_address.wrapping_add(self.len)
    }

    #[must_use]
    pub fn new(instruction: Instruction, start_address: u32, len: u32) -> Self {
        Self {
            instruction,
            start_address,
            len,
        }
    }
}

// todo: rename to `RuntimeContext` and create a `BuildContext`

pub struct JitContext {
    buffer: ExecutableAnonymousMemoryMap,
    blocks: Vec<BasicBlock>,
    ranges: Vec<Range<u32>>,
}

impl JitContext {
    // todo: clean
    pub fn execute_basic_block(
        &mut self,
        pc: &mut u32,
        regs: &mut [u32; crate::XLEN],
        memory: &mut [u8],
    ) {
        // assert 16 MiB of memory for now
        assert_eq!(memory.len(), 1024 * 1024 * 16);

        if let Some(block_num) = self.ranges.iter().position(|range| range.start == *pc) {
            let block = &self.blocks[block_num];
            *pc = unsafe { block(regs.as_mut_ptr(), self, memory.as_mut_ptr()) }
        } else {
            todo!("put an error here")
        }
    }

    #[must_use]
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

impl Default for JitContext {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use super::{InstructionInfo, JitContext};
    use crate::{
        instruction::{self, Instruction},
        opcode,
        register::RiscVRegister,
    };

    fn init() -> ([u32; 32], Vec<u8>) {
        let mut regs = [0xaaaaaaaa; 32];
        regs[0] = 0;
        let memory = vec![0xbb; 1024 * 1024 * 16];

        (regs, memory)
    }

    #[test]
    fn jal_basic() {
        let mut ctx = JitContext::new();

        ctx.generate_basic_block(
            vec![],
            InstructionInfo::new(
                Instruction::J(instruction::J {
                    imm: 4096,
                    rd: Some(RiscVRegister::X4),
                    opcode: opcode::J::JAL,
                }),
                0,
                4,
            ),
        );

        let (mut regs, mut memory) = init();

        let mut pc = 0;

        ctx.execute_basic_block(&mut pc, &mut regs, &mut memory);
        assert_eq!(pc, 4096 + 4);

        for idx in 1..regs.len() {
            match idx {
                0 => assert_eq!(regs[idx], 0),
                4 => assert_eq!(regs[idx], 4),
                _ => assert_eq!(regs[idx], 0xaaaaaaaa),
            }
        }
    }

    #[test]
    fn jalr_basic() {
        let mut ctx = JitContext::new();

        ctx.generate_basic_block(
            vec![],
            InstructionInfo::new(
                Instruction::I(instruction::I {
                    imm: 4096,
                    rd: Some(RiscVRegister::X4),
                    rs1: Some(RiscVRegister::X1),
                    opcode: opcode::I::JALR,
                }),
                4,
                4,
            ),
        );

        let (mut regs, mut memory) = init();

        regs[1] = 1024;
        let mut pc = 4;

        ctx.execute_basic_block(&mut pc, &mut regs, &mut memory);

        assert_eq!(pc, 4096 + 1024);

        for idx in 0..regs.len() {
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
        let mut ctx = JitContext::new();

        ctx.generate_basic_block(
            vec![],
            InstructionInfo::new(
                Instruction::J(instruction::J {
                    imm: 4096,
                    rd: None,
                    opcode: opcode::J::JAL,
                }),
                0,
                4,
            ),
        );

        let (mut regs, mut memory) = init();
        let mut pc = 0;

        ctx.execute_basic_block(&mut pc, &mut regs, &mut memory);
        assert_eq!(pc, 4096 + 4);

        for idx in 0..regs.len() {
            match idx {
                0 => assert_eq!(regs[idx], 0),
                _ => assert_eq!(regs[idx], 0xaaaaaaaa),
            }
        }
    }
}
