use crate::instruction;

mod alloc;
mod generator;

pub mod context;

type BasicBlock =
    unsafe extern "sysv64" fn(regs: *mut u32, ctx: &mut context::Runtime, memory: *mut u8) -> u32;

type CheckRanges = extern "sysv64" fn(pc: u32, ctx: &mut context::Runtime, address: u32) -> bool;

struct BlockBuilder<'a> {
    stream: assembler::InstructionStream<'a>,
    register_manager: alloc::RegisterManager,
    check_ranges: CheckRanges,
}

impl<'a> BlockBuilder<'a> {
    fn start(stream: assembler::InstructionStream<'a>, check_ranges: CheckRanges) -> Self {
        Self {
            stream,
            register_manager: alloc::RegisterManager::new(),
            check_ranges,
        }
    }

    fn make_instruction(&mut self, instruction: instruction::Info) {
        generator::generate_instruction(self, instruction);
    }

    fn complete(mut self, branch_instruction: instruction::Info) -> BasicBlock {
        generator::end_basic_block(&mut self, branch_instruction);

        // deconstruct self to avoid drop panic.
        let BlockBuilder {
            stream,
            register_manager,
            ..
        } = self;

        assert!(register_manager.is_cleared());

        let funct: BasicBlock = unsafe { std::mem::transmute(stream.start_instruction_pointer()) };
        stream.finish();

        funct
    }
}

#[cfg(test)]
mod test {
    use super::context;
    use crate::{
        instruction::{self, Instruction},
        opcode, register,
    };

    fn init() -> ([u32; 32], Vec<u8>) {
        let mut regs = [0xaaaaaaaa; 32];
        regs[0] = 0;
        let memory = vec![0xbb; 1024 * 1024 * 16];

        (regs, memory)
    }

    #[test]
    fn jal_basic() {
        let mut ctx = context::Runtime::new();

        ctx.generate_basic_block(
            vec![],
            instruction::Info::new(
                Instruction::J(instruction::J {
                    imm: 4096,
                    rd: Some(register::RiscV::X4),
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
        let mut ctx = context::Runtime::new();

        ctx.generate_basic_block(
            vec![],
            instruction::Info::new(
                Instruction::I(instruction::I {
                    imm: 4096,
                    rd: Some(register::RiscV::X4),
                    rs1: Some(register::RiscV::X1),
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
        let mut ctx = context::Runtime::new();

        ctx.generate_basic_block(
            vec![],
            instruction::Info::new(
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
