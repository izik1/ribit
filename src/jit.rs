mod alloc;
mod generator;

pub mod context;

type BasicBlock = unsafe extern "sysv64" fn(
    regs: *mut u32,
    ctx: &mut context::Runtime,
    memory: *mut u8,
) -> BlockReturn;

type CheckRanges = extern "sysv64" fn(pc: u32, ctx: &mut context::Runtime, address: u32) -> bool;

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct BlockReturn(u64);

impl BlockReturn {
    fn into_parts(self) -> (u32, ReturnCode) {
        let address = self.0 as u32;
        let return_code: ReturnCode = unsafe { std::mem::transmute((self.0 >> 32) as u32) };
        (address, return_code)
    }

    fn from_parts(addr: u32, return_code: ReturnCode) -> Self {
        Self((addr as u64) | ((return_code as u64) << 32))
    }

    fn as_u64(self) -> u64 {
        self.0
    }
}

#[repr(u32)]
enum ReturnCode {
    #[allow(dead_code)]
    Normal = 0,
    EBreak,
    ECall,
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
        let memory = vec![0xbb; crate::MEMORY_SIZE as usize];

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
