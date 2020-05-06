use crate::{ssa, ReturnCode};
use rasen::params::Register;
use std::collections::HashMap;

// going to delete all of this once the ssa can handle it,
// so don't worry about the dead code.
#[allow(dead_code)]
mod alloc;
mod generator;

pub mod context;
pub mod legalise;
pub mod register_alloc;

type Assembler<'a, 'b> = rasen::Assembler<'a, std::io::Cursor<&'b mut [u8]>>;

type BasicBlock = unsafe extern "sysv64" fn(
    regs: *mut u32,
    memory: *mut u8,
    ctx: &mut context::Runtime,
) -> BlockReturn;

type CheckRanges = extern "sysv64" fn(pc: u32, ctx: &mut context::Runtime, address: u32) -> bool;

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct BlockReturn(u64);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Source {
    Val(u32),
    Register(Register),
}

impl Source {
    pub fn from_ssa_src(src: ssa::Source, map: &HashMap<ssa::Id, Register>) -> Option<Self> {
        match src {
            ssa::Source::Val(v) => Some(Self::Val(v)),
            ssa::Source::Id(id) => map.get(&id).copied().map(Self::Register),
        }
    }

    pub fn val(self) -> Option<u32> {
        match self {
            Self::Val(v) => Some(v),
            _ => None,
        }
    }

    pub fn reg(self) -> Option<Register> {
        match self {
            Self::Register(reg) => Some(reg),
            _ => None,
        }
    }

    #[inline]
    pub fn is_reg(self) -> bool {
        self.reg().is_some()
    }

    #[inline]
    pub fn is_val(self) -> bool {
        self.val().is_some()
    }
}

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
                4,
            ),
            0,
            4,
        );

        let (mut regs, mut memory) = init();

        let mut pc = 0;

        ctx.execute_basic_block(&mut pc, &mut regs, &mut memory);
        assert_eq!(pc, 4096);

        for idx in 1..regs.len() {
            match idx {
                0 => assert_eq!(regs[idx], 0, "reg-num: {}", idx),
                4 => assert_eq!(regs[idx], 4, "reg-num: {}", idx),
                _ => assert_eq!(regs[idx], 0xaaaaaaaa, "reg-num: {}", idx),
            }
        }
    }

    #[test]
    fn jalr_basic() {
        let mut ctx = context::Runtime::new();

        ctx.generate_basic_block(
            vec![],
            instruction::Info::new(
                Instruction::IJump(instruction::IJump {
                    imm: 2047,
                    rd: Some(register::RiscV::X4),
                    rs1: Some(register::RiscV::X1),
                    opcode: opcode::IJump::JALR,
                }),
                4,
            ),
            48,
            52,
        );

        let (mut regs, mut memory) = init();

        regs[1] = 1024;
        let mut pc = 48;

        ctx.execute_basic_block(&mut pc, &mut regs, &mut memory);

        assert_eq!(pc, 2046 + 1024);

        for idx in 0..regs.len() {
            match idx {
                0 => assert_eq!(regs[idx], 0, "regnum={}", idx),
                1 => assert_eq!(regs[idx], 1024, "regnum={}", idx),
                4 => assert_eq!(regs[idx], 52, "regnum={}", idx),
                _ => assert_eq!(regs[idx], 0xaaaaaaaa, "regnum={}", idx),
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
                4,
            ),
            0,
            4,
        );

        let (mut regs, mut memory) = init();
        let mut pc = 0;

        ctx.execute_basic_block(&mut pc, &mut regs, &mut memory);
        assert_eq!(pc, 4096);

        for idx in 0..regs.len() {
            match idx {
                0 => assert_eq!(regs[idx], 0),
                _ => assert_eq!(regs[idx], 0xaaaaaaaa),
            }
        }
    }
}
