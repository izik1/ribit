mod generator;

mod legalise;
mod register_alloc;
pub mod rt;

use ribit_core::ReturnCode;

type Assembler<'a, 'b> = rasen::Assembler<'a, std::io::Cursor<&'b mut [u8]>>;

type BasicBlockFunc = unsafe extern "sysv64" fn(regs: *mut u32, memory: *mut u8) -> BlockReturn;

// this is an offset + length pair into the `Buffer` for `BasicBlockFunc`s.
pub struct Block {
    offset: usize,
    length: usize,
}

#[repr(transparent)]
#[derive(Copy, Clone)]
struct BlockReturn(u64);

impl BlockReturn {
    fn into_parts(self) -> (u32, ReturnCode) {
        let address = self.0 as u32;
        let return_code: ReturnCode = unsafe { std::mem::transmute((self.0 >> 32) as u32) };
        (address, return_code)
    }

    fn from_parts(addr: u32, return_code: ReturnCode) -> Self {
        Self(u64::from(addr) | ((return_code as u64) << 32))
    }

    fn as_u64(self) -> u64 {
        self.0
    }
}

#[cfg(all(test, target_arch = "x86_64"))]
mod test {
    use ribit_core::instruction::Instruction;
    use ribit_core::{instruction, opcode, register};

    use crate::AMD64Runtime;

    fn init() -> (register::File<u32>, Vec<u8>) {
        let regs = register::File([0xaaaa_aaaa; 31]);
        let memory = vec![0xbb; crate::MEMORY_SIZE as usize];

        (regs, memory)
    }

    #[test]
    fn jal_basic() {
        let mut ctx = AMD64Runtime::new();

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

        ctx.execute_basic_block(&mut pc, &mut regs, &mut memory, |_, _| Err(())).unwrap();
        assert_eq!(pc, 4096);

        for (idx, &reg) in regs.0.iter().enumerate() {
            let idx = idx + 1;
            match idx {
                0 => unreachable!(),
                4 => assert_eq!(reg, 4, "reg-num: {idx}"),
                _ => assert_eq!(reg, 0xaaaa_aaaa, "reg-num: {idx}"),
            }
        }
    }

    #[test]
    fn jalr_basic() {
        let mut ctx = AMD64Runtime::new();

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

        regs[register::RiscV::X1] = 1024;
        let mut pc = 48;

        ctx.execute_basic_block(&mut pc, &mut regs, &mut memory, |_, _| Err(())).unwrap();

        assert_eq!(pc, 2046 + 1024);

        for (idx, &reg) in regs.0.iter().enumerate() {
            let idx = idx + 1;
            match idx {
                0 => unreachable!(),
                1 => assert_eq!(reg, 1024, "reg-num={idx}"),
                4 => assert_eq!(reg, 52, "reg-num={idx}"),
                _ => assert_eq!(reg, 0xaaaa_aaaa, "reg-num={idx}"),
            }
        }
    }

    #[test]
    fn reg0_unwritable_imm() {
        let mut ctx = AMD64Runtime::new();

        ctx.generate_basic_block(
            vec![],
            instruction::Info::new(
                Instruction::J(instruction::J { imm: 4096, rd: None, opcode: opcode::J::JAL }),
                4,
            ),
            0,
            4,
        );

        let (mut regs, mut memory) = init();
        let mut pc = 0;

        ctx.execute_basic_block(&mut pc, &mut regs, &mut memory, |_, _| Err(())).unwrap();
        assert_eq!(pc, 4096);

        for (idx, &reg) in regs.0.iter().enumerate() {
            let idx = idx + 1;
            match idx {
                0 => unreachable!(),
                _ => assert_eq!(reg, 0xaaaa_aaaa),
            }
        }
    }
}
