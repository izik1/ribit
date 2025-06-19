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
    use core::fmt;

    #[derive(Eq, PartialEq)]
    struct LowerHex<T: fmt::LowerHex>(T);

    impl<T: fmt::LowerHex> fmt::Debug for LowerHex<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:#0width$x}", self.0, width = std::mem::size_of::<T>() * 2 + 2)
        }
    }

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
        assert_eq!(LowerHex(pc), LowerHex(4096));

        let regs = register::File(regs.0.map(LowerHex));

        expect_test::expect![[r#"
            {
                X1: 0xaaaaaaaa,
                X2: 0xaaaaaaaa,
                X3: 0xaaaaaaaa,
                X4: 0x00000004,
                X5: 0xaaaaaaaa,
                X6: 0xaaaaaaaa,
                X7: 0xaaaaaaaa,
                X8: 0xaaaaaaaa,
                X9: 0xaaaaaaaa,
                X10: 0xaaaaaaaa,
                X11: 0xaaaaaaaa,
                X12: 0xaaaaaaaa,
                X13: 0xaaaaaaaa,
                X14: 0xaaaaaaaa,
                X15: 0xaaaaaaaa,
                X16: 0xaaaaaaaa,
                X17: 0xaaaaaaaa,
                X18: 0xaaaaaaaa,
                X19: 0xaaaaaaaa,
                X20: 0xaaaaaaaa,
                X21: 0xaaaaaaaa,
                X22: 0xaaaaaaaa,
                X23: 0xaaaaaaaa,
                X24: 0xaaaaaaaa,
                X25: 0xaaaaaaaa,
                X26: 0xaaaaaaaa,
                X27: 0xaaaaaaaa,
                X28: 0xaaaaaaaa,
                X29: 0xaaaaaaaa,
                X30: 0xaaaaaaaa,
                X31: 0xaaaaaaaa,
            }
        "#]]
        .assert_debug_eq(&regs);
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

        assert_eq!(LowerHex(pc), LowerHex(2046 + 1024));

        let regs = register::File(regs.0.map(LowerHex));

        expect_test::expect![[r#"
            {
                X1: 0x00000400,
                X2: 0xaaaaaaaa,
                X3: 0xaaaaaaaa,
                X4: 0x00000034,
                X5: 0xaaaaaaaa,
                X6: 0xaaaaaaaa,
                X7: 0xaaaaaaaa,
                X8: 0xaaaaaaaa,
                X9: 0xaaaaaaaa,
                X10: 0xaaaaaaaa,
                X11: 0xaaaaaaaa,
                X12: 0xaaaaaaaa,
                X13: 0xaaaaaaaa,
                X14: 0xaaaaaaaa,
                X15: 0xaaaaaaaa,
                X16: 0xaaaaaaaa,
                X17: 0xaaaaaaaa,
                X18: 0xaaaaaaaa,
                X19: 0xaaaaaaaa,
                X20: 0xaaaaaaaa,
                X21: 0xaaaaaaaa,
                X22: 0xaaaaaaaa,
                X23: 0xaaaaaaaa,
                X24: 0xaaaaaaaa,
                X25: 0xaaaaaaaa,
                X26: 0xaaaaaaaa,
                X27: 0xaaaaaaaa,
                X28: 0xaaaaaaaa,
                X29: 0xaaaaaaaa,
                X30: 0xaaaaaaaa,
                X31: 0xaaaaaaaa,
            }
        "#]]
        .assert_debug_eq(&regs);
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

        assert_eq!(LowerHex(pc), LowerHex(4096));

        let regs = register::File(regs.0.map(LowerHex));

        expect_test::expect![[r#"
            {
                X1: 0xaaaaaaaa,
                X2: 0xaaaaaaaa,
                X3: 0xaaaaaaaa,
                X4: 0xaaaaaaaa,
                X5: 0xaaaaaaaa,
                X6: 0xaaaaaaaa,
                X7: 0xaaaaaaaa,
                X8: 0xaaaaaaaa,
                X9: 0xaaaaaaaa,
                X10: 0xaaaaaaaa,
                X11: 0xaaaaaaaa,
                X12: 0xaaaaaaaa,
                X13: 0xaaaaaaaa,
                X14: 0xaaaaaaaa,
                X15: 0xaaaaaaaa,
                X16: 0xaaaaaaaa,
                X17: 0xaaaaaaaa,
                X18: 0xaaaaaaaa,
                X19: 0xaaaaaaaa,
                X20: 0xaaaaaaaa,
                X21: 0xaaaaaaaa,
                X22: 0xaaaaaaaa,
                X23: 0xaaaaaaaa,
                X24: 0xaaaaaaaa,
                X25: 0xaaaaaaaa,
                X26: 0xaaaaaaaa,
                X27: 0xaaaaaaaa,
                X28: 0xaaaaaaaa,
                X29: 0xaaaaaaaa,
                X30: 0xaaaaaaaa,
                X31: 0xaaaaaaaa,
            }
        "#]].assert_debug_eq(&regs);
    }
}
