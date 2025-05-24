#![no_main]

use core::fmt;
use std::hint::black_box;

use libfuzzer_sys::{arbitrary, fuzz_target};
use ribit_core::instruction::{self, Instruction};
use ribit_core::opcode;
use ribit_ssa::opt::pass_manager::InplacePass;

struct ArbitraryInstruction(instruction::Info);

impl fmt::Debug for ArbitraryInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, r#""{}""#, ribit_core::disassemble::FmtInstruction::from_info(&self.0))
    }
}

impl<'a> arbitrary::Arbitrary<'a> for ArbitraryInstruction {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let raw = u32::arbitrary(u)?;
        let instruction = ribit_decode::instruction(raw).unwrap_or_else(|_| {
            Instruction::I(instruction::I { imm: 0, rs1: None, rd: None, opcode: opcode::I::ADDI })
        });

        Ok(Self(instruction::Info { instruction, len: 4 }))
    }

    fn arbitrary_take_rest(mut u: arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Self::arbitrary(&mut u)
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        u32::size_hint(depth)
    }
}

fuzz_target!(|data: Vec<ArbitraryInstruction>| {
    let block = black_box(lower(data));

    if let Some(mut block) = block {
        ribit_ssa::opt::PassManager::optimized().run(&mut block);
        let _ = black_box(block);
    }
});

fn lower(instructions: Vec<ArbitraryInstruction>) -> Option<ribit_ssa::Block> {
    let mut ctx = ribit_ssa::lower::Context::new(1024, 2_u32.pow(20));

    for ArbitraryInstruction(instruction::Info { instruction, len }) in instructions {
        if instruction.is_terminator() {
            return Some(ribit_ssa::lower::terminal(ctx, instruction, len));
        }

        match instruction {
            Instruction::R(instruction::R {
                rs1: _,
                rs2: _,
                rd: _,
                opcode:
                    opcode::R::MUL
                    | opcode::R::MULH
                    | opcode::R::MULHSU
                    | opcode::R::MULHU
                    | opcode::R::DIV
                    | opcode::R::DIVU
                    | opcode::R::REM
                    | opcode::R::REMU,
            }) => continue,
            _ => {}
        }

        ribit_ssa::lower::non_terminal(&mut ctx, instruction, 4);
    }

    None
}
