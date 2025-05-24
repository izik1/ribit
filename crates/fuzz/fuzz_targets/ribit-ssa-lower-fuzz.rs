#![no_main]

use std::hint::black_box;

use libfuzzer_sys::fuzz_target;
use ribit_core::instruction::{self, Instruction};
use ribit_core::opcode;
use ribit_ssa::opt::pass_manager::InplacePass;

const NOP: Instruction =
    Instruction::I(instruction::I { imm: 0, rs1: None, rd: None, opcode: opcode::I::ADDI });

const EBREAK: Instruction = Instruction::Sys(instruction::Sys { opcode: opcode::RSys::EBREAK });

fn parse(mut data: &[u8]) -> (Vec<instruction::Info>, instruction::Info) {
    let mut instructions = Vec::with_capacity(data.len() / 2);

    while let Some((small, rest)) = data.split_at_checked(2) {
        let info = if small[0] & 0b11 == 0b11 {
            let Some((big, rest)) = data.split_at_checked(4) else {
                break;
            };

            data = rest;

            let raw = u32::from_le_bytes(big.try_into().unwrap());
            let instruction = ribit_decode::instruction(raw).unwrap_or_else(|_| NOP);
            instruction::Info { instruction, len: 4 }
        } else {
            data = rest;

            let raw = u16::from_le_bytes(small.try_into().unwrap());
            let instruction =
                ribit_decode::compressed::decode_instruction(raw).unwrap_or_else(|_| NOP);
            instruction::Info { instruction, len: 2 }
        };

        if info.instruction.is_terminator() {
            return (instructions, info);
        }

        instructions.push(info);
        //
    }

    (instructions, instruction::Info { instruction: EBREAK, len: 4 })
}

fuzz_target!(|data: &[u8]| {
    let (instrutctions, term) = parse(data);

    let mut block = black_box(lower(instrutctions, term));

    ribit_ssa::opt::PassManager::optimized().run(&mut block);
    let _ = black_box(block);
});

fn lower(instructions: Vec<instruction::Info>, term: instruction::Info) -> ribit_ssa::Block {
    let mut ctx = ribit_ssa::lower::Context::new(1024, 2_u32.pow(20));

    for instruction::Info { instruction, len } in instructions {
        assert!(!instruction.is_terminator());

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

        ribit_ssa::lower::non_terminal(&mut ctx, instruction, len);
    }

    assert!(term.instruction.is_terminator());

    ribit_ssa::lower::terminal(ctx, term.instruction, term.len)
}
