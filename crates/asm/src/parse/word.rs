use std::str::FromStr;

use ribit_core::instruction::{Instruction, InstructionKind};
use ribit_core::opcode::{Cmp, SCmp};
use ribit_core::{Width, opcode};

#[non_exhaustive]
pub(crate) struct ParseWordError;

pub(crate) struct Opcode;

impl InstructionKind for Opcode {
    type R = opcode::R;
    type I = opcode::I;
    type IJump = opcode::IJump;
    type IMem = opcode::IMem;
    type S = Width;
    type B = opcode::Cmp;
    type U = opcode::U;
    type J = opcode::J;
    type Sys = opcode::RSys;
}

pub(crate) enum Word {
    Op(Instruction<Opcode>),
    Ret,
    Nop,
    Li,
    J,
}

macro_rules! wrap {
    ($($kind:ident => $f:ident),* $(,)?) => {
        $(
            const fn $f(opcode: <Opcode as InstructionKind>::$kind) -> Word {
                Word::Op(Instruction::$kind(opcode))
            }
        )*
    };
}

wrap!(R => r, I => i, IJump => ijump, IMem => imem, S => s, B => b, U => u, J => j, Sys => sys);

impl FromStr for Word {
    type Err = ParseWordError;

    fn from_str(str: &str) -> Result<Self, Self::Err> {
        let str = str.as_bytes();

        let res = hashify2::tiny_map_ignore_case! {
            str,
            "add" => r(opcode::R::ADD),
            "sub" => r(opcode::R::SUB),
            "sll" => r(opcode::R::SLL),
            "slt" => r(opcode::R::SCond(SCmp::Lt)),
            "sltu" => r(opcode::R::SCond(SCmp::Ltu)),
            "xor" => r(opcode::R::XOR),
            "srl" => r(opcode::R::SRL),
            "sra" => r(opcode::R::SRA),
            "or" => r(opcode::R::OR),
            "and" => r(opcode::R::AND),
            "mul" => r(opcode::R::MUL),
            "mulh" => r(opcode::R::MULH),
            "mulhsu" => r(opcode::R::MULHSU),
            "mulhu" => r(opcode::R::MULHU),
            "div" => r(opcode::R::DIV),
            "divu" => r(opcode::R::DIVU),
            "rem" => r(opcode::R::REM),
            "remu" => r(opcode::R::REMU),

            "addi" => i(opcode::I::ADDI),
            "slti" => i(opcode::I::SICond(SCmp::Lt)),
            "sltiu" => i(opcode::I::SICond(SCmp::Ltu)),
            "xori" => i(opcode::I::XORI),
            "ori" => i(opcode::I::ORI),
            "andi" => i(opcode::I::ANDI),
            "slli" => i(opcode::I::SLLI),
            "srli" => i(opcode::I::SRLI),
            "srai" => i(opcode::I::SRAI),

            "jalr" => ijump(opcode::IJump::JALR),

            "fence" => imem(opcode::IMem::FENCE),
            "lb" => imem(opcode::IMem::LD(Width::Byte)),
            "lh" => imem(opcode::IMem::LD(Width::Word)),
            "lw" => imem(opcode::IMem::LD(Width::DWord)),
            "lbu" => imem(opcode::IMem::LDU(Width::Byte)),
            "lhu" => imem(opcode::IMem::LDU(Width::Word)),

            "sb" => s(Width::Byte),
            "sh" => s(Width::Word),
            "sw" => s(Width::DWord),

            "beq" => b(Cmp::Eq),
            "bne" => b(Cmp::Ne),
            "blt" => b(Cmp::Lt),
            "bltu" => b(Cmp::Ltu),
            "bge" => b(Cmp::Ge),
            "bgeu" => b(Cmp::Geu),

            "lui" => u(opcode::U::LUI),
            "auipc" => u(opcode::U::AUIPC),
            "jal" => j(opcode::J::JAL),
            "ebreak" => sys(opcode::RSys::EBREAK),
            "ecall" => sys(opcode::RSys::ECALL),

            "ret" => Word::Ret,
            "nop" => Word::Nop,
            "li" => Word::Li,
            "j" => Word::J,
        };

        res.ok_or(ParseWordError)
    }
}
