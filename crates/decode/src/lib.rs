#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    clippy::match_bool
)]
#![deny(unsafe_op_in_unsafe_fn)]
#![no_std]

use ribit_core::instruction::{self, Instruction, InstructionKind};
use ribit_core::{Width, opcode, register};

pub mod compressed;
mod error;
mod imm;
pub use error::{CompressedDecodeError, DecodeError, Extension};

#[inline]
const fn decode_register(instruction: u32) -> Option<register::RiscV> {
    register::RiscV::with_u8((instruction as u8) & 0b1_1111)
}

#[must_use]
fn decode_rs(instruction: u32) -> (Option<register::RiscV>, Option<register::RiscV>) {
    let rs1 = decode_register(instruction >> 15);
    let rs2 = decode_register(instruction >> 20);
    (rs1, rs2)
}

#[must_use]
fn decode_rd(instruction: u32) -> Option<register::RiscV> {
    decode_register(instruction >> 7)
}

#[must_use]
const fn sign_extend(value: u16, data_bits: u8) -> u16 {
    let mask = 16 - data_bits;
    (((value << mask) as i16) >> mask) as u16
}

#[must_use]
const fn sign_extend_32(value: u32, data_bits: u8) -> u32 {
    let mask = 32 - data_bits;
    (((value << mask) as i32) >> mask) as u32
}

pub fn instruction(instruction: u32) -> Result<Instruction, DecodeError> {
    struct Opcode;

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

    let funct3 = ((instruction >> 12) & 0b0000_0111) as u8;
    let funct7 = ((instruction >> 25) & 0b0111_1111) as u8;
    let opcode = (instruction & 0b0111_1111) as u8;

    let tmp: Instruction<Opcode> = match (opcode, funct3, funct7) {
        (0b110_1111, _, _) => Instruction::J(opcode::J::JAL),

        (0b001_0111, _, _) => Instruction::U(opcode::U::AUIPC),

        (0b0011_0111, _, _) => Instruction::U(opcode::U::LUI),

        (0b110_0111, 0b000, _) => Instruction::IJump(opcode::IJump::JALR),

        (0b110_0011, 0b000, _) => Instruction::B(opcode::Cmp::Eq),
        (0b110_0011, 0b001, _) => Instruction::B(opcode::Cmp::Ne),
        (0b110_0011, 0b100, _) => Instruction::B(opcode::Cmp::Lt),
        (0b110_0011, 0b101, _) => Instruction::B(opcode::Cmp::Ge),
        (0b110_0011, 0b110, _) => Instruction::B(opcode::Cmp::Ltu),
        (0b110_0011, 0b111, _) => Instruction::B(opcode::Cmp::Geu),

        (0b000_0011, 0b000, _) => Instruction::IMem(opcode::IMem::LD(Width::Byte)),
        (0b000_0011, 0b001, _) => Instruction::IMem(opcode::IMem::LD(Width::Word)),
        (0b000_0011, 0b010, _) => Instruction::IMem(opcode::IMem::LD(Width::DWord)),
        (0b000_0011, 0b100, _) => Instruction::IMem(opcode::IMem::LDU(Width::Byte)),
        (0b000_0011, 0b101, _) => Instruction::IMem(opcode::IMem::LDU(Width::Word)),
        (0b000_1111, 0b000, _) => Instruction::IMem(opcode::IMem::FENCE),
        (0b010_0011, 0b000, _) => Instruction::S(Width::Byte),
        (0b010_0011, 0b001, _) => Instruction::S(Width::Word),
        (0b010_0011, 0b010, _) => Instruction::S(Width::DWord),
        (0b001_0011, 0b000, _) => Instruction::I(opcode::I::ADDI),
        (0b001_0011, 0b010, _) => Instruction::I(opcode::I::SICond(opcode::SCmp::Lt)),
        (0b001_0011, 0b011, _) => Instruction::I(opcode::I::SICond(opcode::SCmp::Ltu)),
        (0b001_0011, 0b100, _) => Instruction::I(opcode::I::XORI),
        (0b001_0011, 0b110, _) => Instruction::I(opcode::I::ORI),
        (0b001_0011, 0b111, _) => Instruction::I(opcode::I::ANDI),
        (0b001_0011, 0b001, 0b000_0000) => Instruction::I(opcode::I::SLLI),
        (0b001_0011, 0b101, 0b000_0000) => Instruction::I(opcode::I::SRLI),
        (0b001_0011, 0b101, 0b010_0000) => Instruction::I(opcode::I::SRAI),
        (0b011_0011, 0b000, 0b000_0000) => Instruction::R(opcode::R::ADD),
        (0b011_0011, 0b000, 0b010_0000) => Instruction::R(opcode::R::SUB),
        (0b011_0011, 0b001, 0b000_0000) => Instruction::R(opcode::R::SLL),
        (0b011_0011, 0b010, 0b000_0000) => Instruction::R(opcode::R::SCond(opcode::SCmp::Lt)),
        (0b011_0011, 0b011, 0b000_0000) => Instruction::R(opcode::R::SCond(opcode::SCmp::Ltu)),
        (0b011_0011, 0b100, 0b000_0000) => Instruction::R(opcode::R::XOR),
        (0b011_0011, 0b101, 0b000_0000) => Instruction::R(opcode::R::SRL),
        (0b011_0011, 0b101, 0b010_0000) => Instruction::R(opcode::R::SRA),
        (0b011_0011, 0b110, 0b000_0000) => Instruction::R(opcode::R::OR),
        (0b011_0011, 0b111, 0b000_0000) => Instruction::R(opcode::R::AND),
        (0b111_0011, _, _) if instruction & !0b111_0011 == 0 => {
            Instruction::Sys(opcode::RSys::ECALL)
        }
        (0b111_0011, _, _) if instruction & !0b111_0011 == (1 << 20) => {
            Instruction::Sys(opcode::RSys::EBREAK)
        }
        (0b011_0011, 0b000, 0b000_0001) => Instruction::R(opcode::R::MUL),
        (0b011_0011, 0b001, 0b000_0001) => Instruction::R(opcode::R::MULH),
        (0b011_0011, 0b010, 0b000_0001) => Instruction::R(opcode::R::MULHSU),
        (0b011_0011, 0b011, 0b000_0001) => Instruction::R(opcode::R::MULHU),
        (0b011_0011, 0b100, 0b000_0001) => Instruction::R(opcode::R::DIV),
        (0b011_0011, 0b101, 0b000_0001) => Instruction::R(opcode::R::DIVU),
        (0b011_0011, 0b110, 0b000_0001) => Instruction::R(opcode::R::REM),
        (0b011_0011, 0b111, 0b000_0001) => Instruction::R(opcode::R::REMU),
        _ => return Err(DecodeError::InvalidInstruction(instruction)),
    };

    let (rs1, rs2) = decode_rs(instruction);
    let rd = decode_rd(instruction);

    Ok(match tmp {
        Instruction::R(opcode) => instruction::R::new(rs1, rs2, rd, opcode).into(),
        Instruction::I(opcode) => instruction::I::new(imm::i(instruction), rs1, rd, opcode).into(),
        Instruction::IJump(opcode) => {
            instruction::IJump::new(imm::i(instruction), rs1, rd, opcode).into()
        }
        Instruction::IMem(opcode) => {
            instruction::IMem::new(imm::i(instruction), rs1, rd, opcode).into()
        }
        Instruction::S(width) => instruction::S::new(imm::s(instruction), rs1, rs2, width).into(),
        Instruction::B(cmp) => instruction::B::new(imm::b(instruction), rs1, rs2, cmp).into(),
        Instruction::U(opcode) => instruction::U::new(instruction & 0xffff_f000, rd, opcode).into(),
        Instruction::J(opcode) => instruction::J::new(imm::j(instruction), rd, opcode).into(),
        Instruction::Sys(opcode) => instruction::Sys::new(opcode).into(),
    })
}

#[cfg(test)]
mod test {
    use ribit_core::instruction::Instruction;

    use crate::opcode;

    #[test]
    fn decode_ebreak() {
        let instruction = crate::instruction(0b0000_0000_0001_0000_0000_0000_0111_0011).unwrap();
        if let Instruction::Sys(instruction) = instruction {
            assert_eq!(instruction.opcode, opcode::RSys::EBREAK);
        } else {
            panic!("Instruction type wasn't RType!")
        }
    }
}
