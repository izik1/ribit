#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    clippy::match_bool
)]

use ribit_core::instruction::{self, Instruction};
use ribit_core::{opcode, register, Width};

pub mod compressed;
mod error;
mod from_instruction;
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

#[allow(clippy::too_many_lines)]
pub fn instruction(instruction: u32) -> Result<Instruction, DecodeError> {
    let funct3 = ((instruction >> 12) & 0b0000_0111) as u8;
    let funct7 = ((instruction >> 25) & 0b0111_1111) as u8;
    let opcode = (instruction & 0b0111_1111) as u8;
    let instruction = match (opcode, funct3, funct7) {
        (0b110_1111, _, _) => Instruction::J(from_instruction::j(instruction, opcode::J::JAL)),

        (0b001_0111, _, _) => Instruction::U(from_instruction::u(instruction, opcode::U::AUIPC)),

        (0b0011_0111, _, _) => Instruction::U(from_instruction::u(instruction, opcode::U::LUI)),

        (0b110_0111, 0b000, _) => {
            Instruction::IJump(from_instruction::ijump(instruction, opcode::IJump::JALR))
        }

        (0b110_0011, _, _) => Instruction::B(decode_branch(instruction)?),
        (0b000_0011, 0b000, _) => {
            Instruction::IMem(from_instruction::imem(instruction, opcode::IMem::LD(Width::Byte)))
        }
        (0b000_0011, 0b001, _) => {
            Instruction::IMem(from_instruction::imem(instruction, opcode::IMem::LD(Width::Word)))
        }
        (0b000_0011, 0b010, _) => {
            Instruction::IMem(from_instruction::imem(instruction, opcode::IMem::LD(Width::DWord)))
        }
        (0b000_0011, 0b100, _) => {
            Instruction::IMem(from_instruction::imem(instruction, opcode::IMem::LDU(Width::Byte)))
        }
        (0b000_0011, 0b101, _) => {
            Instruction::IMem(from_instruction::imem(instruction, opcode::IMem::LDU(Width::Word)))
        }
        (0b010_0011, 0b000, _) => Instruction::S(from_instruction::s(instruction, Width::Byte)),
        (0b010_0011, 0b001, _) => Instruction::S(from_instruction::s(instruction, Width::Word)),
        (0b010_0011, 0b010, _) => Instruction::S(from_instruction::s(instruction, Width::DWord)),
        (0b001_0011, 0b000, _) => Instruction::I(from_instruction::i(instruction, opcode::I::ADDI)),
        (0b001_0011, 0b010, _) => {
            Instruction::I(from_instruction::i(instruction, opcode::I::SICond(opcode::Cmp::Lt)))
        }
        (0b001_0011, 0b011, _) => {
            Instruction::I(from_instruction::i(instruction, opcode::I::SICond(opcode::Cmp::Ltu)))
        }
        (0b001_0011, 0b100, _) => Instruction::I(from_instruction::i(instruction, opcode::I::XORI)),
        (0b001_0011, 0b110, _) => Instruction::I(from_instruction::i(instruction, opcode::I::ORI)),
        (0b001_0011, 0b111, _) => Instruction::I(from_instruction::i(instruction, opcode::I::ANDI)),
        (0b001_0011, 0b001, 0b000_0000) => {
            Instruction::I(from_instruction::i(instruction, opcode::I::SLLI))
        }
        (0b001_0011, 0b101, 0b000_0000) => {
            Instruction::I(from_instruction::i(instruction, opcode::I::SRLI))
        }
        (0b001_0011, 0b101, 0b010_0000) => {
            Instruction::I(from_instruction::i(instruction, opcode::I::SRAI))
        }
        (0b011_0011, 0b000, 0b000_0000) => {
            Instruction::R(from_instruction::r(instruction, opcode::R::ADD))
        }
        (0b011_0011, 0b000, 0b010_0000) => {
            Instruction::R(from_instruction::r(instruction, opcode::R::SUB))
        }
        (0b011_0011, 0b001, 0b000_0000) => {
            Instruction::R(from_instruction::r(instruction, opcode::R::SLL))
        }
        (0b011_0011, 0b010, 0b000_0000) => Instruction::R(from_instruction::r(
            instruction,
            // SLT
            opcode::R::SCond(opcode::Cmp::Lt),
        )),
        (0b011_0011, 0b011, 0b000_0000) => Instruction::R(from_instruction::r(
            instruction,
            // SLTU
            opcode::R::SCond(opcode::Cmp::Ltu),
        )),
        (0b011_0011, 0b100, 0b000_0000) => {
            Instruction::R(from_instruction::r(instruction, opcode::R::XOR))
        }
        (0b011_0011, 0b101, 0b000_0000) => {
            Instruction::R(from_instruction::r(instruction, opcode::R::SRL))
        }
        (0b011_0011, 0b101, 0b010_0000) => {
            Instruction::R(from_instruction::r(instruction, opcode::R::SRA))
        }
        (0b011_0011, 0b110, 0b000_0000) => {
            Instruction::R(from_instruction::r(instruction, opcode::R::OR))
        }
        (0b011_0011, 0b111, 0b000_0000) => {
            Instruction::R(from_instruction::r(instruction, opcode::R::AND))
        }
        (0b000_1111, 0b000, _) => {
            Instruction::IMem(from_instruction::imem(instruction, opcode::IMem::FENCE))
        }
        (0b111_0011, _, _) if instruction & !0b111_0011 == 0 => {
            Instruction::Sys(instruction::Sys::new(opcode::RSys::ECALL))
        }
        (0b111_0011, _, _) if instruction & !0b111_0011 == (1 << 20) => {
            Instruction::Sys(instruction::Sys::new(opcode::RSys::EBREAK))
        }
        (0b011_0011, 0b000, _) => Instruction::R(from_instruction::r(instruction, opcode::R::MUL)),
        (0b011_0011, 0b001, _) => Instruction::R(from_instruction::r(instruction, opcode::R::MULH)),
        (0b011_0011, 0b010, _) => {
            Instruction::R(from_instruction::r(instruction, opcode::R::MULHSU))
        }
        (0b011_0011, 0b011, _) => {
            Instruction::R(from_instruction::r(instruction, opcode::R::MULHU))
        }
        (0b011_0011, 0b100, _) => Instruction::R(from_instruction::r(instruction, opcode::R::DIV)),
        (0b011_0011, 0b101, _) => Instruction::R(from_instruction::r(instruction, opcode::R::DIVU)),
        (0b011_0011, 0b110, _) => Instruction::R(from_instruction::r(instruction, opcode::R::REM)),
        (0b011_0011, 0b111, _) => Instruction::R(from_instruction::r(instruction, opcode::R::REMU)),
        _ => return Err(DecodeError::InvalidInstruction(instruction)),
    };

    Ok(instruction)
}

fn decode_branch(instruction: u32) -> Result<instruction::B, DecodeError> {
    let imm = (((instruction >> 19) & 0b0001_0000_0000_0000)
        | ((instruction >> 20) & 0b0000_0111_1110_0000)
        | ((instruction << 4) & 0b0000_1000_0000_0000)
        | ((instruction >> 7) & 0b0000_0000_0001_1110)) as u16;
    let imm = sign_extend(imm, 13);

    let (rs1, rs2) = decode_rs(instruction);
    let cmp_mode = match ((instruction >> 12) & 0b111) as u8 {
        0b000 => opcode::Cmp::Eq,
        0b001 => opcode::Cmp::Ne,
        0b100 => opcode::Cmp::Lt,
        0b101 => opcode::Cmp::Ge,
        0b110 => opcode::Cmp::Ltu,
        0b111 => opcode::Cmp::Geu,
        0b010 | 0b011 => return Err(DecodeError::InvalidInstruction(instruction)),
        0x08..=0xff => unreachable!(),
    };

    Ok(instruction::B::new(imm, rs1, rs2, cmp_mode))
}

#[cfg(test)]
mod test {
    use ribit_core::instruction::Instruction;

    use crate::opcode;

    #[test]
    fn decode_ebreak() {
        let instruction = crate::instruction(0b0000_0000_0001_0000_0000_0000_0111_0011).unwrap();
        if let Instruction::Sys(instruction) = instruction {
            assert_eq!(instruction.opcode, opcode::RSys::EBREAK)
        } else {
            panic!("Instruction type wasn't RType!")
        }
    }
}
