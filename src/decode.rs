pub mod compressed;

use crate::DecodeError;

use std::num::NonZeroU8;

use crate::instruction::{self, Instruction};

use crate::register;

use crate::opcode::{self, Width};

#[inline]
fn decode_register(instruction: u32) -> Option<register::RiscV> {
    NonZeroU8::new((instruction as u8) & 0b1_1111).and_then(register::RiscV::new)
}

#[must_use]
pub fn decode_rs(instruction: u32) -> (Option<register::RiscV>, Option<register::RiscV>) {
    let rs1 = decode_register(instruction >> 15);
    let rs2 = decode_register(instruction >> 24);
    (rs1, rs2)
}

#[must_use]
pub fn decode_rd(instruction: u32) -> Option<register::RiscV> {
    decode_register(instruction >> 7)
}

#[must_use]
pub const fn sign_extend(value: u16, data_bits: u8) -> u16 {
    let mask = 16 - data_bits;
    (((value << mask) as i16) >> mask) as u16
}

#[must_use]
pub const fn sign_extend_32(value: u32, data_bits: u8) -> u32 {
    let mask = 32 - data_bits;
    (((value << mask) as i32) >> mask) as u32
}

#[allow(clippy::too_many_lines)]
pub fn decode_instruction(instruction: u32) -> Result<Instruction, DecodeError> {
    let funct3 = ((instruction >> 12) & 0b0000_0111) as u8;
    let funct7 = ((instruction >> 25) & 0b0111_1111) as u8;
    let opcode = (instruction & 0b0111_1111) as u8;
    let instruction = match (opcode, funct3, funct7) {
        // todo: sign extend imm
        (0b110_1111, _, _) => Instruction::J(instruction::J::from_instruction(
            instruction,
            opcode::J::JAL,
        )),

        // todo: sign extend & set lsb to 0
        (0b110_0111, 0b000, _) => Instruction::I(instruction::I::from_instruction(
            instruction,
            opcode::I::JALR,
        )),

        (0b110_0011, _, _) => Instruction::B(decode_branch(instruction)?),
        (0b000_0011, 0b000, _) => {
            Instruction::I(instruction::I::from_instruction(instruction, opcode::I::LB))
        }
        (0b000_0011, 0b001, _) => {
            Instruction::I(instruction::I::from_instruction(instruction, opcode::I::LH))
        }
        (0b000_0011, 0b010, _) => {
            Instruction::I(instruction::I::from_instruction(instruction, opcode::I::LW))
        }
        (0b000_0011, 0b100, _) => Instruction::I(instruction::I::from_instruction(
            instruction,
            opcode::I::LBU,
        )),
        (0b000_0011, 0b101, _) => Instruction::I(instruction::I::from_instruction(
            instruction,
            opcode::I::LHU,
        )),
        (0b010_0011, 0b000, _) => {
            Instruction::S(instruction::S::from_instruction(instruction, Width::Byte))
        }
        (0b010_0011, 0b001, _) => {
            Instruction::S(instruction::S::from_instruction(instruction, Width::Word))
        }
        (0b010_0011, 0b010, _) => {
            Instruction::S(instruction::S::from_instruction(instruction, Width::DWord))
        }
        (0b001_0011, 0b000, _) => Instruction::I(instruction::I::from_instruction(
            instruction,
            opcode::I::ADDI,
        )),
        (0b001_0011, 0b010, _) => Instruction::I(instruction::I::from_instruction(
            instruction,
            opcode::I::SLTI,
        )),
        (0b001_0011, 0b011, _) => Instruction::I(instruction::I::from_instruction(
            instruction,
            opcode::I::SLTIU,
        )),
        (0b001_0011, 0b100, _) => Instruction::I(instruction::I::from_instruction(
            instruction,
            opcode::I::XORI,
        )),
        (0b001_0011, 0b110, _) => Instruction::I(instruction::I::from_instruction(
            instruction,
            opcode::I::ORI,
        )),
        (0b001_0011, 0b111, _) => Instruction::I(instruction::I::from_instruction(
            instruction,
            opcode::I::ANDI,
        )),
        (0b001_0011, 0b001, 0b000_0000) => Instruction::R(instruction::R::from_instruction(
            instruction,
            opcode::R::Shift(opcode::RShift::SLLI),
        )),
        (0b001_0011, 0b101, 0b000_0000) => Instruction::R(instruction::R::from_instruction(
            instruction,
            opcode::R::Shift(opcode::RShift::SRLI),
        )),
        (0b001_0011, 0b101, 0b010_0000) => Instruction::R(instruction::R::from_instruction(
            instruction,
            opcode::R::Shift(opcode::RShift::SRAI),
        )),
        (0b011_0011, 0b000, 0b000_0000) => Instruction::R(instruction::R::from_instruction(
            instruction,
            opcode::R::Math(opcode::RMath::ADD),
        )),
        (0b011_0011, 0b000, 0b010_0000) => Instruction::R(instruction::R::from_instruction(
            instruction,
            opcode::R::Math(opcode::RMath::SUB),
        )),
        (0b011_0011, 0b001, 0b000_0000) => Instruction::R(instruction::R::from_instruction(
            instruction,
            opcode::R::Math(opcode::RMath::SLL),
        )),
        (0b011_0011, 0b010, 0b000_0000) => Instruction::R(instruction::R::from_instruction(
            instruction,
            // SLT
            opcode::R::Math(opcode::RMath::SCond(opcode::Cmp::Lt)),
        )),
        (0b011_0011, 0b011, 0b000_0000) => Instruction::R(instruction::R::from_instruction(
            instruction,
            // SLTU
            opcode::R::Math(opcode::RMath::SCond(opcode::Cmp::Ltu)),
        )),
        (0b011_0011, 0b100, 0b000_0000) => Instruction::R(instruction::R::from_instruction(
            instruction,
            opcode::R::Math(opcode::RMath::XOR),
        )),
        (0b011_0011, 0b101, 0b000_0000) => Instruction::R(instruction::R::from_instruction(
            instruction,
            opcode::R::Math(opcode::RMath::SRL),
        )),
        (0b011_0011, 0b101, 0b010_0000) => Instruction::R(instruction::R::from_instruction(
            instruction,
            opcode::R::Math(opcode::RMath::SRA),
        )),
        (0b011_0011, 0b110, 0b000_0000) => Instruction::R(instruction::R::from_instruction(
            instruction,
            opcode::R::Math(opcode::RMath::OR),
        )),
        (0b011_0011, 0b111, 0b000_0000) => Instruction::R(instruction::R::from_instruction(
            instruction,
            opcode::R::Math(opcode::RMath::AND),
        )),
        (0b000_1111, 0b000, _) => Instruction::I(instruction::I::from_instruction(
            instruction,
            opcode::I::FENCE,
        )),
        (0b111_0011, _, _) if instruction & !0b111_0011 == 0 => {
            Instruction::Sys(instruction::Sys::new(opcode::RSys::ECALL))
        }
        (0b111_0011, _, _) if instruction & !0b111_0011 == (1 << 20) => {
            Instruction::Sys(instruction::Sys::new(opcode::RSys::EBREAK))
        }
        (0b011_0011, 0b000, _) => Instruction::R(instruction::R::from_instruction(
            instruction,
            opcode::R::Math(opcode::RMath::MUL),
        )),
        (0b011_0011, 0b001, _) => Instruction::R(instruction::R::from_instruction(
            instruction,
            opcode::R::Math(opcode::RMath::MULH),
        )),
        (0b011_0011, 0b010, _) => Instruction::R(instruction::R::from_instruction(
            instruction,
            opcode::R::Math(opcode::RMath::MULHSU),
        )),
        (0b011_0011, 0b011, _) => Instruction::R(instruction::R::from_instruction(
            instruction,
            opcode::R::Math(opcode::RMath::MULHU),
        )),
        (0b011_0011, 0b100, _) => Instruction::R(instruction::R::from_instruction(
            instruction,
            opcode::R::Math(opcode::RMath::DIV),
        )),
        (0b011_0011, 0b101, _) => Instruction::R(instruction::R::from_instruction(
            instruction,
            opcode::R::Math(opcode::RMath::DIVU),
        )),
        (0b011_0011, 0b110, _) => Instruction::R(instruction::R::from_instruction(
            instruction,
            opcode::R::Math(opcode::RMath::REM),
        )),
        (0b011_0011, 0b111, _) => Instruction::R(instruction::R::from_instruction(
            instruction,
            opcode::R::Math(opcode::RMath::REMU),
        )),
        _ => return Err(DecodeError),
    };

    Ok(instruction)
}

fn decode_branch(instruction: u32) -> Result<instruction::B, DecodeError> {
    let imm = (((instruction >> 19) & 0b0001_0000_0000_0000)
        | ((instruction << 4) & 0b0000_1000_0000_0000)
        | ((instruction >> 20) & 0b0000_0111_1100_0000)
        | ((instruction >> 7) & 0b0000_0000_0011_1110)) as u16;

    let (rs1, rs2) = decode_rs(instruction);
    let cmp_mode = match ((instruction >> 12) & 0b111) as u8 {
        0b000 => opcode::Cmp::Eq,
        0b001 => opcode::Cmp::Ne,
        0b100 => opcode::Cmp::Lt,
        0b101 => opcode::Cmp::Ge,
        0b110 => opcode::Cmp::Ltu,
        0b111 => opcode::Cmp::Geu,
        0b010 | 0b011 => return Err(DecodeError),
        0x08..=0xff => unreachable!(),
    };

    Ok(instruction::B::new(imm, rs1, rs2, cmp_mode))
}

#[cfg(test)]
mod test {
    use crate::{instruction::Instruction, opcode};
    #[test]
    fn decode_ebreak() {
        let instruction =
            super::decode_instruction(0b0000_0000_0001_0000_0000_0000_0111_0011).unwrap();
        if let Instruction::Sys(instruction) = instruction {
            assert_eq!(instruction.opcode, opcode::RSys::EBREAK)
        } else {
            panic!("Instruction type wasn't RType!")
        }
    }
}
