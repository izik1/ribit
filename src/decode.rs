pub mod compressed;

use crate::DecodeError;

use crate::instruction::*;

pub fn decode_rs(instruction: u32) -> u8 {
    (instruction >> 15) as u8
}

pub fn decode_rd(instruction: u32) -> u8 {
    ((instruction >> 7) & 0b1111) as u8
}

pub fn decode_instruction(instruction: u32) -> Result<Instruction, DecodeError> {
    let funct3 = ((instruction >> 12) & 0b0000_0111) as u8;
    let funct7 = ((instruction >> 25) & 0b0111_1111) as u8;
    let opcode = (instruction & 0b0111_1111) as u8;
    let instruction =
        match (opcode, funct3, funct7) {
            (0b110_1111, _, _) => Instruction::JType(JTypeInstruction::from_instruction(
                instruction,
                JTypeOpcode::JAL,
            )),
            (0b110_0111, 0b000, _) => Instruction::IType(ITypeInstruction::from_instruction(
                instruction,
                ITypeOpcode::JALR,
            )),
            (0b110_0011, _, _) => Instruction::BType(decode_branch(instruction)?),
            (0b000_0011, 0b000, _) => Instruction::IType(ITypeInstruction::from_instruction(
                instruction,
                ITypeOpcode::LB,
            )),
            (0b000_0011, 0b001, _) => Instruction::IType(ITypeInstruction::from_instruction(
                instruction,
                ITypeOpcode::LH,
            )),
            (0b000_0011, 0b010, _) => Instruction::IType(ITypeInstruction::from_instruction(
                instruction,
                ITypeOpcode::LW,
            )),
            (0b000_0011, 0b100, _) => Instruction::IType(ITypeInstruction::from_instruction(
                instruction,
                ITypeOpcode::LBU,
            )),
            (0b000_0011, 0b101, _) => Instruction::IType(ITypeInstruction::from_instruction(
                instruction,
                ITypeOpcode::LHU,
            )),
            (0b010_0011, 0b000, _) => Instruction::SType(STypeInstruction::from_instruction(
                instruction,
                STypeOpcode::SB,
            )),
            (0b010_0011, 0b001, _) => Instruction::SType(STypeInstruction::from_instruction(
                instruction,
                STypeOpcode::SH,
            )),
            (0b010_0011, 0b010, _) => Instruction::SType(STypeInstruction::from_instruction(
                instruction,
                STypeOpcode::SW,
            )),
            (0b001_0011, 0b000, _) => Instruction::IType(ITypeInstruction::from_instruction(
                instruction,
                ITypeOpcode::ADDI,
            )),
            (0b001_0011, 0b010, _) => Instruction::IType(ITypeInstruction::from_instruction(
                instruction,
                ITypeOpcode::SLTI,
            )),
            (0b001_0011, 0b011, _) => Instruction::IType(ITypeInstruction::from_instruction(
                instruction,
                ITypeOpcode::SLTIU,
            )),
            (0b001_0011, 0b100, _) => Instruction::IType(ITypeInstruction::from_instruction(
                instruction,
                ITypeOpcode::XORI,
            )),
            (0b001_0011, 0b110, _) => Instruction::IType(ITypeInstruction::from_instruction(
                instruction,
                ITypeOpcode::ORI,
            )),
            (0b001_0011, 0b111, _) => Instruction::IType(ITypeInstruction::from_instruction(
                instruction,
                ITypeOpcode::ANDI,
            )),
            (0b001_0011, 0b001, 0b000_0000) => Instruction::RType(
                RTypeInstruction::from_instruction(instruction, RTypeOpcode::SLLI),
            ),
            (0b001_0011, 0b101, 0b000_0000) => Instruction::RType(
                RTypeInstruction::from_instruction(instruction, RTypeOpcode::SRLI),
            ),
            (0b001_0011, 0b101, 0b010_0000) => Instruction::RType(
                RTypeInstruction::from_instruction(instruction, RTypeOpcode::SRAI),
            ),
            (0b011_0011, 0b000, 0b000_0000) => Instruction::RType(
                RTypeInstruction::from_instruction(instruction, RTypeOpcode::ADD),
            ),
            (0b011_0011, 0b000, 0b010_0000) => Instruction::RType(
                RTypeInstruction::from_instruction(instruction, RTypeOpcode::SUB),
            ),
            (0b011_0011, 0b001, 0b000_0000) => Instruction::RType(
                RTypeInstruction::from_instruction(instruction, RTypeOpcode::SLL),
            ),
            (0b011_0011, 0b010, 0b000_0000) => Instruction::RType(
                RTypeInstruction::from_instruction(instruction, RTypeOpcode::SLT),
            ),
            (0b011_0011, 0b011, 0b000_0000) => Instruction::RType(
                RTypeInstruction::from_instruction(instruction, RTypeOpcode::SLTU),
            ),
            (0b011_0011, 0b100, 0b000_0000) => Instruction::RType(
                RTypeInstruction::from_instruction(instruction, RTypeOpcode::XOR),
            ),
            (0b011_0011, 0b101, 0b010_0000) => Instruction::RType(
                RTypeInstruction::from_instruction(instruction, RTypeOpcode::SRL),
            ),
            (0b011_0011, 0b101, 0b010_0000) => Instruction::RType(
                RTypeInstruction::from_instruction(instruction, RTypeOpcode::SRA),
            ),
            (0b011_0011, 0b110, 0b000_0000) => Instruction::RType(
                RTypeInstruction::from_instruction(instruction, RTypeOpcode::OR),
            ),
            (0b011_0011, 0b111, 0b000_0000) => Instruction::RType(
                RTypeInstruction::from_instruction(instruction, RTypeOpcode::AND),
            ),
            (0b000_1111, 0b000, _) => Instruction::IType(ITypeInstruction::from_instruction(
                instruction,
                ITypeOpcode::FENCE,
            )),
            (0b111_0011, _, _) if instruction & !0b111_0011 == 0 => Instruction::RType(
                RTypeInstruction::from_instruction(instruction, RTypeOpcode::ECALL),
            ),
            (0b111_0011, _, _) if instruction & !0b111_0011 == (1 << 20) => Instruction::RType(
                RTypeInstruction::from_instruction(instruction, RTypeOpcode::EBREAK),
            ),
            (0b011_0011, 0b000, _) => Instruction::RType(RTypeInstruction::from_instruction(
                instruction,
                RTypeOpcode::MUL,
            )),
            (0b011_0011, 0b001, _) => Instruction::RType(RTypeInstruction::from_instruction(
                instruction,
                RTypeOpcode::MULH,
            )),
            (0b011_0011, 0b010, _) => Instruction::RType(RTypeInstruction::from_instruction(
                instruction,
                RTypeOpcode::MULHSU,
            )),
            (0b011_0011, 0b011, _) => Instruction::RType(RTypeInstruction::from_instruction(
                instruction,
                RTypeOpcode::MULHU,
            )),
            (0b011_0011, 0b100, _) => Instruction::RType(RTypeInstruction::from_instruction(
                instruction,
                RTypeOpcode::DIV,
            )),
            (0b011_0011, 0b101, _) => Instruction::RType(RTypeInstruction::from_instruction(
                instruction,
                RTypeOpcode::DIVU,
            )),
            (0b011_0011, 0b110, _) => Instruction::RType(RTypeInstruction::from_instruction(
                instruction,
                RTypeOpcode::REM,
            )),
            (0b011_0011, 0b111, _) => Instruction::RType(RTypeInstruction::from_instruction(
                instruction,
                RTypeOpcode::REMU,
            )),
            _ => return Err(DecodeError),
        };

    Ok(instruction)
}

fn decode_branch(instruction: u32) -> Result<BTypeInstruction, DecodeError> {
    let imm = (((instruction >> 19) & 0b0001_0000_0000_0000)
        | ((instruction << 04) & 0b0000_1000_0000_0000)
        | ((instruction >> 20) & 0b0000_0111_1100_0000)
        | ((instruction >> 07) & 0b0000_0000_0011_1110)) as u16;

    let rs = (instruction >> 17) as u8;
    let opcode = match (instruction >> 12) & 0b111 {
        0b000 => BTypeOpcode::BEQ,
        0b001 => BTypeOpcode::BNE,
        0b100 => BTypeOpcode::BLT,
        0b101 => BTypeOpcode::BGE,
        0b110 => BTypeOpcode::BLTU,
        0b111 => BTypeOpcode::BGEU,
        _ => return Err(DecodeError),
    };

    Ok(BTypeInstruction::new(imm, rs, opcode))
}

#[cfg(test)]
mod test {
    use crate::instruction::{Instruction, RTypeOpcode};

    #[test]
    fn decode_ebreak() {
        let instruction =
            super::decode_instruction(0b0000_0000_0001_0000_0000_0000_0111_0011).unwrap();
        if let Instruction::RType(instruction) = instruction {
            assert_eq!(instruction.opcode, RTypeOpcode::EBREAK)
        } else {
            panic!("Instruction type wasn't RType!")
        }
    }
}
