use ribit_core::instruction::{self, Instruction};
use ribit_core::{Width, opcode, register};

pub mod compressed;

#[derive(Debug)]
pub enum EncodeError {
    InvalidInstruction,
    /// the instruction doesn't have a two byte (C.) variant.
    Incompressable,
}

#[inline]
const fn encode_register(register: Option<register::RiscV>) -> u32 {
    match register {
        None => 0,
        Some(it) => it.get() as u32,
    }
}

#[must_use]
fn encode_rs(rs1: Option<register::RiscV>, rs2: Option<register::RiscV>) -> u32 {
    let rs1 = encode_register(rs1) << 15;
    let rs2 = encode_register(rs2) << 20;

    rs1 | rs2
}

#[must_use]
fn encode_rd(rd: Option<register::RiscV>) -> u32 {
    encode_register(rd) << 7
}

#[inline]
fn signed_truncate<const BITS: u8>(imm: u16) -> Result<u16, EncodeError> {
    debug_assert!(BITS > 0);

    // truncate from 16 bits to BITS.
    // the tricky thing here is that we want to error if we lose values.
    // like, say we have the value `1000_0000_0000_0000`, that should error if `BITS < 16`, because otherwise it gets turned into 0.

    // basically for the bits we truncate out they must all be the same value (all 0s or 1s).

    // build a mask of zeros at the MSB end of `16 - BITS` bits by turning a full mask into "full mask except MSB end".
    let mask = u16::MAX.wrapping_shr(u16::BITS - u32::from(BITS));

    if imm & !mask == 0 || imm & !mask == !mask {
        Ok(imm & mask)
    } else {
        Err(EncodeError::InvalidInstruction)
    }
}

#[inline]
fn signed_truncate_32<const BITS: u8>(imm: u32) -> Result<u32, EncodeError> {
    debug_assert!(BITS > 0);

    // truncate from 32 bits to BITS.
    // the tricky thing here is that we want to error if we lose values.
    // like, say we have the value `1000_0000_0000_0000`, that should error if `BITS < 32`, because otherwise it gets turned into 0.

    // basically for the bits we truncate out they must all be the same value (all 0s or 1s).

    // build a mask of zeros at the MSB end of `32 - BITS` bits by turning a full mask into "full mask except MSB end".
    let mask = u32::MAX.wrapping_shr(u32::BITS - u32::from(BITS));

    if imm & !mask == 0 || imm & !mask == !mask {
        Ok(imm & mask)
    } else {
        Err(EncodeError::InvalidInstruction)
    }
}

pub fn i_32(regs: u32, opcode: u8, func3: u8, imm: u16) -> Result<u32, EncodeError> {
    let opcode = u32::from(opcode);
    let func3 = u32::from(func3) << 12;

    let imm = u32::from(signed_truncate::<12>(imm)?) << 20;

    Ok(regs | opcode | func3 | imm)
}

pub fn instruction(instruction: &Instruction) -> Result<u32, EncodeError> {
    match instruction {
        Instruction::R(instruction::R { rs1, rs2, rd, opcode }) => {
            let regs = encode_rs(*rs1, *rs2) | encode_rd(*rd);

            let (opcode, func3, func7): (u8, u8, u8) = match opcode {
                opcode::R::ADD => (0b011_0011, 0b000, 0b000_0000),
                opcode::R::SUB => (0b011_0011, 0b000, 0b010_0000),
                opcode::R::SLL => (0b011_0011, 0b001, 0b000_0000),
                opcode::R::SCond(opcode::Cmp::Lt) => (0b011_0011, 0b010, 0b000_0000),
                opcode::R::SCond(opcode::Cmp::Ltu) => (0b011_0011, 0b011, 0b000_0000),
                // these don't exist.
                opcode::R::SCond(_) => return Err(EncodeError::InvalidInstruction),
                opcode::R::XOR => (0b011_0011, 0b100, 0b000_0000),
                opcode::R::SRL => (0b011_0011, 0b101, 0b000_0000),
                opcode::R::SRA => (0b011_0011, 0b101, 0b010_0000),
                opcode::R::OR => (0b011_0011, 0b110, 0b000_0000),
                opcode::R::AND => (0b011_0011, 0b111, 0b000_0000),
                opcode::R::MUL => (0b011_0011, 0b000, 0b000_0001),
                opcode::R::MULH => (0b011_0011, 0b001, 0b000_0001),
                opcode::R::MULHSU => (0b011_0011, 0b010, 0b000_0001),
                opcode::R::MULHU => (0b011_0011, 0b011, 0b000_0001),
                opcode::R::DIV => (0b011_0011, 0b100, 0b000_0001),
                opcode::R::DIVU => (0b011_0011, 0b101, 0b000_0001),
                opcode::R::REM => (0b011_0011, 0b110, 0b000_0001),
                opcode::R::REMU => (0b011_0011, 0b111, 0b000_0001),
            };

            let opcode = u32::from(opcode);
            let func3 = u32::from(func3) << 12;
            let func7 = u32::from(func7) << 25;

            Ok(regs | opcode | func3 | func7)
        }

        Instruction::I(instruction::I { imm, rs1, rd, opcode }) => {
            let regs = encode_rs(*rs1, None) | encode_rd(*rd);

            let imm = match opcode {
                opcode::I::SLLI => (imm & 0b1_1111) | (0b000_0000 << 5),
                opcode::I::SRLI => (imm & 0b1_1111) | (0b000_0000 << 5),
                opcode::I::SRAI => (imm & 0b1_1111) | (0b010_0000 << 5),

                _ => *imm,
            };

            let (opcode, func3): (u8, u8) = match opcode {
                opcode::I::ADDI => (0b001_0011, 0b000),
                opcode::I::SICond(opcode::Cmp::Lt) => (0b001_0011, 0b010),
                opcode::I::SICond(opcode::Cmp::Ltu) => (0b001_0011, 0b011),
                // these don't exist
                opcode::I::SICond(_) => return Err(EncodeError::InvalidInstruction),
                opcode::I::XORI => (0b001_0011, 0b100),
                opcode::I::ORI => (0b001_0011, 0b110),
                opcode::I::ANDI => (0b001_0011, 0b111),
                opcode::I::SLLI => (0b001_0011, 0b001),
                opcode::I::SRLI => (0b001_0011, 0b101),
                opcode::I::SRAI => (0b001_0011, 0b101),
            };

            i_32(regs, opcode, func3, imm)
        }

        Instruction::IJump(instruction::IJump { rs1, rd, opcode, imm }) => {
            let regs = encode_rs(*rs1, None) | encode_rd(*rd);

            let (opcode, func3): (u8, u8) = match opcode {
                opcode::IJump::JALR => (0b110_0111, 0b000),
            };

            i_32(regs, opcode, func3, *imm)
        }

        Instruction::IMem(instruction::IMem { rs1, rd, opcode, imm }) => {
            let regs = encode_rs(*rs1, None) | encode_rd(*rd);

            let (opcode, func3): (u8, u8) = match opcode {
                opcode::IMem::FENCE => (0b000_1111, 0b000),
                opcode::IMem::LD(Width::Byte) => (0b000_0011, 0b000),
                opcode::IMem::LD(Width::Word) => (0b000_0011, 0b001),
                opcode::IMem::LD(Width::DWord) => (0b000_0011, 0b010),
                opcode::IMem::LDU(Width::Byte) => (0b000_0011, 0b100),
                opcode::IMem::LDU(Width::Word) => (0b000_0011, 0b101),
                // LWU doesn't exist
                opcode::IMem::LDU(Width::DWord) => return Err(EncodeError::InvalidInstruction),
            };

            i_32(regs, opcode, func3, *imm)
        }

        Instruction::S(instruction::S { imm, rs1, rs2, width }) => {
            const OPCODE: u8 = 0b010_0011;
            let regs = encode_rs(*rs1, *rs2);

            let func3: u8 = match width {
                Width::Byte => 0b000,
                Width::Word => 0b001,
                Width::DWord => 0b010,
            };

            let imm = signed_truncate::<12>(*imm)?;

            let imm = u32::from(imm);
            let imm = ((imm & !0b1_1111) << 20) | ((imm & 0b1_1111) << 7);
            let func3 = u32::from(func3) << 12;

            Ok(imm | regs | func3 | u32::from(OPCODE))
        }

        Instruction::B(instruction::B { rs1, rs2, imm, cmp_mode }) => {
            const OPCODE: u8 = 0b110_0011;
            let regs = encode_rs(*rs1, *rs2);

            let func3: u8 = match cmp_mode {
                opcode::Cmp::Eq => 0b000,
                opcode::Cmp::Ne => 0b001,
                opcode::Cmp::Lt => 0b100,
                opcode::Cmp::Ge => 0b101,
                opcode::Cmp::Ltu => 0b110,
                opcode::Cmp::Geu => 0b111,
            };

            let imm = u32::from(*imm);

            let imm = ((imm & 0b0001_0000_0000_0000) << 19)
                | ((imm & 0b0000_0111_1110_0000) << 20)
                | ((imm & 0b0000_1000_0000_0000) >> 4)
                | ((imm & 0b0000_0000_0001_1110) << 7);

            let func3 = u32::from(func3) << 12;

            Ok(imm | regs | func3 | u32::from(OPCODE))
        }

        Instruction::U(instruction::U { imm, rd, opcode }) => {
            let regs = encode_rd(*rd);

            let opcode: u8 = match opcode {
                opcode::U::LUI => 0b0011_0111,
                opcode::U::AUIPC => 0b001_0111,
            };

            if imm & 0xffff_f000 != *imm {
                return Err(EncodeError::InvalidInstruction);
            }

            Ok(imm | regs | u32::from(opcode))
        }

        Instruction::J(instruction::J { imm, rd, opcode }) => {
            let regs = encode_rd(*rd);

            let opcode: u8 = match opcode {
                opcode::J::JAL => 0b110_1111,
            };

            // instruction::000a_dddd_dddd_cbbb_bbbb_bbb0 -> abbb_bbbb_bbbc_dddd_dddd_xxxx_xxxx_xxxx
            let imm = ((imm & 0b0001_0000_0000_0000_0000_0000) << 11)
                | ((imm & 0b0000_0000_0000_0111_1111_1110) << 20)
                | ((imm & 0b0000_0000_0000_1000_0000_0000) << 9)
                | (imm & 0b0000_1111_1111_0000_0000_0000);

            Ok(imm | regs | u32::from(opcode))
        }

        Instruction::Sys(sys) => match sys.opcode {
            opcode::RSys::ECALL => Ok(0b111_0011 | (0 << 20)),
            opcode::RSys::EBREAK => Ok(0b111_0011 | (1 << 20)),
        },
    }
}

#[cfg(test)]
mod tests {
    #[test]
    #[ignore = "test is very slow"]
    fn roundtrip_all() {
        for raw in 0_u32..=u32::MAX {
            if let Ok(instruction) = ribit_decode::instruction(raw) {
                let res = super::instruction(&instruction);

                assert!(res.is_ok(), "decode failed: {instruction:?}");

                let res = res.unwrap();

                assert!(raw == res, "roundtrip failed ({raw:032b} != {res:032b}): {instruction:?}");
            }
        }
    }
}
