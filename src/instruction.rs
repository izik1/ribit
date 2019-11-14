use crate::decode::{decode_rd, decode_rs};

// todo: asserts
pub enum Instruction {
    RType(RTypeInstruction),
    IType(ITypeInstruction),
    SType(STypeInstruction),
    BType(BTypeInstruction),
    UType(UTypeInstruction),
    JType(JTypeInstruction),
}

#[derive(Debug)]
pub struct RTypeInstruction {
    rs: u8, // 7:4 -> rs2, 3:0 -> rs1
    rd: u8,
    opcode: RTypeOpcode,
}

impl RTypeInstruction {
    pub(crate) fn new(rs: u8, rd: u8, opcode: RTypeOpcode) -> Self {
        Self { rs, rd, opcode }
    }

    pub(crate) fn from_instruction(instruction: u32, opcode: RTypeOpcode) -> Self {
        let rs = decode_rs(instruction);
        let rd = decode_rd(instruction);
        Self { rs, rd, opcode }
    }
}

pub struct ITypeInstruction {
    imm: u16,
    rs1: u8,
    rd: u8,
    opcode: ITypeOpcode,
}

impl ITypeInstruction {
    pub(crate) fn new(imm: u16, rs1: u8, rd: u8, opcode: ITypeOpcode) -> Self {
        Self {
            imm,
            rs1,
            rd,
            opcode,
        }
    }

    pub(crate) fn from_instruction(instruction: u32, opcode: ITypeOpcode) -> ITypeInstruction {
        let imm = ((instruction >> 20) & 0x0fff) as u16;
        let rs1 = decode_rs(instruction) & 0x0f; // only has rs1
        let rd = decode_rd(instruction);
        ITypeInstruction {
            imm,
            rs1,
            rd,
            opcode,
        }
    }
}

pub struct STypeInstruction {
    imm: u16,
    rs: u8, // 7:4 -> rs2, 3:0 -> rs1
    opcode: STypeOpcode,
}

impl STypeInstruction {
    pub(crate) fn new(imm: u16, rs: u8, opcode: STypeOpcode) -> STypeInstruction {
        Self { imm, rs, opcode }
    }

    pub(crate) fn from_instruction(instruction: u32, opcode: STypeOpcode) -> STypeInstruction {
        let rs = decode_rs(instruction);

        let imm = (((instruction >> 19) & 0b0000_1111_1110_0000)
            | ((instruction >> 07) & 0b0000_0000_0001_1111)) as u16;

        STypeInstruction { rs, imm, opcode }
    }
}

pub struct BTypeInstruction {
    rs: u8, // 7:4 -> rs2, 3:0 -> rs1
    imm: u16,
    opcode: BTypeOpcode,
}

impl BTypeInstruction {
    pub(crate) fn new(imm: u16, rs: u8, opcode: BTypeOpcode) -> Self {
        Self { imm, rs, opcode }
    }
}

pub struct UTypeInstruction {
    imm: u32,
    rd: u8,
    opcode: UTypeOpcode,
}

impl UTypeInstruction {
    pub(crate) fn new(imm: u32, rd: u8, opcode: UTypeOpcode) -> Self {
        Self { imm, rd, opcode }
    }
}

pub struct JTypeInstruction {
    imm: u32,
    rd: u8,
    opcode: JTypeOpcode,
}

impl JTypeInstruction {
    pub(crate) fn new(imm: u32, rd: u8, opcode: JTypeOpcode) -> Self {
        Self { imm, rd, opcode }
    }

    pub(crate) fn from_instruction(instruction: u32, opcode: JTypeOpcode) -> Self {
        let rd = ((instruction >> 7) & 0x0f) as u8;
        // abbb_bbbb_bbbc_dddd_dddd_xxxx_xxxx_xxxx -> 000a_dddd_dddd_cbbb_bbbb_bbb0
        let imm = ((instruction >> 11) & 0b0001_0000_0000_0000_0000_0000)
            | ((instruction >> 19) & 0b0000_0000_0000_0111_1111_1110)
            | ((instruction >> 09) & 0b0000_0000_0000_1000_0000_0000)
            | ((instruction >> 00) & 0b0000_1111_1111_0000_0000_0000);

        Self { imm, rd, opcode }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum RTypeOpcode {
    SLLI,
    SRLI,
    SRAI,
    ADD,
    SUB,
    SLL,
    SLT,
    SLTU,
    XOR,
    SRL,
    SRA,
    OR,
    AND,
    ECALL,
    EBREAK,
    MUL,
    MULH,
    MULHSU,
    MULHU,
    DIV,
    DIVU,
    REM,
    REMU,
}

pub enum STypeOpcode {
    SB,
    SH,
    SW,
}

pub enum BTypeOpcode {
    BEQ,
    BNE,
    BLT,
    BGE,
    BLTU,
    BGEU,
}

pub enum ITypeOpcode {
    FENCE,
    ADDI,
    SLTI,
    SLTIU,
    XORI,
    ORI,
    ANDI,
    JALR,
    LB,
    LH,
    LW,
    LBU,
    LHU,
}

pub enum UTypeOpcode {
    LUI,
    AUIPC,
}

pub enum JTypeOpcode {
    JAL,
}
