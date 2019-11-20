// these opcode names are _exact_, and naming rules don't apply to them
#![allow(clippy::pub_enum_variant_names)]

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum R {
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum S {
    SB,
    SH,
    SW,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum B {
    BEQ,
    BNE,
    BLT,
    BGE,
    BLTU,
    BGEU,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum I {
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum U {
    LUI,
    AUIPC,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum J {
    JAL,
}
