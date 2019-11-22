// these opcode names are _exact_, and naming rules don't apply to them
#![allow(clippy::pub_enum_variant_names)]

// note: RISC-V would have these be: B, H(W), W
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Width {
    Byte,
    Word,
    DWord,
}

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
    LD(Width),
    // note that LDU(DWord) doesn't exist, but would be the same as LD(DWord) anyway.
    LDU(Width),
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
