// these opcode names are _exact_, and naming rules don't apply to them
#![allow(clippy::pub_enum_variant_names)]

// note: RISC-V would have these be: B, H(W), W
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Width {
    Byte,
    Word,
    DWord,
}

//todo: split
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum R {
    Shift(RShift),
    Math(RMath),
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum RShift {
    SLLI,
    SRLI,
    SRAI,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum RMath {
    ADD,
    SUB,
    SLL,
    SCond(Cmp),
    XOR,
    SRL,
    SRA,
    OR,
    AND,
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
pub enum RSys {
    ECALL,
    EBREAK,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Cmp {
    Eq,
    Ne,
    Lt,
    Ge,
    Ltu,
    Geu,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum I {
    FENCE,
    ADDI,
    SICond(Cmp),
    XORI,
    ORI,
    ANDI,
    JALR,
    LD(Width),
    // note that LDU(DWord) doesn't exist, but would be the same as LD(DWord) anyway.
    LDU(Width),
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
