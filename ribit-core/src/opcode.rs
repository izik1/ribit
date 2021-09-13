// these opcode names are _exact_, and naming rules don't apply to them
#![allow(clippy::pub_enum_variant_names)]

use std::fmt;

use crate::Width;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum R {
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

impl R {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            R::ADD => "ADD",
            R::SUB => "SUB",
            R::SLL => "SLL",
            R::XOR => "XOR",
            R::SRL => "SRL",
            R::SRA => "SRA",
            R::OR => "OR",
            R::AND => "AND",
            R::MUL => "MUL",
            R::MULH => "MULH",
            R::MULHSU => "MULHSU",
            R::MULHU => "MULHU",
            R::DIV => "DIV",
            R::DIVU => "DIVU",
            R::REM => "REM",
            R::REMU => "REMU",
            R::SCond(Cmp::Lt) => "SLT",
            R::SCond(Cmp::Ltu) => "SLTU",
            R::SCond(Cmp::Eq) => panic!("invalid instruction: SEQ"),
            R::SCond(Cmp::Ne) => panic!("invalid instruction: SNE"),
            R::SCond(Cmp::Ge) => panic!("invalid instruction: SGE"),
            R::SCond(Cmp::Geu) => panic!("invalid instruction: SGEU"),
        }
    }
}

impl fmt::Display for R {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str(self.as_str())
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum RSys {
    ECALL,
    EBREAK,
}

impl RSys {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            RSys::ECALL => "ECALL",
            RSys::EBREAK => "EBREAK",
        }
    }
}

impl fmt::Display for RSys {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str(self.as_str())
    }
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
    ADDI,
    SICond(Cmp),
    XORI,
    ORI,
    ANDI,
    SLLI,
    SRLI,
    SRAI,
}

impl I {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            I::ADDI => "ADDI",
            I::XORI => "XORI",
            I::ORI => "ORI",
            I::ANDI => "ANDI",
            I::SLLI => "SLLI",
            I::SRLI => "SRLI",
            I::SRAI => "SRAI",
            I::SICond(Cmp::Lt) => "SLTI",
            I::SICond(Cmp::Ltu) => "SLTIU",
            I::SICond(Cmp::Eq) => panic!("invalid instruction: SEQI"),
            I::SICond(Cmp::Ne) => panic!("invalid instruction: SNEI"),
            I::SICond(Cmp::Ge) => panic!("invalid instruction: SGEI"),
            I::SICond(Cmp::Geu) => panic!("invalid instruction: SGEIU"),
        }
    }
}

impl fmt::Display for I {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str(self.as_str())
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum IMem {
    FENCE,
    LD(Width),
    // note that LDU(DWord) doesn't exist, but would be the same as LD(DWord) anyway.
    LDU(Width),
}

impl IMem {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            IMem::FENCE => "FENCE",
            IMem::LD(Width::Byte) => "LB",
            IMem::LD(Width::Word) => "LH",
            IMem::LD(Width::DWord) => "LW",
            IMem::LDU(Width::Byte) => "LBU",
            IMem::LDU(Width::Word) => "LHU",
            IMem::LDU(Width::DWord) => panic!("Invalid instruction LWU"),
        }
    }
}

impl fmt::Display for IMem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str(self.as_str())
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum IJump {
    JALR,
}

impl IJump {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            IJump::JALR => "JALR",
        }
    }
}

impl fmt::Display for IJump {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str(self.as_str())
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum U {
    LUI,
    AUIPC,
}

impl U {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            U::LUI => "LUI",
            U::AUIPC => "AUIPC",
        }
    }
}

impl fmt::Display for U {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str(self.as_str())
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum J {
    JAL,
}

impl J {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            J::JAL => "JAL",
        }
    }
}

impl fmt::Display for J {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str(self.as_str())
    }
}
