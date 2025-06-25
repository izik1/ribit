// these opcode names are _exact_, and naming rules don't apply to them

use core::fmt;

use crate::Width;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum R {
    ADD,
    SUB,
    SLL,
    SCond(SCmp),
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
    pub const fn is_m_extension(self) -> bool {
        matches!(
            self,
            Self::MUL
                | Self::MULH
                | Self::MULHSU
                | Self::MULHU
                | Self::DIV
                | Self::DIVU
                | Self::REM
                | Self::REMU,
        )
    }

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
            R::SCond(SCmp::Lt) => "SLT",
            R::SCond(SCmp::Ltu) => "SLTU",
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
    pub const fn as_str(self) -> &'static str {
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
pub enum SCmp {
    Lt,
    Ltu,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum I {
    ADDI,
    SICond(SCmp),
    XORI,
    ORI,
    ANDI,
    SLLI,
    SRLI,
    SRAI,
}

impl I {
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            I::ADDI => "ADDI",
            I::XORI => "XORI",
            I::ORI => "ORI",
            I::ANDI => "ANDI",
            I::SLLI => "SLLI",
            I::SRLI => "SRLI",
            I::SRAI => "SRAI",
            I::SICond(SCmp::Lt) => "SLTI",
            I::SICond(SCmp::Ltu) => "SLTIU",
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
    pub const fn as_str(self) -> &'static str {
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
    pub const fn as_str(self) -> &'static str {
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
    pub const fn as_str(self) -> &'static str {
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
    pub const fn as_str(self) -> &'static str {
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
