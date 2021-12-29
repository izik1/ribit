use ribit_core::instruction::{self, Instruction};
use ribit_core::opcode::{self, Cmp};
use ribit_core::{register, Width};

use super::{sign_extend_32, ParseContext};

use super::{parse_general_purpose_register, parse_immediate, sign_extend, test_len};

pub(super) fn r_32(context: &mut ParseContext, op: &str, full_op: &str, args: &[&str]) -> bool {
    let opcode = match op {
        "ADD" => (opcode::R::ADD),
        "SUB" => (opcode::R::SUB),
        "SLL" => (opcode::R::SLL),
        "SLT" => (opcode::R::SCond(Cmp::Lt)),
        "SLTU" => (opcode::R::SCond(Cmp::Ltu)),
        "XOR" => (opcode::R::XOR),
        "SRL" => (opcode::R::SRL),
        "SRA" => (opcode::R::SRA),
        "OR" => (opcode::R::OR),
        "AND" => (opcode::R::AND),
        "MUL" => (opcode::R::MUL),
        "MULH" => (opcode::R::MULH),
        "MULHSU" => (opcode::R::MULHSU),
        "MULHU" => (opcode::R::MULHU),
        "DIV" => (opcode::R::DIV),
        "DIVU" => (opcode::R::DIVU),
        "REM" => (opcode::R::REM),
        "REMU" => (opcode::R::REMU),
        _ => return false,
    };

    if let Some([rd, rs1, rs2]) = r_args(context, full_op, &args) {
        context.instructions.push(instruction::Info {
            instruction: Instruction::R(instruction::R::new(rs1, rs2, rd, opcode)),
            len: 4,
        })
    }

    true
}

fn r_args(
    context: &mut ParseContext,
    full_op: &str,
    args: &[&str],
) -> Option<[Option<register::RiscV>; 3]> {
    let mut has_error = test_len(context, full_op, 3, args.len());

    let rd = match args.get(0) {
        Some(reg) => match parse_general_purpose_register(reg) {
            Ok(it) => it,
            Err(e) => {
                has_error = true;
                context.errors.push(e);
                None
            }
        },
        None => None,
    };

    let rs1 = match args.get(1) {
        Some(reg) => match parse_general_purpose_register(reg) {
            Ok(it) => it,
            Err(e) => {
                has_error = true;
                context.errors.push(e);
                None
            }
        },
        None => None,
    };

    let rs2 = match args.get(2) {
        Some(reg) => match parse_general_purpose_register(reg) {
            Ok(it) => it,
            Err(e) => {
                has_error = true;
                context.errors.push(e);
                None
            }
        },
        None => None,
    };

    (!has_error).then(|| [rd, rs1, rs2])
}

pub(super) fn i_32(context: &mut ParseContext, op: &str, full_op: &str, args: &[&str]) -> bool {
    let opcode = match op {
        "ADDI" => opcode::I::ADDI,
        "SLTI" => opcode::I::SICond(Cmp::Lt),
        "SLTIU" => opcode::I::SICond(Cmp::Ltu),
        "XORI" => opcode::I::XORI,
        "ORI" => opcode::I::ORI,
        "ANDI" => opcode::I::ANDI,
        "SLLI" => opcode::I::SLLI,
        "SRLI" => opcode::I::SRLI,
        "SRAI" => opcode::I::SRAI,
        _ => return false,
    };

    if let Some(([rd, rs1], imm)) = i_args(context, full_op, &args) {
        context.instructions.push(instruction::Info {
            instruction: Instruction::I(instruction::I::new(imm, rs1, rd, opcode)),
            len: 4,
        })
    }

    true
}

fn i_args(
    context: &mut ParseContext,
    full_op: &str,
    args: &[&str],
) -> Option<([Option<register::RiscV>; 2], u16)> {
    let mut has_error = test_len(context, full_op, 3, args.len());

    let rd = match args.get(0) {
        Some(reg) => match parse_general_purpose_register(reg) {
            Ok(it) => it,
            Err(e) => {
                has_error = true;
                context.errors.push(e);
                None
            }
        },
        None => None,
    };

    let rs1 = match args.get(1) {
        Some(reg) => match parse_general_purpose_register(reg) {
            Ok(it) => it,
            Err(e) => {
                has_error = true;
                context.errors.push(e);
                None
            }
        },
        None => None,
    };

    let imm = match args.get(2) {
        Some(it) => match parse_immediate(it, 12) {
            Ok(it) => sign_extend(it as u16, 12),
            Err(e) => {
                has_error = true;
                context.errors.push(e);
                0
            }
        },
        None => 0,
    };

    (!has_error).then(|| ([rd, rs1], imm))
}

pub(super) fn ijump_32(context: &mut ParseContext, op: &str, full_op: &str, args: &[&str]) -> bool {
    let opcode = match op {
        "JALR" => opcode::IJump::JALR,
        _ => return false,
    };

    if let Some((rd, imm, rs1)) = rir_args(context, full_op, &args, 12) {
        context.instructions.push(instruction::Info {
            instruction: Instruction::IJump(instruction::IJump::new(imm, rs1, rd, opcode)),
            len: 4,
        })
    }

    true
}

pub(super) fn imem_32(context: &mut ParseContext, op: &str, full_op: &str, args: &[&str]) -> bool {
    let opcode = match op {
        "FENCE" => opcode::IMem::FENCE,
        "LB" => opcode::IMem::LD(Width::Byte),
        "LH" => opcode::IMem::LD(Width::Word),
        "LW" => opcode::IMem::LD(Width::DWord),
        "LBU" => opcode::IMem::LDU(Width::Byte),
        "LHU" => opcode::IMem::LDU(Width::Word),
        _ => return false,
    };

    if opcode == opcode::IMem::FENCE {
        if !test_len(context, full_op, 0, args.len()) {
            context.instructions.push(instruction::Info {
                instruction: Instruction::IMem(instruction::IMem::new(0, None, None, opcode)),
                len: 4,
            })
        }

        return true;
    }

    if let Some((rd, imm, rs1)) = rir_args(context, full_op, &args, 12) {
        context.instructions.push(instruction::Info {
            instruction: Instruction::IMem(instruction::IMem::new(imm, rs1, rd, opcode)),
            len: 4,
        })
    }

    true
}

pub(super) fn s_32(context: &mut ParseContext, op: &str, full_op: &str, args: &[&str]) -> bool {
    let width = match op {
        "SB" => Width::Byte,
        "SH" => Width::Word,
        "SW" => Width::DWord,
        _ => return false,
    };

    if let Some((rs1, imm, rs2)) = rir_args(context, full_op, &args, 12) {
        context.instructions.push(instruction::Info {
            instruction: Instruction::S(instruction::S::new(imm, rs1, rs2, width)),
            len: 4,
        })
    }

    true
}

pub(super) fn b_32(context: &mut ParseContext, op: &str, full_op: &str, args: &[&str]) -> bool {
    let cmp = match op {
        "BEQ" => Cmp::Eq,
        "BNE" => Cmp::Ne,
        "BLT" => Cmp::Lt,
        "BLTU" => Cmp::Ltu,
        "BGE" => Cmp::Ge,
        "BGEU" => Cmp::Geu,
        _ => return false,
    };

    if let Some((rs1, imm, rs2)) = rir_args(context, full_op, &args, 12) {
        let imm = match context.supports_compressed {
            true => imm << 1,
            false => imm << 2,
        };

        context.instructions.push(instruction::Info {
            instruction: Instruction::B(instruction::B::new(imm, rs1, rs2, cmp)),
            len: 4,
        })
    }

    true
}

fn rir_args(
    context: &mut ParseContext,
    full_op: &str,
    args: &[&str],
    immediate_width: u8,
) -> Option<(Option<register::RiscV>, u16, Option<register::RiscV>)> {
    let mut has_error = test_len(context, full_op, 2, args.len());

    let r1 = match args.get(0) {
        Some(reg) => match parse_general_purpose_register(reg) {
            Ok(it) => it,
            Err(e) => {
                has_error = true;
                context.errors.push(e);
                None
            }
        },
        None => None,
    };

    let (imm, r2) = match args.get(1) {
        Some(it) => {
            match it.split_once('(').and_then(|(imm, r)| r.strip_suffix(')').map(|r| (imm, r))) {
                Some((imm, r2)) => {
                    let imm = match parse_immediate(imm, immediate_width) {
                        Ok(it) => sign_extend(it as u16, immediate_width),
                        Err(e) => {
                            has_error = true;
                            context.errors.push(e);
                            0
                        }
                    };

                    let r2 = match parse_general_purpose_register(r2) {
                        Ok(it) => it,
                        Err(e) => {
                            has_error = true;
                            context.errors.push(e);
                            None
                        }
                    };

                    (imm, r2)
                }
                None => {
                    has_error = true;
                    context
                        .errors
                        .push(format!("expected an arg in the format 0(x2), found `{}`", it));

                    (0, None)
                }
            }
        }
        None => (0, None),
    };

    (!has_error).then(|| (r1, imm, r2))
}

pub(super) fn u_32(context: &mut ParseContext, op: &str, full_op: &str, args: &[&str]) -> bool {
    let opcode = match op {
        "LUI" => opcode::U::LUI,
        "AUIPC" => opcode::U::AUIPC,
        _ => return false,
    };

    if let Some((rd, imm)) = ri_args(context, full_op, &args) {
        context.instructions.push(instruction::Info {
            instruction: Instruction::U(instruction::U::new(imm << 12, rd, opcode)),
            len: 4,
        })
    }

    true
}

pub(super) fn j_32(context: &mut ParseContext, op: &str, full_op: &str, args: &[&str]) -> bool {
    let opcode = match op {
        "JAL" => opcode::J::JAL,
        _ => return false,
    };

    if let Some((rd, imm)) = ri_args(context, full_op, &args) {
        let imm = match context.supports_compressed {
            true => imm << 1,
            false => imm << 2,
        };

        context.instructions.push(instruction::Info {
            instruction: Instruction::J(instruction::J::new(imm, rd, opcode)),
            len: 4,
        })
    }

    true
}

fn ri_args(
    context: &mut ParseContext,
    full_op: &str,
    args: &[&str],
) -> Option<(Option<register::RiscV>, u32)> {
    let mut has_error = test_len(context, full_op, 2, args.len());

    let rd = match args.get(0) {
        Some(reg) => match parse_general_purpose_register(reg) {
            Ok(it) => it,
            Err(e) => {
                has_error = true;
                context.errors.push(e);
                None
            }
        },
        None => None,
    };

    let imm = match args.get(1) {
        Some(it) => match parse_immediate(it, 20) {
            Ok(it) => sign_extend_32(it, 20),
            Err(e) => {
                has_error = true;
                context.errors.push(e);
                0
            }
        },
        None => 0,
    };

    (!has_error).then(|| (rd, imm))
}

pub(super) fn sys_32(context: &mut ParseContext, op: &str, full_op: &str, args: &[&str]) -> bool {
    let opcode = match op {
        "EBREAK" => opcode::RSys::EBREAK,
        "ECALL" => opcode::RSys::ECALL,
        _ => return false,
    };

    if !test_len(context, full_op, 0, args.len()) {
        context.instructions.push(instruction::Info {
            instruction: Instruction::Sys(instruction::Sys::new(opcode)),
            len: 4,
        })
    }

    true
}
