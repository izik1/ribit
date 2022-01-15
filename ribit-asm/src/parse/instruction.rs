use ribit_core::instruction::{self, Instruction};
use ribit_core::opcode::{self, Cmp};
use ribit_core::{register, Width};

use super::{
    parse_compressed_register, parse_general_purpose_register, parse_immediate, sign_extend,
    sign_extend_32, test_len, ParseContext,
};

pub(super) fn r_32(context: &mut ParseContext, op: &str, full_op: &str, args: &[&str]) -> bool {
    let opcode = match op {
        "add" | "ADD" => (opcode::R::ADD),
        "sub" | "SUB" => (opcode::R::SUB),
        "sll" | "SLL" => (opcode::R::SLL),
        "slt" | "SLT" => (opcode::R::SCond(Cmp::Lt)),
        "sltu" | "SLTU" => (opcode::R::SCond(Cmp::Ltu)),
        "xor" | "XOR" => (opcode::R::XOR),
        "srl" | "SRL" => (opcode::R::SRL),
        "sra" | "SRA" => (opcode::R::SRA),
        "or" | "OR" => (opcode::R::OR),
        "and" | "AND" => (opcode::R::AND),
        "mul" | "MUL" => (opcode::R::MUL),
        "mulh" | "MULH" => (opcode::R::MULH),
        "mulhsu" | "MULHSU" => (opcode::R::MULHSU),
        "mulhu" | "MULHU" => (opcode::R::MULHU),
        "div" | "DIV" => (opcode::R::DIV),
        "divu" | "DIVU" => (opcode::R::DIVU),
        "rem" | "REM" => (opcode::R::REM),
        "remu" | "REMU" => (opcode::R::REMU),
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
        "addi" | "ADDI" => opcode::I::ADDI,
        "slti" | "SLTI" => opcode::I::SICond(Cmp::Lt),
        "sltiu" | "SLTIU" => opcode::I::SICond(Cmp::Ltu),
        "xori" | "XORI" => opcode::I::XORI,
        "ori" | "ORI" => opcode::I::ORI,
        "andi" | "ANDI" => opcode::I::ANDI,
        "slli" | "SLLI" => opcode::I::SLLI,
        "srli" | "SRLI" => opcode::I::SRLI,
        "srai" | "SRAI" => opcode::I::SRAI,
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
        "jalr" | "JALR" => opcode::IJump::JALR,
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
        "fence" | "FENCE" => opcode::IMem::FENCE,
        "lb" | "LB" => opcode::IMem::LD(Width::Byte),
        "lh" | "LH" => opcode::IMem::LD(Width::Word),
        "lw" | "LW" => opcode::IMem::LD(Width::DWord),
        "lbu" | "LBU" => opcode::IMem::LDU(Width::Byte),
        "lhu" | "LHU" => opcode::IMem::LDU(Width::Word),
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
        "sb" | "SB" => Width::Byte,
        "sh" | "SH" => Width::Word,
        "sw" | "SW" => Width::DWord,
        _ => return false,
    };

    if let Some((rs2, imm, rs1)) = rir_args(context, full_op, &args, 12) {
        context.instructions.push(instruction::Info {
            instruction: Instruction::S(instruction::S::new(imm, rs1, rs2, width)),
            len: 4,
        })
    }

    true
}

pub(super) fn b_32(context: &mut ParseContext, op: &str, full_op: &str, args: &[&str]) -> bool {
    let cmp = match op {
        "beq" | "BEQ" => Cmp::Eq,
        "bne" | "BNE" => Cmp::Ne,
        "blt" | "BLT" => Cmp::Lt,
        "bltu" | "BLTU" => Cmp::Ltu,
        "bge" | "BGE" => Cmp::Ge,
        "bgeu" | "BGEU" => Cmp::Geu,
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
                        .push(format!("expected an arg in the format 0(x2), found `{it}`"));

                    (0, None)
                }
            }
        }
        None => (0, None),
    };

    (!has_error).then(|| (r1, imm, r2))
}

pub(super) fn rir_args_16(
    context: &mut ParseContext,
    full_op: &str,
    args: &[&str],
    immediate_width: u8,
    sign_extend_imm: bool,
) -> Option<(Option<register::RiscV>, u16, Option<register::RiscV>)> {
    let mut has_error = test_len(context, full_op, 2, args.len());

    let r1 = match args.get(0) {
        Some(reg) => match parse_compressed_register(reg) {
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
                        Ok(it) if sign_extend_imm => sign_extend(it as u16, immediate_width),
                        Ok(it) => it as u16,
                        Err(e) => {
                            has_error = true;
                            context.errors.push(e);
                            0
                        }
                    };

                    let r2 = match parse_compressed_register(r2) {
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
                        .push(format!("expected an arg in the format 0(x16), found `{it}`"));

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
        "lui" | "LUI" => opcode::U::LUI,
        "auipc" | "AUIPC" => opcode::U::AUIPC,
        _ => return false,
    };

    if let Some((rd, imm)) = ri_args(context, full_op, &args, 20) {
        context.instructions.push(instruction::Info {
            instruction: Instruction::U(instruction::U::new(imm << 12, rd, opcode)),
            len: 4,
        })
    }

    true
}

pub(super) fn j_32(context: &mut ParseContext, op: &str, full_op: &str, args: &[&str]) -> bool {
    let opcode = match op {
        "jal" | "JAL" => opcode::J::JAL,
        _ => return false,
    };

    if let Some((rd, imm)) = ri_args(context, full_op, &args, 20) {
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
    imm_bits: u8,
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
        Some(it) => match parse_immediate(it, imm_bits) {
            Ok(it) => sign_extend_32(it, imm_bits),
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
        "ebreak" | "EBREAK" => opcode::RSys::EBREAK,
        "ecall" | "ECALL" => opcode::RSys::ECALL,
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

fn compressed_jump(context: &mut ParseContext, op: &str, full_op: &str, args: &[&str]) -> bool {
    let rd = match op {
        "j" | "J" => None,
        "jal" | "JAL" => Some(register::RiscV::X1),
        _ => return false,
    };

    if let Some(imm) = compressed_jump_args(context, full_op, args) {
        context.instructions.push(instruction::Info {
            instruction: Instruction::J(instruction::J::new(imm << 1, rd, opcode::J::JAL)),
            len: 2,
        });
    }

    true
}

pub(super) fn compressed(
    context: &mut ParseContext,
    op: &str,
    full_op: &str,
    args: &[&str],
) -> bool {
    if !context.supports_compressed {
        context.errors.push(format!(
            "`{full_op}` is not a valid opcode (hint: compressed instructions aren't enabled)",
        ));
        return true;
    }

    if compressed_jump(context, op, full_op, args) {
        return true;
    }

    match op {
        // "lwsp" | "LWSP" => {}
        "lw" | "LW" => {
            if let Some((rd, imm, rs1)) = rir_args_16(context, full_op, args, 5, false) {
                let imm = imm << 2;

                context.instructions.push(instruction::Info {
                    instruction: Instruction::IMem(instruction::IMem::new(
                        imm,
                        rs1,
                        rd,
                        opcode::IMem::LD(Width::DWord),
                    )),
                    len: 2,
                })
            }
        }

        "nop" | "NOP" => {
            if !test_len(context, full_op, 0, args.len()) {
                context.instructions.push(instruction::Info {
                    instruction: Instruction::I(instruction::I::new(
                        0,
                        None,
                        None,
                        opcode::I::ADDI,
                    )),
                    len: 2,
                })
            }
        }

        "ret" | "RET" => {
            if !test_len(context, full_op, 0, args.len()) {
                context.instructions.push(instruction::Info {
                    instruction: Instruction::IJump(instruction::IJump::new(
                        0,
                        Some(register::RiscV::X1),
                        None,
                        opcode::IJump::JALR,
                    )),
                    len: 2,
                })
            }
        }

        "li" | "LI" => {
            if let Some((rd, imm)) = ri_args(context, full_op, &args, 6) {
                if rd.is_none() {
                    context.errors.push(format!("`x0` is not a valid register for `{op}`"));
                    return true;
                }

                let imm = imm as u16;
                context.instructions.push(instruction::Info {
                    instruction: Instruction::I(instruction::I::new(
                        imm,
                        None,
                        rd,
                        opcode::I::ADDI,
                    )),
                    len: 2,
                });
            }
        }
        _ => return false,
    };

    true
}

fn compressed_jump_args(context: &mut ParseContext, full_op: &str, args: &[&str]) -> Option<u32> {
    let mut has_error = test_len(context, full_op, 1, args.len());

    let imm = match args.get(0) {
        Some(it) => match parse_immediate(it, 10) {
            Ok(it) => sign_extend_32(it, 10),
            Err(e) => {
                has_error = true;
                context.errors.push(e);
                0
            }
        },
        None => 0,
    };

    (!has_error).then(|| imm)
}
