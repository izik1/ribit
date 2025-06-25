use ribit_core::opcode::{self, Cmp, SCmp};
use ribit_core::{Width, instruction, register};

use super::{
    ParseContext, compressed_integer_register, integer_register, parse_imm_sx, parse_imm_sx32,
    parse_immediate, sign_extend, test_len,
};

pub(super) fn r_32(context: &mut ParseContext, op: &str, full_op: &str, args: &[&str]) -> bool {
    let opcode = match op {
        "add" | "ADD" => opcode::R::ADD,
        "sub" | "SUB" => opcode::R::SUB,
        "sll" | "SLL" => opcode::R::SLL,
        "slt" | "SLT" => opcode::R::SCond(SCmp::Lt),
        "sltu" | "SLTU" => opcode::R::SCond(SCmp::Ltu),
        "xor" | "XOR" => opcode::R::XOR,
        "srl" | "SRL" => opcode::R::SRL,
        "sra" | "SRA" => opcode::R::SRA,
        "or" | "OR" => opcode::R::OR,
        "and" | "AND" => opcode::R::AND,
        "mul" | "MUL" => opcode::R::MUL,
        "mulh" | "MULH" => opcode::R::MULH,
        "mulhsu" | "MULHSU" => opcode::R::MULHSU,
        "mulhu" | "MULHU" => opcode::R::MULHU,
        "div" | "DIV" => opcode::R::DIV,
        "divu" | "DIVU" => opcode::R::DIVU,
        "rem" | "REM" => opcode::R::REM,
        "remu" | "REMU" => opcode::R::REMU,
        _ => return false,
    };

    if let Some([rd, rs1, rs2]) = r_args(context, full_op, args) {
        context.push32(instruction::R::new(rs1, rs2, rd, opcode));
    }

    true
}

fn r_args(
    context: &mut ParseContext,
    full_op: &str,
    args: &[&str],
) -> Option<[Option<register::RiscV>; 3]> {
    let len_err = test_len(context, full_op, 3, args.len());
    let mut args = args.iter();

    let rd = context.consume(integer_register(args.next()?));
    let rs1 = context.consume(integer_register(args.next()?));
    let rs2 = context.consume(integer_register(args.next()?));

    match len_err {
        false => Some([rd?, rs1?, rs2?]),
        true => None,
    }
}

pub(super) fn i_32(context: &mut ParseContext, op: &str, full_op: &str, args: &[&str]) -> bool {
    let opcode = match op {
        "addi" | "ADDI" => opcode::I::ADDI,
        "slti" | "SLTI" => opcode::I::SICond(SCmp::Lt),
        "sltiu" | "SLTIU" => opcode::I::SICond(SCmp::Ltu),
        "xori" | "XORI" => opcode::I::XORI,
        "ori" | "ORI" => opcode::I::ORI,
        "andi" | "ANDI" => opcode::I::ANDI,
        "slli" | "SLLI" => opcode::I::SLLI,
        "srli" | "SRLI" => opcode::I::SRLI,
        "srai" | "SRAI" => opcode::I::SRAI,
        _ => return false,
    };

    if let Some(([rd, rs1], imm)) = i_args(context, full_op, args) {
        context.push32(instruction::I::new(imm, rs1, rd, opcode));
    }

    true
}

fn i_args(
    context: &mut ParseContext,
    full_op: &str,
    args: &[&str],
) -> Option<([Option<register::RiscV>; 2], u16)> {
    let len_err = test_len(context, full_op, 3, args.len());
    let mut args = args.iter();

    let rd = context.consume(integer_register(args.next()?));
    let rs1 = context.consume(integer_register(args.next()?));
    let imm = context.consume(parse_imm_sx(args.next()?, 12));

    match len_err {
        false => Some(([rd?, rs1?], imm?)),
        true => None,
    }
}

pub(super) fn ijump_32(context: &mut ParseContext, op: &str, full_op: &str, args: &[&str]) -> bool {
    let opcode = match op {
        "jalr" | "JALR" => opcode::IJump::JALR,
        _ => return false,
    };

    if let Some((rd, imm, rs1)) = rir_args(context, full_op, args, 12) {
        context.push32(instruction::IJump::new(imm, rs1, rd, opcode));
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
            context.push32(instruction::IMem::new(0, None, None, opcode));
        }

        return true;
    }

    if let Some((rd, imm, rs1)) = rir_args(context, full_op, args, 12) {
        context.push32(instruction::IMem::new(imm, rs1, rd, opcode));
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

    if let Some((rs2, imm, rs1)) = rir_args(context, full_op, args, 12) {
        context.push32(instruction::S::new(imm, rs1, rs2, width));
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

    if let Some((rs1, imm, rs2)) = rir_args(context, full_op, args, 12) {
        let imm = match context.supports_compressed {
            true => imm << 1,
            false => imm << 2,
        };

        context.push32(instruction::B::new(imm, rs1, rs2, cmp));
    }

    true
}

fn split_offset_index(arg: &str) -> Result<(&str, &str), String> {
    arg.split_once('(')
        .and_then(|(imm, r)| r.strip_suffix(')').map(|r| (imm, r)))
        .ok_or_else(|| format!("expected an arg in the format 0(x16), found `{arg}`"))
}

fn rir_args(
    context: &mut ParseContext,
    full_op: &str,
    args: &[&str],
    immediate_width: u8,
) -> Option<(Option<register::RiscV>, u16, Option<register::RiscV>)> {
    let len_err = test_len(context, full_op, 2, args.len());
    let mut args = args.iter();

    let r1 = context.consume(integer_register(args.next()?));

    let (offset, index) = context.consume(split_offset_index(args.next()?))?;

    let offset = context.consume(parse_imm_sx(offset, immediate_width));
    let index = context.consume(integer_register(index));

    match len_err {
        false => Some((r1?, offset?, index?)),
        true => None,
    }
}

pub(super) fn rir_args_16(
    context: &mut ParseContext,
    full_op: &str,
    args: &[&str],
    immediate_width: u8,
    sign_extend_imm: bool,
) -> Option<(Option<register::RiscV>, u16, Option<register::RiscV>)> {
    let len_err = test_len(context, full_op, 2, args.len());
    let mut args = args.iter();

    let r1 = context.consume(compressed_integer_register(args.next()?));

    let (offset, index) = context.consume(split_offset_index(args.next()?))?;

    let offset = context.consume(parse_immediate(offset, immediate_width)).map(|it| {
        if sign_extend_imm { sign_extend(it as u16, immediate_width) } else { it as u16 }
    });
    let index = context.consume(compressed_integer_register(index));

    match len_err {
        false => Some((r1?, offset?, index?)),
        true => None,
    }
}

pub(super) fn u_32(context: &mut ParseContext, op: &str, full_op: &str, args: &[&str]) -> bool {
    let opcode = match op {
        "lui" | "LUI" => opcode::U::LUI,
        "auipc" | "AUIPC" => opcode::U::AUIPC,
        _ => return false,
    };

    if let Some((rd, imm)) = ri_args(context, full_op, args, 20) {
        context.push32(instruction::U::new(imm << 12, rd, opcode));
    }

    true
}

pub(super) fn j_32(context: &mut ParseContext, op: &str, full_op: &str, args: &[&str]) -> bool {
    let opcode = match op {
        "jal" | "JAL" => opcode::J::JAL,
        _ => return false,
    };

    if let Some((rd, imm)) = ri_args(context, full_op, args, 20) {
        let imm = match context.supports_compressed {
            true => imm << 1,
            false => imm << 2,
        };

        context.push32(instruction::J::new(imm, rd, opcode));
    }

    true
}

fn ri_args(
    context: &mut ParseContext,
    full_op: &str,
    args: &[&str],
    imm_bits: u8,
) -> Option<(Option<register::RiscV>, u32)> {
    let len_err = test_len(context, full_op, 2, args.len());
    let mut args = args.iter();

    let rd = context.consume(integer_register(args.next()?));
    let imm = context.consume(parse_imm_sx32(args.next()?, imm_bits));

    match len_err {
        false => Some((rd?, imm?)),
        true => None,
    }
}

pub(super) fn sys_32(context: &mut ParseContext, op: &str, full_op: &str, args: &[&str]) -> bool {
    let opcode = match op {
        "ebreak" | "EBREAK" => opcode::RSys::EBREAK,
        "ecall" | "ECALL" => opcode::RSys::ECALL,
        _ => return false,
    };

    if !test_len(context, full_op, 0, args.len()) {
        context.push32(instruction::Sys::new(opcode));
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
        context.push16(instruction::J::new(imm << 1, rd, opcode::J::JAL));
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

                context.push16(instruction::IMem::new(
                    imm,
                    rs1,
                    rd,
                    opcode::IMem::LD(Width::DWord),
                ));
            }
        }

        "nop" | "NOP" => {
            if !test_len(context, full_op, 0, args.len()) {
                context.push16(instruction::I::new(0, None, None, opcode::I::ADDI));
            }
        }

        "ret" | "RET" => {
            if !test_len(context, full_op, 0, args.len()) {
                context.push16(instruction::IJump::new(
                    0,
                    Some(register::RiscV::X1),
                    None,
                    opcode::IJump::JALR,
                ));
            }
        }

        "li" | "LI" => {
            if let Some((rd, imm)) = ri_args(context, full_op, args, 6) {
                if rd.is_none() {
                    context.errors.push(format!("`x0` is not a valid register for `{op}`"));
                    return true;
                }

                let imm = imm as u16;
                context.push16(instruction::I::new(imm, None, rd, opcode::I::ADDI));
            }
        }
        _ => return false,
    }

    true
}

fn compressed_jump_args(context: &mut ParseContext, full_op: &str, args: &[&str]) -> Option<u32> {
    let len_err = test_len(context, full_op, 1, args.len());
    let mut args = args.iter();

    let imm = context.consume(parse_imm_sx32(args.next()?, 10));

    match len_err {
        false => imm,
        true => None,
    }
}
