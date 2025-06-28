use ribit_core::instruction::Instruction;
use ribit_core::opcode::{self, Cmp};
use ribit_core::{Width, instruction, register};

use super::{
    ParseContext, compressed_integer_register, integer_register, parse_imm_sx, parse_imm_sx32,
    parse_immediate, sign_extend, test_len,
};
use crate::parse::word::Word;

const NOP: instruction::I = instruction::I::new(0, None, None, opcode::I::ADDI);
const RET: instruction::IJump =
    instruction::IJump::new(0, Some(register::RiscV::X1), None, opcode::IJump::JALR);

fn compressed_pseudo_no_args<I: Into<Instruction>>(
    context: &mut ParseContext,
    instruction: I,
    full_op: &str,
    args: &[&str],
) {
    fn inner(context: &mut ParseContext, instruction: Instruction, full_op: &str, argc: usize) {
        if test_len(context, full_op, 0, argc) {
            return;
        }

        context.push16(instruction);
    }

    inner(context, instruction.into(), full_op, args.len());
}

fn pseudo_no_args<I: Into<Instruction>>(
    context: &mut ParseContext,
    instruction: I,
    full_op: &str,
    args: &[&str],
) {
    fn inner(context: &mut ParseContext, instruction: Instruction, full_op: &str, argc: usize) {
        if test_len(context, full_op, 0, argc) {
            return;
        }

        context.push32(instruction);
    }

    inner(context, instruction.into(), full_op, args.len());
}

pub(super) fn nop(context: &mut ParseContext, full_op: &str, args: &[&str]) {
    pseudo_no_args(context, NOP, full_op, args)
}

pub(super) fn ret(context: &mut ParseContext, full_op: &str, args: &[&str]) {
    pseudo_no_args(context, RET, full_op, args)
}

pub(super) fn r_32(context: &mut ParseContext, op: opcode::R, full_op: &str, args: &[&str]) {
    if let Some([rd, rs1, rs2]) = r_args(context, full_op, args) {
        context.push32(instruction::R::new(rs1, rs2, rd, op));
    }
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

pub(super) fn i_32(context: &mut ParseContext, op: opcode::I, full_op: &str, args: &[&str]) {
    if let Some(([rd, rs1], imm)) = i_args(context, full_op, args) {
        context.push32(instruction::I::new(imm, rs1, rd, op));
    }
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

pub(super) fn ijump_32(
    context: &mut ParseContext,
    op: opcode::IJump,
    full_op: &str,
    args: &[&str],
) {
    if let Some((rd, imm, rs1)) = rir_args(context, full_op, args, 12) {
        context.push32(instruction::IJump::new(imm, rs1, rd, op));
    }
}

pub(super) fn imem_32(context: &mut ParseContext, op: opcode::IMem, full_op: &str, args: &[&str]) {
    if op == opcode::IMem::FENCE {
        if !test_len(context, full_op, 0, args.len()) {
            context.push32(instruction::IMem::new(0, None, None, op));
        }

        return;
    }

    if let Some((rd, imm, rs1)) = rir_args(context, full_op, args, 12) {
        context.push32(instruction::IMem::new(imm, rs1, rd, op));
    }
}

pub(super) fn s_32(context: &mut ParseContext, op: Width, full_op: &str, args: &[&str]) {
    if let Some((rs2, imm, rs1)) = rir_args(context, full_op, args, 12) {
        context.push32(instruction::S::new(imm, rs1, rs2, op));
    }
}

pub(super) fn b_32(context: &mut ParseContext, op: Cmp, full_op: &str, args: &[&str]) {
    if let Some((rs1, imm, rs2)) = rir_args(context, full_op, args, 12) {
        let imm = match context.supports_compressed {
            true => imm << 1,
            false => imm << 2,
        };

        context.push32(instruction::B::new(imm, rs1, rs2, op));
    }
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

pub(super) fn u_32(context: &mut ParseContext, op: opcode::U, full_op: &str, args: &[&str]) {
    if let Some((rd, imm)) = ri_args(context, full_op, args, 20) {
        context.push32(instruction::U::new(imm << 12, rd, op));
    }
}

pub(super) fn j_32(context: &mut ParseContext, op: opcode::J, full_op: &str, args: &[&str]) {
    if let Some((rd, imm)) = ri_args(context, full_op, args, 20) {
        let imm = match context.supports_compressed {
            true => imm << 1,
            false => imm << 2,
        };

        context.push32(instruction::J::new(imm, rd, op));
    }
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

pub(super) fn sys_32(context: &mut ParseContext, op: opcode::RSys, full_op: &str, args: &[&str]) {
    if !test_len(context, full_op, 0, args.len()) {
        context.push32(instruction::Sys::new(op));
    }
}

fn compressed_jump(
    context: &mut ParseContext,
    rd: Option<register::RiscV>,
    full_op: &str,
    args: &[&str],
) {
    if let Some(imm) = compressed_jump_args(context, full_op, args) {
        context.push16(instruction::J::new(imm << 1, rd, opcode::J::JAL));
    }
}

pub(super) fn compressed(
    context: &mut ParseContext,
    op: Word,
    full_op: &str,
    args: &[&str],
) -> bool {
    if !context.supports_compressed {
        context.errors.push(format!(
            "`{full_op}` is not a valid opcode (hint: compressed instructions aren't enabled)",
        ));
        return true;
    }

    match op {
        Word::Op(Instruction::IMem(op @ opcode::IMem::LD(Width::DWord))) => {
            if let Some((rd, imm, rs1)) = rir_args_16(context, full_op, args, 5, false) {
                let imm = imm << 2;

                context.push16(instruction::IMem::new(imm, rs1, rd, op));
            }
        }
        Word::Op(Instruction::J(opcode::J::JAL)) => {
            compressed_jump(context, Some(register::RiscV::X1), full_op, args);
        }
        Word::J => {
            compressed_jump(context, None, full_op, args);
        }
        Word::Ret => compressed_pseudo_no_args(context, RET, full_op, args),
        Word::Nop => compressed_pseudo_no_args(context, NOP, full_op, args),
        Word::Li => {
            if let Some((rd, imm)) = ri_args(context, full_op, args, 6) {
                if rd.is_none() {
                    context.errors.push(format!("`x0` is not a valid register for `li`"));
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
