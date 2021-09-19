use insta::assert_display_snapshot;
use ribit_core::{instruction, opcode, register, Width};

use super::Context;
use crate::test::MEM_SIZE;
use crate::{Arg, Constant, Id, Instruction, Source, Terminator};

#[test]
fn jalr_link_eq_src() {
    let mut ctx = Context::new(0x1012c, MEM_SIZE);

    super::non_terminal(
        &mut ctx,
        instruction::Instruction::U(instruction::U::new(
            0,
            Some(register::RiscV::X17),
            opcode::U::AUIPC,
        )),
        4,
    );

    super::non_terminal(
        &mut ctx,
        instruction::Instruction::I(instruction::I::new(
            285,
            Some(register::RiscV::X17),
            Some(register::RiscV::X17),
            opcode::I::ADDI,
        )),
        4,
    );

    let block = super::terminal(
        ctx,
        instruction::Instruction::IJump(instruction::IJump::new(
            65279,
            Some(register::RiscV::X17),
            Some(register::RiscV::X17),
            opcode::IJump::JALR,
        )),
        4,
    );

    assert_display_snapshot!(&block.display_instructions());
}

#[test]
fn jalr_bit() {
    let ctx = Context::new(48, MEM_SIZE);

    let block = super::terminal(
        ctx,
        instruction::Instruction::IJump(instruction::IJump {
            imm: 2047,
            rd: Some(register::RiscV::X4),
            rs1: Some(register::RiscV::X1),
            opcode: opcode::IJump::JALR,
        }),
        4,
    );

    assert_display_snapshot!(&block.display_instructions());
}

#[test]
fn jalr_pc() {
    let ctx = Context::new(48, MEM_SIZE);

    let block = super::terminal(
        ctx,
        instruction::Instruction::IJump(instruction::IJump {
            imm: 2046,
            rd: Some(register::RiscV::X4),
            rs1: Some(register::RiscV::X1),
            opcode: opcode::IJump::JALR,
        }),
        4,
    );

    assert_display_snapshot!(&block.display_instructions());
}

#[test]
fn jal_basic() {
    let ctx = Context::new(0, MEM_SIZE);

    let block = super::terminal(
        ctx,
        instruction::Instruction::J(instruction::J {
            imm: 4096,
            rd: Some(register::RiscV::X4),
            opcode: opcode::J::JAL,
        }),
        4,
    );

    assert_display_snapshot!(&block.display_instructions());
}

#[test]
fn sys_break() {
    let ctx = Context::new(0, MEM_SIZE);

    let block = super::terminal(
        ctx,
        instruction::Instruction::Sys(instruction::Sys::new(opcode::RSys::EBREAK)),
        4,
    );

    assert_display_snapshot!(&block.display_instructions());
}

#[test]
fn addi_nop() {
    let mut ctx = Context::new(0, MEM_SIZE);
    super::non_terminal(
        &mut ctx,
        instruction::Instruction::I(instruction::I::new(0, None, None, opcode::I::ADDI)),
        4,
    );

    let block = super::terminal(
        ctx,
        instruction::Instruction::Sys(instruction::Sys::new(opcode::RSys::EBREAK)),
        4,
    );

    assert_display_snapshot!(&block.display_instructions());
}

#[test]
fn branch_0_0_eq() {
    let ctx = Context::new(0, MEM_SIZE);
    let block = super::terminal(
        ctx,
        instruction::Instruction::B(instruction::B::new(1024, None, None, opcode::Cmp::Eq)),
        4,
    );

    assert_display_snapshot!(&block.display_instructions());
}

#[test]
fn branch_0_x1_eq() {
    let ctx = Context::new(0, MEM_SIZE);
    let block = super::terminal(
        ctx,
        instruction::Instruction::B(instruction::B::new(
            1024,
            None,
            Some(register::RiscV::X1),
            opcode::Cmp::Eq,
        )),
        4,
    );

    assert_display_snapshot!(&block.display_instructions());
}

#[test]
fn addi_no_dest() {
    let mut ctx = Context::new(0, MEM_SIZE);
    super::non_terminal(
        &mut ctx,
        instruction::Instruction::I(instruction::I::new(
            50,
            Some(register::RiscV::X1),
            None,
            opcode::I::ADDI,
        )),
        4,
    );

    let block = super::terminal(
        ctx,
        instruction::Instruction::Sys(instruction::Sys::new(opcode::RSys::EBREAK)),
        4,
    );

    assert_display_snapshot!(&block.display_instructions());
}

#[test]
fn mem_read_write() {
    let mut ctx = Context::new(0, MEM_SIZE);
    super::non_terminal(
        &mut ctx,
        instruction::Instruction::IMem(instruction::IMem::new(
            0,
            Some(register::RiscV::X1),
            Some(register::RiscV::X2),
            opcode::IMem::LD(Width::DWord),
        )),
        4,
    );

    super::non_terminal(
        &mut ctx,
        instruction::Instruction::I(instruction::I::new(
            100,
            Some(register::RiscV::X2),
            Some(register::RiscV::X2),
            opcode::I::ADDI,
        )),
        4,
    );

    super::non_terminal(
        &mut ctx,
        instruction::Instruction::S(instruction::S::new(
            50,
            Some(register::RiscV::X2),
            Some(register::RiscV::X1),
            Width::DWord,
        )),
        4,
    );

    let block = super::terminal(
        ctx,
        instruction::Instruction::Sys(instruction::Sys::new(opcode::RSys::EBREAK)),
        4,
    );

    assert_display_snapshot!(&block.display_instructions());
}

#[test]
fn addi_no_src() {
    let mut ctx = Context::new(0, MEM_SIZE);
    super::non_terminal(
        &mut ctx,
        instruction::Instruction::I(instruction::I::new(
            50,
            None,
            Some(register::RiscV::X2),
            opcode::I::ADDI,
        )),
        4,
    );

    let block = super::terminal(
        ctx,
        instruction::Instruction::Sys(instruction::Sys::new(opcode::RSys::EBREAK)),
        4,
    );

    assert_display_snapshot!(&block.display_instructions());
}

#[test]
fn empty() {
    const START_PC: u32 = 0xfefe_fefe;

    let ctx = super::Context::new(START_PC, MEM_SIZE);

    let block = ctx.ret();

    assert_eq!(block.instructions.len(), 2);

    assert_eq!(block.instructions[0], Instruction::Arg { src: Arg::Register, dest: Id(0) });

    assert_eq!(block.instructions[1], Instruction::Arg { src: Arg::Memory, dest: Id(1) });

    assert_eq!(
        block.terminator,
        Terminator::Ret {
            addr: Source::Const(Constant::i32(START_PC)),
            code: Source::Const(Constant::i32(0))
        }
    );
}

#[test]
fn reads_register() {
    let mut ctx = super::Context::new(0, MEM_SIZE);

    ctx.read_register(register::RiscV::X1);
    ctx.read_register(register::RiscV::X2);
    ctx.read_register(register::RiscV::X1);
    let block = ctx.ret();

    assert_eq!(block.instructions.len(), 4);

    assert_eq!(block.instructions[0], Instruction::Arg { dest: Id(0), src: Arg::Register });

    assert_eq!(block.instructions[1], Instruction::Arg { dest: Id(1), src: Arg::Memory });

    let reg_arg = block.arg_ref(Arg::Register).unwrap();

    assert_eq!(
        block.instructions[2],
        Instruction::ReadReg { dest: Id(2), src: register::RiscV::X1, base: Source::Ref(reg_arg) }
    );

    assert_eq!(
        block.instructions[3],
        Instruction::ReadReg { dest: Id(3), src: register::RiscV::X2, base: Source::Ref(reg_arg) }
    );

    assert_eq!(
        block.terminator,
        Terminator::Ret {
            addr: Source::Const(Constant::i32(0)),
            code: Source::Const(Constant::i32(0))
        }
    );
}

#[test]
fn writes_register() {
    let mut ctx = super::Context::new(0, MEM_SIZE);

    ctx.write_register(register::RiscV::X2, Source::Const(Constant::i32(0)));
    let block = ctx.ret();

    assert_eq!(block.instructions.len(), 3);

    assert_eq!(block.instructions[0], Instruction::Arg { src: Arg::Register, dest: Id(0) });

    assert_eq!(block.instructions[1], Instruction::Arg { src: Arg::Memory, dest: Id(1) });

    let reg_arg = block.arg_ref(Arg::Register).unwrap();

    assert_eq!(
        block.instructions[2],
        Instruction::WriteReg {
            dest: register::RiscV::X2,
            src: Source::Const(Constant::i32(0)),
            base: Source::Ref(reg_arg),
        }
    );
}
