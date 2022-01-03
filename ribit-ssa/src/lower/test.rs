use expect_test::expect;
use ribit_core::{instruction, opcode, register};

use super::Context;
use crate::tests::MEM_SIZE;
use crate::{AnySource, Arg, Constant, Id, Instruction, Terminator};

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

    expect![[r#"
        %0 = args[0]
        %1 = args[1]
        x(%0)17 = 00010138
        ret 00000000, 00010148"#]]
    .assert_eq(&block.display_instructions().to_string())
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

    expect![[r#"
        %0 = args[0]
        %1 = args[1]
        %2 = x(%0)1
        %3 = add %2, 000007ff
        %4 = and %3, fffffffe
        x(%0)4 = 00000034
        ret 00000000, %4"#]]
    .assert_eq(&block.display_instructions().to_string())
}

#[test]
fn jalr_pc() {
    let block = crate::tests::assemble_block("jalr x4, 1023(x1)");

    expect![[r#"
        %0 = args[0]
        %1 = args[1]
        %2 = x(%0)1
        %3 = add %2, 000003ff
        %4 = and %3, fffffffe
        x(%0)4 = 00000404
        ret 00000000, %4"#]]
    .assert_eq(&block.display_instructions().to_string())
}

#[test]
fn jal_basic() {
    let block = crate::tests::assemble_block("JAL x4, 2048");

    expect![[r#"
        %0 = args[0]
        %1 = args[1]
        x(%0)4 = 00000404
        ret 00000000, 00001400"#]]
    .assert_eq(&block.display_instructions().to_string())
}

#[test]
fn sys_break() {
    let block = crate::tests::assemble_block("ebreak");

    expect![[r#"
        %0 = args[0]
        %1 = args[1]
        ret 00000001, 00000404"#]]
    .assert_eq(&block.display_instructions().to_string())
}

#[test]
fn addi_nop() {
    let block = crate::tests::assemble_block(
        r#"
            c.nop
            ebreak
        "#,
    );

    expect![[r#"
        %0 = args[0]
        %1 = args[1]
        ret 00000001, 00000406"#]]
    .assert_eq(&block.display_instructions().to_string())
}

#[test]
fn branch_0_0_eq() {
    let block = crate::tests::assemble_block(r#"beq x0, 512(x0)"#);

    expect![[r#"
        %0 = args[0]
        %1 = args[1]
        ret 00000000, 00000800"#]]
    .assert_eq(&block.display_instructions().to_string())
}

#[test]
fn branch_0_x1_eq() {
    let block = crate::tests::assemble_block(r#"beq x0, 512(x1)"#);

    expect![[r#"
        %0 = args[0]
        %1 = args[1]
        %2 = x(%0)1
        %3 = cmp EQ 00000000, %2
        %4 = select %3, 00000800, 00000404
        ret 00000000, %4"#]]
    .assert_eq(&block.display_instructions().to_string())
}

#[test]
fn addi_no_dest() {
    let block = crate::tests::assemble_block(
        r#"
            addi x0, x1, 50
            ebreak
        "#,
    );

    expect![[r#"
        %0 = args[0]
        %1 = args[1]
        %2 = x(%0)1
        %3 = add %2, 00000032
        ret 00000001, 00000408"#]]
    .assert_eq(&block.display_instructions().to_string())
}

#[test]
fn mem_read_write() {
    let block = crate::tests::assemble_block(
        r#"
            LW x2, 0(x1)
            addi x2, x2, 100
            SW x2, 50(x1)
            ebreak
        "#,
    );

    expect![[r#"
        %0 = args[0]
        %1 = args[1]
        %2 = x(%0)1
        %3 = and %2, 00ffffff
        %4 = signed dword m(%1)%3
        %5 = add %4, 00000064
        %6 = add %2, 00000032
        %7 = and %6, 00ffffff
        m(%1)%7 = dword %5
        x(%0)2 = %5
        ret 00000001, 00000410"#]]
    .assert_eq(&block.display_instructions().to_string())
}

#[test]
fn addi_no_src() {
    let block = crate::tests::assemble_block(
        r#"
            addi x2, x0, 50
            ebreak
        "#,
    );

    expect![[r#"
        %0 = args[0]
        %1 = args[1]
        x(%0)2 = 00000032
        ret 00000001, 00000408"#]]
    .assert_eq(&block.display_instructions().to_string())
}

/// ensure results of slt[u] can be used in other instructions
#[test]
fn cmp_add() {
    let block = crate::tests::assemble_block(
        r#"
            SLT x2, x2, x3
            ADD x2, x2, x4
            ebreak
        "#,
    );

    expect![[r#"
        %0 = args[0]
        %1 = args[1]
        %2 = x(%0)2
        %3 = x(%0)3
        %4 = cmp SL %2, %3
        %5 = zext dword %4
        %6 = x(%0)4
        %7 = add %5, %6
        x(%0)2 = %7
        ret 00000001, 0000040c"#]]
    .assert_eq(&block.display_instructions().to_string())
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
            addr: AnySource::Const(Constant::i32(START_PC)),
            code: AnySource::Const(Constant::i32(0))
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
        Instruction::ReadReg { dest: Id(2), src: register::RiscV::X1, base: reg_arg.into() }
    );

    assert_eq!(
        block.instructions[3],
        Instruction::ReadReg { dest: Id(3), src: register::RiscV::X2, base: reg_arg.into() }
    );

    assert_eq!(
        block.terminator,
        Terminator::Ret {
            addr: AnySource::Const(Constant::i32(0)),
            code: AnySource::Const(Constant::i32(0))
        }
    );
}

#[test]
fn writes_register() {
    let mut ctx = super::Context::new(0, MEM_SIZE);

    ctx.write_register(register::RiscV::X2, AnySource::Const(Constant::i32(0)));
    let block = ctx.ret();

    assert_eq!(block.instructions.len(), 3);

    assert_eq!(block.instructions[0], Instruction::Arg { src: Arg::Register, dest: Id(0) });

    assert_eq!(block.instructions[1], Instruction::Arg { src: Arg::Memory, dest: Id(1) });

    let reg_arg = block.arg_ref(Arg::Register).unwrap();

    assert_eq!(
        block.instructions[2],
        Instruction::WriteReg {
            dest: register::RiscV::X2,
            src: AnySource::Const(Constant::i32(0)),
            base: reg_arg.into(),
        }
    );
}
