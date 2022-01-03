use expect_test::expect;
use ribit_core::{instruction, opcode, register};

use super::Context;
use crate::tests::{expect_block, MEM_SIZE};
use crate::{AnySource, Constant};

/// ensure register writes get placed *after* register reads, but *before* return.
#[test]
fn jalr_link_eq_src() {
    let block = crate::tests::assemble_block_with_context(
        Context::new(0x1012c, MEM_SIZE),
        r"
            auipc x17, 0
            addi x17, x17, 285
            jalr x17, 0xf00(x17)
        ",
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

    // note that `imm` here is talking about a literal byte value, this is an *impossible* instruction.
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
    expect_block(
        "jalr x4, 1023(x1)",
        expect![[r#"
        %0 = args[0]
        %1 = args[1]
        %2 = x(%0)1
        %3 = add %2, 000003ff
        %4 = and %3, fffffffe
        x(%0)4 = 00000404
        ret 00000000, %4"#]],
    );
}

#[test]
fn jal_basic() {
    expect_block(
        "jal x4, 2048",
        expect![[r#"
        %0 = args[0]
        %1 = args[1]
        x(%0)4 = 00000404
        ret 00000000, 00001400"#]],
    );
}

#[test]
fn sys_break() {
    expect_block(
        "ebreak",
        expect![[r#"
        %0 = args[0]
        %1 = args[1]
        ret 00000001, 00000404"#]],
    );
}

#[test]
fn addi_nop() {
    expect_block(
        r#"
            c.nop
            ebreak
        "#,
        expect![[r#"
            %0 = args[0]
            %1 = args[1]
            ret 00000001, 00000406"#]],
    );
}

#[test]
fn branch_0_0_eq() {
    expect_block(
        "beq x0, 512(x0)",
        expect![[r#"
        %0 = args[0]
        %1 = args[1]
        ret 00000000, 00000800"#]],
    );
}

#[test]
fn branch_0_x1_eq() {
    expect_block(
        "beq x0, 512(x1)",
        expect![[r#"
        %0 = args[0]
        %1 = args[1]
        %2 = x(%0)1
        %3 = cmp EQ 00000000, %2
        %4 = select %3, 00000800, 00000404
        ret 00000000, %4"#]],
    );
}

#[test]
fn addi_no_dest() {
    expect_block(
        r#"
            addi x0, x1, 50
            ebreak
        "#,
        expect![[r#"
            %0 = args[0]
            %1 = args[1]
            %2 = x(%0)1
            %3 = add %2, 00000032
            ret 00000001, 00000408"#]],
    );
}

#[test]
fn mem_read_write() {
    expect_block(
        r#"
            lw x2, 0(x1)
            addi x2, x2, 100
            sw x2, 50(x1)
            ebreak
        "#,
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
            ret 00000001, 00000410"#]],
    );
}

#[test]
fn addi_no_src() {
    expect_block(
        r#"
            addi x2, x0, 50
            ebreak
        "#,
        expect![[r#"
            %0 = args[0]
            %1 = args[1]
            x(%0)2 = 00000032
            ret 00000001, 00000408"#]],
    );
}

/// ensure results of slt[u] can be used in other instructions
#[test]
fn cmp_add() {
    expect_block(
        r#"
            slt x2, x2, x3
            add x2, x2, x4
            ebreak
        "#,
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
            ret 00000001, 0000040c"#]],
    );
}

#[test]
fn empty() {
    let ctx = super::Context::new(0xfefe_fefe, MEM_SIZE);

    let block = ctx.ret();

    expect![[r#"
        %0 = args[0]
        %1 = args[1]
        ret 00000000, fefefefe"#]]
    .assert_eq(&block.display_instructions().to_string());
}

#[test]
fn reads_register() {
    let mut ctx = super::Context::new(0, MEM_SIZE);

    ctx.read_register(register::RiscV::X1);
    ctx.read_register(register::RiscV::X2);
    ctx.read_register(register::RiscV::X1);
    let block = ctx.ret();

    expect![[r#"
        %0 = args[0]
        %1 = args[1]
        %2 = x(%0)1
        %3 = x(%0)2
        ret 00000000, 00000000"#]]
    .assert_eq(&block.display_instructions().to_string());
}

#[test]
fn writes_register() {
    let mut ctx = super::Context::new(0, MEM_SIZE);

    ctx.write_register(register::RiscV::X2, AnySource::Const(Constant::i32(0)));
    let block = ctx.ret();

    expect![[r#"
        %0 = args[0]
        %1 = args[1]
        x(%0)2 = 00000000
        ret 00000000, 00000000"#]]
    .assert_eq(&block.display_instructions().to_string());
}
