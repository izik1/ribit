use std::fmt::Write;

use expect_test::expect;
use ribit_core::instruction;

fn print_instructions(instructions: &[instruction::Info]) -> String {
    let mut output = String::new();
    for instruction in instructions {
        let fmt = ribit_core::disassemble::FmtInstruction::from_info(instruction);
        writeln!(&mut output, "{}", fmt).unwrap();
    }

    output
}

fn assert_instructions(input: &str, expected: expect_test::Expect) {
    let result = super::tokenize(input, true);

    for error in &result.errors {
        eprintln!("error: {}", error);
    }

    if !result.errors.is_empty() {
        panic!("failing due to previous error(s)");
    }

    expected.assert_eq(&print_instructions(&result.instructions));
}

#[test]
fn basic_r_nop() {
    assert_instructions(
        r#"
            ADD x0, x0, x0
            ADD x0, x1, x0
            SUB x0, x0, x0
        "#,
        expect![[r#"
            ADD x0, x0, x0
            ADD x0, x1, x0
            SUB x0, x0, x0
        "#]],
    );
}

#[test]
fn basic_imm() {
    // so many literals, still not even enough.
    // todo: do a generator style "everything"
    assert_instructions(
        r#"
            ADDI x0, x0, 0
            ADDI x0, x0, -0
            ADDI x0, x0, 0h
            ADDI x0, x0, -0h
            ADDI x0, x0, 0x0
            ADDI x0, x0, -0x0
            ADDI x0, x0, 0o0
            ADDI x0, x0, -0o0
            ADDI x0, x0, 0b0
            ADDI x0, x0, -0b0
            ADDI x0, x0, 0b
            ADDI x0, x0, -0b
            ADDI x0, x0, 4095
            ADDI x0, x0, -2048
            ADDI x0, x0, fffh
            ADDI x0, x0, -800h
            ADDI x0, x0, 0xfff
            ADDI x0, x0, -0x800
            ADDI x0, x0, 0o7777
            ADDI x0, x0, -0o4000
            ADDI x0, x0,  0b111111111111
            ADDI x0, x0, -0b100000000000
            ADDI x0, x0,  111111111111b
            ADDI x0, x0, -100000000000b
        "#,
        expect![[r#"
            ADDI x0, x0, 000
            ADDI x0, x0, 000
            ADDI x0, x0, 000
            ADDI x0, x0, 000
            ADDI x0, x0, 000
            ADDI x0, x0, 000
            ADDI x0, x0, 000
            ADDI x0, x0, 000
            ADDI x0, x0, 000
            ADDI x0, x0, 000
            ADDI x0, x0, 000
            ADDI x0, x0, 000
            ADDI x0, x0, fff
            ADDI x0, x0, 800
            ADDI x0, x0, fff
            ADDI x0, x0, 800
            ADDI x0, x0, fff
            ADDI x0, x0, 800
            ADDI x0, x0, fff
            ADDI x0, x0, 800
            ADDI x0, x0, fff
            ADDI x0, x0, 800
            ADDI x0, x0, fff
            ADDI x0, x0, 800
        "#]],
    );
}

#[test]
fn basic_jalr() {
    assert_instructions(
        r#"
            JALR x1, 20(x2)
        "#,
        expect![[r#"
            JALR x1, 014(x2)
        "#]],
    );
}

#[test]
fn fence() {
    assert_instructions(
        r#"
            FENCE
        "#,
        expect![[r#"
            FENCE x0, 000(x0)
        "#]],
    );
}

#[test]
fn imem() {
    assert_instructions(
        r#"
            LB x0, 0(x0)
            LBU x0, 0(x0)
            LH x0, 0(x0)
            LHU x0, 0(x0)
            LW x0, 0(x0)
        "#,
        expect![[r#"
            LB x0, 000(x0)
            LBU x0, 000(x0)
            LH x0, 000(x0)
            LHU x0, 000(x0)
            LW x0, 000(x0)
        "#]],
    )
}

#[test]
fn s_32() {
    assert_instructions(
        r#"
            SB x0, 0(x0)
            SH x0, 0(x0)
            SW x0, 0(x0)
            SW x1, 0(x2)
        "#,
        expect![[r#"
            SB x0, 000(x0)
            SH x0, 000(x0)
            SW x0, 000(x0)
            SW x1, 000(x2)
        "#]],
    )
}

#[test]
fn b_32() {
    assert_instructions(
        r#"
            BEQ x0, 10h(x0)
            BNE x0, 10h(x0)
            BLT x0, 10h(x0)
        "#,
        expect![[r#"
            BEQ x0, 0020(x0)
            BNE x0, 0020(x0)
            BLT x0, 0020(x0)
        "#]],
    )
}

#[test]
fn u_32() {
    assert_instructions(
        r#"
            AUIPC x1, 0xdeafd
            LUI x1, 0xdfaed
        "#,
        expect![[r#"
            AUIPC x1, deafd
            LUI x1, dfaed
        "#]],
    )
}

#[test]
fn j_32() {
    assert_instructions(
        r#"
            JAL x1, 0xd
            JAL x1, -0x1
        "#,
        expect![[r#"
            JAL x1, 0000d
            JAL x1, fffff
        "#]],
    )
}

#[test]
fn sys_32() {
    assert_instructions(
        r#"
            ECALL
            EBREAK
        "#,
        expect![[r#"
            ECALL
            EBREAK
        "#]],
    )
}
