use expect_test::expect;

use crate::test::{assemble_block, max_fn, min_fn};

#[test]
fn jal_basic_const_prop() {
    let mut block = assemble_block(
        r#"
            JAL x4, 2048
        "#,
    );

    super::fold_and_prop_consts(&mut block);

    expect![[r#"
            %0 = args[0]
            %1 = args[1]
            x(%0)4 = 00000404
            ret 00000000, 00001400"#]]
    .assert_eq(&block.display_instructions().to_string())
}

#[test]
fn mem_read_write_all_opts() {
    let mut block = assemble_block(
        r#"
            LW x2, 0(x1)
            ADDI x2, x2, 100
            SW x2, 50(x1)
            EBREAK
        "#,
    );

    super::fold_and_prop_consts(&mut block);
    super::dead_instruction_elimination(&mut block);
    super::register_writeback_shrinking(&mut block);

    expect![[r#"
            %0 = args[0]
            %1 = args[1]
            %2 = x(%0)1
            %3 = add %2, 00000000
            %4 = and %3, 00ffffff
            %5 = signed dword m(%1)%4
            %6 = add %5, 00000064
            x(%0)2 = %6
            %7 = add %6, 00000032
            %8 = and %7, 00ffffff
            m(%1)%8 = dword %2
            ret 00000001, 00000410"#]]
    .assert_eq(&block.display_instructions().to_string())
}

#[test]
fn max() {
    let mut block = max_fn();

    super::fold_and_prop_consts(&mut block);
    super::dead_instruction_elimination(&mut block);
    super::register_writeback_shrinking(&mut block);

    expect![[r#"
            %0 = args[0]
            %2 = x(%0)10
            %3 = x(%0)11
            %4 = add %2, %3
            %5 = srl %4, 0000001f
            x(%0)12 = %5
            %6 = and %4, %5
            x(%0)11 = %6
            %7 = add %2, %6
            x(%0)10 = %7
            %8 = x(%0)1
            %9 = and %8, fffffffe
            ret 00000000, %9"#]]
    .assert_eq(&block.display_instructions().to_string())
}

#[test]
fn min() {
    let mut block = min_fn();

    super::fold_and_prop_consts(&mut block);
    super::dead_instruction_elimination(&mut block);
    super::register_writeback_shrinking(&mut block);

    expect![[r#"
            %0 = args[0]
            %2 = x(%0)10
            %3 = x(%0)11
            %4 = cmp UL %2, %3
            %5 = zext dword %4
            %6 = sub 00000000, %5
            x(%0)12 = %6
            %7 = xor %2, %3
            %8 = and %7, %6
            %9 = xor %8, %3
            x(%0)10 = %9
            %10 = x(%0)1
            %11 = and %10, fffffffe
            ret 00000000, %11"#]]
    .assert_eq(&block.display_instructions().to_string())
}

#[test]
fn max_opt_bf_bb_1() {
    // todo: psudeo instructions (`mv`, `call`)
    let mut block = assemble_block(
        r#"
            SB x2, 0(x11)
            ADDI x2, x12, 0
            AUIPC x6, 0
            JALR x1, 182(x6)
        "#,
    );

    super::fold_and_prop_consts(&mut block);
    super::dead_instruction_elimination(&mut block);
    super::register_writeback_shrinking(&mut block);

    // todo: C-constprop (add %n, 0) instructions

    expect![[r#"
            %0 = args[0]
            %1 = args[1]
            %2 = x(%0)2
            %3 = add %2, 00000000
            %4 = and %3, 00ffffff
            %5 = x(%0)11
            m(%1)%4 = byte %5
            %6 = x(%0)12
            %7 = add %6, 00000000
            x(%0)2 = %7
            x(%0)1 = 00000410
            x(%0)6 = 00000408
            ret 00000000, 000004be"#]]
    .assert_eq(&block.display_instructions().to_string())
}

#[test]
fn jal_basic_die() {
    let mut block = assemble_block(
        r#"
            JAL x4, 2048
        "#,
    );

    super::fold_and_prop_consts(&mut block);
    super::dead_instruction_elimination(&mut block);

    expect![[r#"
            %0 = args[0]
            x(%0)4 = 00000404
            ret 00000000, 00001400"#]]
    .assert_eq(&block.display_instructions().to_string())
}
