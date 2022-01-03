use expect_test::expect;

use crate::tests::{expect_block_with_opts, max_fn, min_fn};

use super::pass_manager::{InplacePass, Pass};
use super::PassManager;

#[test]
fn jal_basic_const_prop() {
    expect_block_with_opts(
        PassManager::with_passes(vec![Pass::ConstProp]),
        "jal x4, 2048",
        expect![[r#"
            %0 = args[0]
            %1 = args[1]
            x(%0)4 = 00000404
            ret 00000000, 00001400"#]],
    );
}

#[test]
fn mem_read_write_all_opts() {
    expect_block_with_opts(
        PassManager::optimized(),
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
            x(%0)2 = %5
            %6 = add %2, 00000032
            %7 = and %6, 00ffffff
            m(%1)%7 = dword %5
            ret 00000001, 00000410"#]],
    );
}

#[test]
fn max() {
    let mut block = max_fn();

    PassManager::optimized().run(&mut block);

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

    PassManager::optimized().run(&mut block);

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
    // todo: C-constprop (add %n, 0) instructions
    expect_block_with_opts(
        PassManager::optimized(),
        r#"
            sb x2, 0(x11)
            addi x2, x12, 0
            auipc x6, 0
            jalr x1, 182(x6)
        "#,
        expect![[r#"
            %0 = args[0]
            %1 = args[1]
            %2 = x(%0)11
            %3 = and %2, 00ffffff
            %4 = x(%0)2
            m(%1)%3 = byte %4
            %5 = x(%0)12
            x(%0)2 = %5
            x(%0)1 = 00000410
            x(%0)6 = 00000408
            ret 00000000, 000004be"#]],
    );
}

#[test]
fn jal_basic_die() {
    expect_block_with_opts(
        PassManager::with_passes(vec![Pass::ConstProp, Pass::DeadInstructionElimination]),
        "jal x4, 2048",
        expect![[r#"
            %0 = args[0]
            x(%0)4 = 00000404
            ret 00000000, 00001400"#]],
    );
}
