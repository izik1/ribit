use expect_test::expect;

use super::PassManager;
use super::pass_manager::Pass;
use crate::tests::{expect_block_with_opts, max_fn, min_fn};

#[test]
fn register_writeback_multiple_stores() {
    expect_block_with_opts(
        PassManager::with_passes(Vec::from([Pass::RegisterWritebackShrinking])),
        r#"
        add x20, x0, x26
        add x21, x0, x26
        add x26, x0, x26
        ebreak
        "#,
        expect![[r#"
            %0 = args[0]
            %1 = args[1]
            %2 = x(%0)26
            x(%0)21 = %2
            x(%0)20 = %2
            ret 1, 00000410"#]],
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
            ret 1, 00000410"#]],
    );
}

#[test]
fn max() {
    expect_block_with_opts(
        PassManager::optimized(),
        max_fn(),
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
            ret 0, %9"#]],
    );
}

#[test]
fn min() {
    expect_block_with_opts(
        PassManager::optimized(),
        min_fn(),
        //
        expect![[r#"
            %0 = args[0]
            %2 = x(%0)10
            %3 = x(%0)11
            %4 = cmp ult %2, %3
            %5 = zext dword %4
            %6 = sub 00000000, %5
            x(%0)12 = %6
            %7 = xor %2, %3
            %8 = and %6, %7
            %9 = xor %3, %8
            x(%0)10 = %9
            %10 = x(%0)1
            %11 = and %10, fffffffe
            ret 0, %11"#]],
    );
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
            ret 0, 000004be"#]],
    );
}

#[test]
fn max_opt_ori_ori() {
    expect_block_with_opts(
        PassManager::optimized(),
        r#"
            ori x10, x10, 1
            ori x10, x10, 8
            ebreak
        "#,
        expect![[r#"
            %0 = args[0]
            %2 = x(%0)10
            %4 = or %2, 00000009
            x(%0)10 = %4
            ret 1, 0000040c"#]],
    );
}

#[test]
fn many_addis_die() {
    expect_block_with_opts(
        PassManager::with_passes(vec![Pass::DeadInstructionElimination]),
        r#"
            addi x10, x10, 1
            addi x10, x10, 2
            addi x10, x10, 3
            addi x10, x10, 4
            addi x10, x10, -1
            addi x10, x10, 5
            ebreak
        "#,
        expect![[r#"
            %0 = args[0]
            %2 = x(%0)10
            %8 = add %2, 0000000e
            x(%0)10 = %8
            ret 1, 0000041c"#]],
    );
}

#[test]
fn addi_lvn() {
    expect_block_with_opts(
        PassManager::with_passes(Vec::from([Pass::LocalValueNumbering])),
        r#"
            addi x10, x10, 1
            addi x11, x10, 3
            addi x11, x11, -3
            ebreak
        "#,
        expect![[r#"
            %0 = args[0]
            %1 = args[1]
            %2 = x(%0)10
            %3 = add %2, 00000001
            %4 = add %2, 00000004
            %5 = add %2, 00000001
            x(%0)10 = %3
            x(%0)11 = %3
            ret 1, 00000410"#]],
    );
}

#[test]
fn jal_basic_die() {
    expect_block_with_opts(
        PassManager::with_passes(vec![Pass::DeadInstructionElimination]),
        "jal x4, 2048",
        expect![[r#"
            %0 = args[0]
            x(%0)4 = 00000404
            ret 0, 00001400"#]],
    );
}

#[test]
fn load_update_store_known_offset() {
    expect_block_with_opts(
        PassManager::optimized(),
        r"
        auipc x10, 0
        lw x11, -4(x10)
        xori x11, x11, -1
        sw x11, -4(x10)
        ebreak
        ",
        expect![[r#"
            %0 = args[0]
            %1 = args[1]
            %2 = signed dword m(%1)000003fc
            %3 = xor %2, ffffffff
            x(%0)11 = %3
            m(%1)000003fc = dword %3
            x(%0)10 = 00000400
            ret 1, 00000414"#]],
    );
}

#[test]
fn load_update_store_unknown_offset() {
    expect_block_with_opts(
        PassManager::optimized(),
        r"
        lw x11, -4(x10)
        xori x11, x11, -1
        sw x11, -4(x10)
        ebreak
        ",
        expect![[r#"
            %0 = args[0]
            %1 = args[1]
            %2 = x(%0)10
            %3 = add %2, fffffffc
            %4 = and %3, 00ffffff
            %5 = signed dword m(%1)%4
            %6 = xor %5, ffffffff
            x(%0)11 = %6
            m(%1)%4 = dword %6
            ret 1, 00000410"#]],
    );
}

#[test]
fn many_register_loads() {
    expect_block_with_opts(
        PassManager::optimized(),
        r#"
            add x3, x2, x1
            add x6, x5, x4
            add x9, x8, x7
            add x12, x11, x10
            add x15, x14, x13
            add x16, x2, x1
            add x17, x5, x4
            add x18, x8, x7
            add x19, x11, x10
            add x20, x14, x13
            ebreak
        "#,
        expect![[r#"
            %0 = args[0]
            %2 = x(%0)2
            %3 = x(%0)1
            %4 = add %2, %3
            x(%0)16 = %4
            x(%0)3 = %4
            %5 = x(%0)5
            %6 = x(%0)4
            %7 = add %5, %6
            x(%0)17 = %7
            x(%0)6 = %7
            %8 = x(%0)8
            %9 = x(%0)7
            %10 = add %8, %9
            x(%0)18 = %10
            x(%0)9 = %10
            %11 = x(%0)11
            %12 = x(%0)10
            %13 = add %11, %12
            x(%0)19 = %13
            x(%0)12 = %13
            %14 = x(%0)14
            %15 = x(%0)13
            %16 = add %14, %15
            x(%0)20 = %16
            x(%0)15 = %16
            ret 1, 0000042c"#]],
    );
}
