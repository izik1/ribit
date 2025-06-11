use expect_test::expect;

use super::PassManager;
use super::pass_manager::Pass;
use crate::tests::{expect_block_with_opts, max_fn, min_fn};

#[test]
fn jal_basic_const_prop() {
    expect_block_with_opts(
        PassManager::with_passes(vec![Pass::ConstProp]),
        "jal x4, 2048",
        expect![[r#"
            %2 = args[0]
            %3 = args[1]
            x(%2)4 = 00000404
            ret 0, 00001400"#]],
    );
}

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
            %2 = args[0]
            %3 = args[1]
            %4 = x(%2)26
            x(%2)21 = %4
            x(%2)20 = %4
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
            %2 = args[0]
            %3 = args[1]
            %4 = x(%2)1
            %5 = and %4, 00ffffff
            %6 = signed dword m(%3)%5
            %7 = add %6, 00000064
            x(%2)2 = %7
            %8 = add %4, 00000032
            %9 = and %8, 00ffffff
            m(%3)%9 = dword %7
            ret 1, 00000410"#]],
    );
}

#[test]
fn max() {
    expect_block_with_opts(
        PassManager::optimized(),
        max_fn(),
        expect![[r#"
            %2 = args[0]
            %4 = x(%2)10
            %5 = x(%2)11
            %6 = add %4, %5
            %7 = srl %6, 0000001f
            x(%2)12 = %7
            %8 = and %6, %7
            x(%2)11 = %8
            %9 = add %4, %8
            x(%2)10 = %9
            %10 = x(%2)1
            %11 = and %10, fffffffe
            ret 0, %11"#]],
    );
}

#[test]
fn min() {
    expect_block_with_opts(
        PassManager::optimized(),
        min_fn(),
        //
        expect![[r#"
            %2 = args[0]
            %4 = x(%2)10
            %5 = x(%2)11
            %6 = cmp ult %4, %5
            %7 = zext dword %6
            %8 = sub 00000000, %7
            x(%2)12 = %8
            %9 = xor %4, %5
            %10 = and %9, %8
            %11 = xor %10, %5
            x(%2)10 = %11
            %12 = x(%2)1
            %13 = and %12, fffffffe
            ret 0, %13"#]],
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
            %2 = args[0]
            %3 = args[1]
            %4 = x(%2)11
            %5 = and %4, 00ffffff
            %6 = x(%2)2
            m(%3)%5 = byte %6
            %7 = x(%2)12
            x(%2)2 = %7
            x(%2)1 = 00000410
            x(%2)6 = 00000408
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
            %2 = args[0]
            %4 = x(%2)10
            %6 = or %4, 00000009
            x(%2)10 = %6
            ret 1, 0000040c"#]],
    );
}

#[test]
fn max_opt_many_addis() {
    expect_block_with_opts(
        PassManager::optimized(),
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
            %2 = args[0]
            %4 = x(%2)10
            %10 = add %4, 0000000e
            x(%2)10 = %10
            ret 1, 0000041c"#]],
    );
}

#[test]
fn jal_basic_die() {
    expect_block_with_opts(
        PassManager::with_passes(vec![Pass::ConstProp, Pass::DeadInstructionElimination]),
        "jal x4, 2048",
        expect![[r#"
            %2 = args[0]
            x(%2)4 = 00000404
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
            %2 = args[0]
            %3 = args[1]
            %4 = signed dword m(%3)000003fc
            %5 = xor %4, ffffffff
            x(%2)11 = %5
            m(%3)000003fc = dword %5
            x(%2)10 = 00000400
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
        // note the redundant address calculation between the load/store.
        // ideally this is solved via some kind of value numbering, which is easier said than done.
        // hazards LVN needs to worry about:
        // `fence` (IORW<->IORW barriers), "todo: more fine grained fences", but also "todo: actually care about fences"
        // IDs destination IDs (two instructions won't `==` if their ID is different... but that pass is trying to explicitly figure when it's just the ID that's different)
        // multiple replacements (see: this very block, `%10 == %6`, but just doing a single sweep/replace isn't enough to figure that out without clever tricks)
        // memory, register, and stack loads may look equal because they alias, but if there's an intervening write (in all cases), or fence (memory, csr), they most likely _aren't_.
        // stores don't have IDs so LVN wouldn't apply to them, but, if they did, there's one more:
        // stores with a fence in the way may not be replaced.
        expect![[r#"
            %2 = args[0]
            %3 = args[1]
            %4 = x(%2)10
            %5 = add %4, fffffffc
            %6 = and %5, 00ffffff
            %7 = signed dword m(%3)%6
            %8 = xor %7, ffffffff
            x(%2)11 = %8
            %9 = add %4, fffffffc
            %10 = and %9, 00ffffff
            m(%3)%10 = dword %8
            ret 1, 00000410"#]],
    );
}
