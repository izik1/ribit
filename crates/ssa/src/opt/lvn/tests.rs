use expect_test::expect;
use ribit_core::register;

use crate::opt::PassManager;
use crate::opt::pass_manager::Pass;
use crate::tests::expect_block_with_opts;

fn passes() -> PassManager {
    PassManager::with_passes(Vec::from([
        Pass::LocalValueNumbering,
        Pass::DeadInstructionElimination,
    ]))
}

// positive test: this should lead to the adds being merged.
#[test]
fn commutative_equivalent() {
    expect_block_with_opts(
        passes(),
        r"
        add x10, x11, x12
        add x11, x12, x11
        ebreak
        ",
        expect![[r#"
            %0 = args[0]
            %2 = x(%0)11
            %3 = x(%0)12
            %4 = add %2, %3
            x(%0)10 = %4
            x(%0)11 = %4
            ret 1, 0000040c"#]],
    );
}

#[test]
fn commutative_non_equivalent() {
    expect_block_with_opts(
        passes(),
        {
            let mut context = crate::lower::Context::new(1024, 0x1_0000);

            let reg_a = context.load_register(Some(register::RiscV::X10));
            let reg_b = context.load_register(Some(register::RiscV::X11));
            let reg_c = context.load_register(Some(register::RiscV::X12));

            let add_a = context.add(reg_a, reg_b);
            let add_b = context.add(reg_b, reg_c);

            context.write_register(register::RiscV::X10, add_a);
            context.write_register(register::RiscV::X11, add_b);

            context.ret()
        },
        expect![[r#"
            %0 = args[0]
            %2 = x(%0)10
            %3 = x(%0)11
            %4 = x(%0)12
            %5 = add %2, %3
            %6 = add %3, %4
            x(%0)10 = %5
            x(%0)11 = %6
            ret 0, 00000400"#]],
    );
}

// this is currently a negative test, but the only thing stopping this from working is the pass not being smart enough.
#[test]
fn add_sll_equivalent() {
    expect_block_with_opts(
        passes(),
        r#"
        add x10, x11, x11
        slli x11, x11, 1
        ebreak
        "#,
        expect![[r#"
            %0 = args[0]
            %2 = x(%0)11
            %3 = add %2, %2
            %4 = sll %2, 00000001
            x(%0)10 = %3
            x(%0)11 = %4
            ret 1, 0000040c"#]],
    );
}

#[test]
fn cmp_equivalent_eq() {
    expect_block_with_opts(
        passes(),
        {
            let mut context = crate::lower::Context::new(1024, 0x1_0000);

            let reg_a = context.load_register(Some(register::RiscV::X10));
            let reg_b = context.load_register(Some(register::RiscV::X11));

            let cmp_a = context.cmp(reg_a, reg_b, crate::CmpKind::Eq);
            let cmp_b = context.cmp(reg_b, reg_a, crate::CmpKind::Eq);

            let val_a = context.load_register(Some(register::RiscV::X12));
            let val_b = context.load_register(Some(register::RiscV::X13));

            let out_a = context.select(cmp_a, val_a, val_b);
            let out_b = context.select(cmp_b, val_a, val_b);

            context.write_register(register::RiscV::X10, out_a);
            context.write_register(register::RiscV::X11, out_b);

            context.ret()
        },
        expect![[r#"
            %0 = args[0]
            %2 = x(%0)10
            %3 = x(%0)11
            %4 = cmp eq %2, %3
            %6 = x(%0)12
            %7 = x(%0)13
            %8 = select %4, %6, %7
            x(%0)10 = %8
            x(%0)11 = %8
            ret 0, 00000400"#]],
    );
}

#[test]
fn cmp_equivalent_inequality_same() {
    expect_block_with_opts(
        passes(),
        {
            let mut context = crate::lower::Context::new(1024, 0x1_0000);

            let reg_a = context.load_register(Some(register::RiscV::X10));
            let reg_b = context.load_register(Some(register::RiscV::X11));

            let cmp_a =
                context.cmp(reg_a, reg_b, crate::CmpKind::Inequality(crate::Inequality::Ult));
            let cmp_b =
                context.cmp(reg_a, reg_b, crate::CmpKind::Inequality(crate::Inequality::Ult));

            let val_a = context.load_register(Some(register::RiscV::X12));
            let val_b = context.load_register(Some(register::RiscV::X13));

            let out_a = context.select(cmp_a, val_a, val_b);
            let out_b = context.select(cmp_b, val_a, val_b);

            context.write_register(register::RiscV::X10, out_a);
            context.write_register(register::RiscV::X11, out_b);

            context.ret()
        },
        expect![[r#"
            %0 = args[0]
            %2 = x(%0)10
            %3 = x(%0)11
            %4 = cmp ult %2, %3
            %6 = x(%0)12
            %7 = x(%0)13
            %8 = select %4, %6, %7
            x(%0)10 = %8
            x(%0)11 = %8
            ret 0, 00000400"#]],
    );
}

#[test]
fn cmp_equivalent_inequality_mirrored() {
    expect_block_with_opts(
        passes(),
        {
            let mut context = crate::lower::Context::new(1024, 0x1_0000);

            let reg_a = context.load_register(Some(register::RiscV::X10));
            let reg_b = context.load_register(Some(register::RiscV::X11));

            let cmp_a =
                context.cmp(reg_a, reg_b, crate::CmpKind::Inequality(crate::Inequality::Ult));
            let cmp_b =
                context.cmp(reg_b, reg_a, crate::CmpKind::Inequality(crate::Inequality::Ugt));

            let out_a = context.select(cmp_a, reg_a, reg_b);
            let out_b = context.select(cmp_b, reg_a, reg_b);

            context.write_register(register::RiscV::X10, out_a);
            context.write_register(register::RiscV::X11, out_b);

            context.ret()
        },
        expect![[r#"
            %0 = args[0]
            %2 = x(%0)10
            %3 = x(%0)11
            %4 = cmp ult %2, %3
            %6 = select %4, %2, %3
            x(%0)10 = %6
            x(%0)11 = %6
            ret 0, 00000400"#]],
    );
}

// this is currently a negative test, but the only thing stopping this from working is the pass not being smart enough.
#[test]
fn cmp_non_equivalent_eq() {
    expect_block_with_opts(
        passes(),
        {
            let mut context = crate::lower::Context::new(1024, 0x1_0000);

            let reg_a = context.load_register(Some(register::RiscV::X10));
            let reg_b = context.load_register(Some(register::RiscV::X11));

            let cmp_a = context.cmp(reg_a, reg_b, crate::CmpKind::Eq);

            let reg_b = context.load_register(Some(register::RiscV::X12));

            let cmp_b = context.cmp(reg_b, reg_a, crate::CmpKind::Eq);

            let val_a = context.load_register(Some(register::RiscV::X12));
            let val_b = context.load_register(Some(register::RiscV::X13));

            let out_a = context.select(cmp_a, val_a, val_b);
            let out_b = context.select(cmp_b, val_a, val_b);

            context.write_register(register::RiscV::X10, out_a);
            context.write_register(register::RiscV::X11, out_b);

            context.ret()
        },
        expect![[r#"
            %0 = args[0]
            %2 = x(%0)10
            %3 = x(%0)11
            %4 = cmp eq %2, %3
            %5 = x(%0)12
            %6 = cmp eq %2, %5
            %7 = x(%0)13
            %8 = select %4, %5, %7
            %9 = select %6, %5, %7
            x(%0)10 = %8
            x(%0)11 = %9
            ret 0, 00000400"#]],
    );
}

#[test]
fn cmp_non_equivalent_inequality_same() {
    expect_block_with_opts(
        passes(),
        {
            let mut context = crate::lower::Context::new(1024, 0x1_0000);

            let reg_a = context.load_register(Some(register::RiscV::X10));
            let reg_b = context.load_register(Some(register::RiscV::X11));

            let cmp_a =
                context.cmp(reg_a, reg_b, crate::CmpKind::Inequality(crate::Inequality::Ult));
            let cmp_b =
                context.cmp(reg_b, reg_a, crate::CmpKind::Inequality(crate::Inequality::Ult));

            let out_a = context.select(cmp_a, reg_a, reg_b);
            let out_b = context.select(cmp_b, reg_a, reg_b);

            context.write_register(register::RiscV::X10, out_a);
            context.write_register(register::RiscV::X11, out_b);

            context.ret()
        },
        expect![[r#"
            %0 = args[0]
            %2 = x(%0)10
            %3 = x(%0)11
            %4 = cmp ult %2, %3
            %5 = cmp ugt %2, %3
            %6 = select %4, %2, %3
            %7 = select %5, %2, %3
            x(%0)10 = %6
            x(%0)11 = %7
            ret 0, 00000400"#]],
    );
}

#[test]
fn cmp_non_equivalent_inequality_mirrored() {
    expect_block_with_opts(
        passes(),
        {
            let mut context = crate::lower::Context::new(1024, 0x1_0000);

            let reg_a = context.load_register(Some(register::RiscV::X10));
            let reg_b = context.load_register(Some(register::RiscV::X11));

            let cmp_a =
                context.cmp(reg_a, reg_b, crate::CmpKind::Inequality(crate::Inequality::Ult));
            let cmp_b =
                context.cmp(reg_a, reg_b, crate::CmpKind::Inequality(crate::Inequality::Ugt));

            let out_a = context.select(cmp_a, reg_a, reg_b);
            let out_b = context.select(cmp_b, reg_a, reg_b);

            context.write_register(register::RiscV::X10, out_a);
            context.write_register(register::RiscV::X11, out_b);

            context.ret()
        },
        expect![[r#"
            %0 = args[0]
            %2 = x(%0)10
            %3 = x(%0)11
            %4 = cmp ult %2, %3
            %5 = cmp ugt %2, %3
            %6 = select %4, %2, %3
            %7 = select %5, %2, %3
            x(%0)10 = %6
            x(%0)11 = %7
            ret 0, 00000400"#]],
    );
}
