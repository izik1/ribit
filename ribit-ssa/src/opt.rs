mod const_prop;
mod dead_instruction_elimination;
pub mod pass_manager;
mod register_writeback_shrinking;

pub use const_prop::run as fold_and_prop_consts;
pub use dead_instruction_elimination::run as dead_instruction_elimination;
pub use pass_manager::PassManager;
pub use register_writeback_shrinking::run as register_writeback_shrinking;
#[cfg(test)]
mod test {
    use expect_test::expect;
    use ribit_core::{instruction, opcode, register, Width};

    use crate::lower;
    use crate::test::{max_fn, MEM_SIZE};

    #[test]
    fn jal_basic_const_prop() {
        let ctx = lower::Context::new(0, MEM_SIZE);

        let mut block = lower::terminal(
            ctx,
            instruction::Instruction::J(instruction::J {
                imm: 4096,
                rd: Some(register::RiscV::X4),
                opcode: opcode::J::JAL,
            }),
            4,
        );

        super::fold_and_prop_consts(&mut block);

        expect![[r#"
            %0 = args[0]
            %1 = args[1]
            x(%0)4 = 00000004
            ret 00000000, 00001000"#]].assert_eq(&block.display_instructions().to_string())
    }

    #[test]
    fn mem_read_write_all_opts() {
        let mut ctx = lower::Context::new(0, MEM_SIZE);
        lower::non_terminal(
            &mut ctx,
            instruction::Instruction::IMem(instruction::IMem::new(
                0,
                Some(register::RiscV::X1),
                Some(register::RiscV::X2),
                opcode::IMem::LD(Width::DWord),
            )),
            4,
        );

        lower::non_terminal(
            &mut ctx,
            instruction::Instruction::I(instruction::I::new(
                100,
                Some(register::RiscV::X2),
                Some(register::RiscV::X2),
                opcode::I::ADDI,
            )),
            4,
        );

        lower::non_terminal(
            &mut ctx,
            instruction::Instruction::S(instruction::S::new(
                50,
                Some(register::RiscV::X2),
                Some(register::RiscV::X1),
                Width::DWord,
            )),
            4,
        );

        let mut block = lower::terminal(
            ctx,
            instruction::Instruction::Sys(instruction::Sys::new(opcode::RSys::EBREAK)),
            4,
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
            ret 00000001, 00000010"#]].assert_eq(&block.display_instructions().to_string())
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
            ret 00000000, %9"#]].assert_eq(&block.display_instructions().to_string())
    }

    #[test]
    fn max_opt_bf_bb_1() {
        let mut ctx = lower::Context::new(0x1001c, MEM_SIZE);
        lower::non_terminal(
            &mut ctx,
            instruction::Instruction::S(instruction::S::new(
                0,
                Some(register::RiscV::X2),
                Some(register::RiscV::X11),
                Width::Byte,
            )),
            4,
        );

        lower::non_terminal(
            &mut ctx,
            instruction::Instruction::I(instruction::I::new(
                0,
                Some(register::RiscV::X12),
                Some(register::RiscV::X2),
                opcode::I::ADDI,
            )),
            4,
        );

        lower::non_terminal(
            &mut ctx,
            instruction::Instruction::U(instruction::U::new(
                0,
                Some(register::RiscV::X6),
                opcode::U::AUIPC,
            )),
            4,
        );

        let mut block = lower::terminal(
            ctx,
            instruction::Instruction::IJump(instruction::IJump::new(
                364,
                Some(register::RiscV::X6),
                Some(register::RiscV::X1),
                opcode::IJump::JALR,
            )),
            4,
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
            x(%0)1 = 0001002c
            x(%0)6 = 00010024
            ret 00000000, 00010190"#]].assert_eq(&block.display_instructions().to_string())
    }

    #[test]
    fn jal_basic_die() {
        let ctx = lower::Context::new(0, 0x10000);

        let mut block = lower::terminal(
            ctx,
            instruction::Instruction::J(instruction::J {
                imm: 4096,
                rd: Some(register::RiscV::X4),
                opcode: opcode::J::JAL,
            }),
            4,
        );

        super::fold_and_prop_consts(&mut block);
        super::dead_instruction_elimination(&mut block);

        expect![[r#"
            %0 = args[0]
            x(%0)4 = 00000004
            ret 00000000, 00001000"#]].assert_eq(&block.display_instructions().to_string())
    }
}
