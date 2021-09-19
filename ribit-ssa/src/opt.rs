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
    use insta::assert_display_snapshot;
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

        assert_display_snapshot!(block.display_instructions());
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

        assert_display_snapshot!(block.display_instructions());
    }

    #[test]
    fn max() {
        let mut block = max_fn();

        super::fold_and_prop_consts(&mut block);
        super::dead_instruction_elimination(&mut block);
        super::register_writeback_shrinking(&mut block);

        assert_display_snapshot!(block.display_instructions());
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

        assert_display_snapshot!(block.display_instructions());
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

        assert_display_snapshot!(block.display_instructions());
    }
}
