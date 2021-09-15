use std::collections::HashMap;

use crate::{eval, Id, Instruction, Source};

pub mod pass_manager;

pub use pass_manager::PassManager;

fn const_id_lookup(consts: &HashMap<Id, u32>, src: Source) -> Option<u32> {
    src.id().and_then(|it| consts.get(&it).copied())
}

fn const_prop(consts: &HashMap<Id, u32>, src: &mut Source) -> Option<u32> {
    if let Some(v) = const_id_lookup(consts, *src) {
        *src = Source::Val(v);
        Some(v)
    } else if let Source::Val(v) = src {
        Some(*v)
    } else {
        None
    }
}

// todo: `pub fn complex_const_prop(graph: &mut [Instruction])`

pub fn fold_and_prop_consts(graph: &mut [Instruction]) {
    let mut consts = HashMap::new();
    for instruction in graph.iter_mut() {
        let (dest, val) = match instruction {
            Instruction::LoadConst { dest, src } => (*dest, *src),

            Instruction::Fence
            | Instruction::Arg { .. }
            | Instruction::ReadStack { .. }
            | Instruction::WriteStack { .. } => continue,
            Instruction::ReadReg { base, .. } => {
                const_prop(&consts, base);
                continue;
            }

            Instruction::WriteReg { src, base, .. }
            | Instruction::ReadMem { src, base, .. }
            | Instruction::WriteMem { src, base, .. } => {
                const_prop(&consts, src);
                const_prop(&consts, base);

                continue;
            }

            Instruction::BinOp { dest, src1, src2, op } => {
                let src1 = const_prop(&consts, src1);
                let src2 = const_prop(&consts, src2);

                let res = match (src1, src2) {
                    (Some(src1), Some(src2)) => eval::binop(src1, src2, *op),
                    _ => continue,
                };

                (*dest, res)
            }

            Instruction::Cmp { dest, src1, src2, kind } => {
                let src1 = const_prop(&consts, src1);
                let src2 = const_prop(&consts, src2);

                // note: this explicitly doesn't simplify things like
                // `cmp eq %0, %0`, that's for a different pass
                // todo: write pass for the above.

                let res = match (src1, src2) {
                    (Some(src1), Some(src2)) => eval::cmp(src1, src2, *kind),
                    _ => continue,
                };

                (*dest, res)
            }

            Instruction::Select { dest, cond, if_true, if_false } => {
                let cond = const_prop(&consts, cond);
                let if_true = const_prop(&consts, if_true);
                let if_false = const_prop(&consts, if_false);

                if let Some(res) = eval::try_select(cond, if_true, if_false) {
                    (*dest, res)
                } else {
                    continue;
                }
            }

            Instruction::Ret { addr, code } => {
                const_prop(&consts, addr);
                const_prop(&consts, code);

                continue;
            }
        };

        *instruction = Instruction::LoadConst { dest, src: val };

        consts.insert(dest, val);
    }
}

fn mark_live(live_instructions: &mut [bool; 0x1_0000], src: Source) {
    if let Source::Id(id) = src {
        live_instructions[id.0 as usize] = true;
    }
}

#[must_use]
pub fn dead_instruction_elimination(graph: &[Instruction]) -> Vec<Instruction> {
    let mut live_ids = [false; 0x1_0000];
    let mut live_instruction_count = 0;

    for instruction in graph.iter().rev() {
        match instruction {
            Instruction::Ret { addr, code } => {
                mark_live(&mut live_ids, *addr);
                mark_live(&mut live_ids, *code);

                live_instruction_count += 1;
            }

            Instruction::Fence => live_instruction_count += 1,

            Instruction::ReadReg { src: _, dest, base } => {
                if live_ids[dest.0 as usize] {
                    mark_live(&mut live_ids, *base);
                    live_instruction_count += 1;
                }
            }

            Instruction::ReadStack { dest, src: _ }
            | Instruction::LoadConst { dest, .. }
            | Instruction::Arg { dest, .. } => {
                if live_ids[dest.0 as usize] {
                    live_instruction_count += 1;
                }
            }

            Instruction::ReadMem { src, base, dest, sign_extend: _, width: _ } => {
                // todo: revist this. Current EE *does* allow removing dead reads. Future ones may not.
                // hack: I'm not sure when it's safe to remove unused memory reads,
                // so assume we never can.

                if live_ids[dest.0 as usize] {
                    live_instruction_count += 1;
                    mark_live(&mut live_ids, *src);
                    mark_live(&mut live_ids, *base);
                }
            }

            Instruction::WriteReg { src, dest: _, base } => {
                live_instruction_count += 1;
                mark_live(&mut live_ids, *src);
                mark_live(&mut live_ids, *base);
            }

            Instruction::WriteStack { dest: _, src } => {
                live_instruction_count += 1;
                mark_live(&mut live_ids, Source::Id(*src));
            }

            Instruction::WriteMem { addr, src, base, width: _ } => {
                live_instruction_count += 1;
                mark_live(&mut live_ids, *addr);
                mark_live(&mut live_ids, *src);
                mark_live(&mut live_ids, *base);
            }

            Instruction::BinOp { dest, src1, src2, .. }
            | Instruction::Cmp { dest, src1, src2, .. } => {
                if live_ids[dest.0 as usize] {
                    live_instruction_count += 1;
                    mark_live(&mut live_ids, *src1);
                    mark_live(&mut live_ids, *src2);
                }
            }

            Instruction::Select { dest, cond, if_true, if_false } => {
                if live_ids[dest.0 as usize] {
                    live_instruction_count += 1;
                    mark_live(&mut live_ids, *cond);
                    mark_live(&mut live_ids, *if_true);
                    mark_live(&mut live_ids, *if_false);
                }
            }
        }
    }

    let mut instructions = Vec::with_capacity(live_instruction_count);

    for instr in graph.iter().filter(|it| match it.id() {
        Some(id) => live_ids[id.0 as usize],
        None => true,
    }) {
        instructions.push(instr.clone());
    }

    instructions
}

/// Moves register writes to be as close to their computations as possible.
pub fn register_writeback_shrinking(graph: &[Instruction]) -> Vec<Instruction> {
    let mut stores = vec![];
    let mut instrs = Vec::with_capacity(graph.len());

    for instr in graph {
        match instr {
            Instruction::WriteReg { dest, src: Source::Id(id), base } => {
                stores.push((*dest, *id, *base));
            }

            _ => instrs.push(instr.clone()),
        }
    }

    for idx in (0..instrs.len()).rev() {
        if let Some(store_idx) =
            instrs[idx].id().and_then(|id| stores.iter().copied().position(|(_, src, _)| src == id))
        {
            let store = stores.remove(store_idx);
            // insert _after_ the instruction we just looked at
            instrs.insert(
                idx + 1,
                Instruction::WriteReg { dest: store.0, src: Source::Id(store.1), base: store.2 },
            );
        }
    }

    instrs
}

#[cfg(test)]
mod test {
    use insta::assert_display_snapshot;
    use ribit_core::{instruction, opcode, register, DisplayDeferSlice, Width};

    use crate::lower;
    use crate::test::{max_fn, MEM_SIZE};

    #[test]
    fn jal_basic_const_prop() {
        let ctx = lower::Context::new(0, MEM_SIZE);

        let (mut instrs, _) = lower::terminal(
            ctx,
            instruction::Instruction::J(instruction::J {
                imm: 4096,
                rd: Some(register::RiscV::X4),
                opcode: opcode::J::JAL,
            }),
            4,
        );

        super::fold_and_prop_consts(&mut instrs);

        assert_display_snapshot!(DisplayDeferSlice(&instrs));
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

        let (mut instrs, _) = lower::terminal(
            ctx,
            instruction::Instruction::Sys(instruction::Sys::new(opcode::RSys::EBREAK)),
            4,
        );

        super::fold_and_prop_consts(&mut instrs);
        let instrs = super::dead_instruction_elimination(&instrs);
        let instrs = super::register_writeback_shrinking(&instrs);

        assert_display_snapshot!(DisplayDeferSlice(&instrs));
    }

    #[test]
    fn max() {
        let (mut instrs, _) = max_fn();

        super::fold_and_prop_consts(&mut instrs);
        let instrs = super::dead_instruction_elimination(&instrs);
        let instrs = super::register_writeback_shrinking(&instrs);

        assert_display_snapshot!(DisplayDeferSlice(&instrs));
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

        let (mut instrs, _) = lower::terminal(
            ctx,
            instruction::Instruction::IJump(instruction::IJump::new(
                364,
                Some(register::RiscV::X6),
                Some(register::RiscV::X1),
                opcode::IJump::JALR,
            )),
            4,
        );

        super::fold_and_prop_consts(&mut instrs);
        let instrs = super::dead_instruction_elimination(&instrs);
        let instrs = super::register_writeback_shrinking(&instrs);

        // todo: C-constprop (add %n, 0) instructions

        assert_display_snapshot!(DisplayDeferSlice(&instrs));
    }

    #[test]
    fn jal_basic_die() {
        let ctx = lower::Context::new(0, 0x10000);

        let (mut instrs, _) = lower::terminal(
            ctx,
            instruction::Instruction::J(instruction::J {
                imm: 4096,
                rd: Some(register::RiscV::X4),
                opcode: opcode::J::JAL,
            }),
            4,
        );

        super::fold_and_prop_consts(&mut instrs);
        let instrs = super::dead_instruction_elimination(&instrs);

        assert_display_snapshot!(DisplayDeferSlice(&instrs));
    }
}
