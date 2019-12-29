use crate::ssa::{eval, Id, Instruction, Source};
use std::collections::HashMap;

fn const_id_lookup(consts: &HashMap<Id, u32>, src: Source) -> Option<u32> {
    match src {
        Source::Id(id) => Some(id),
        Source::Val(_) => None,
    }
    .and_then(|it| consts.get(&it).copied())
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

pub fn fold_and_prop_consts(graph: &mut [Instruction]) {
    let mut consts = HashMap::new();
    for instruction in graph.iter_mut() {
        let (dest, val) = match instruction {
            Instruction::LoadConst { dest, src } => (*dest, *src),

            Instruction::Fence | Instruction::ReadReg { .. } => continue,

            Instruction::WriteReg { src, .. }
            | Instruction::ReadMem { src, .. }
            | Instruction::WriteMem { src, .. } => {
                const_prop(&consts, src);

                continue;
            }

            Instruction::BinOp {
                dest,
                src1,
                src2,
                op,
            } => {
                let src1 = const_prop(&consts, src1);
                let src2 = const_prop(&consts, src2);

                let res = match (src1, src2) {
                    (Some(src1), Some(src2)) => eval::binop(src1, src2, *op),
                    _ => continue,
                };

                (*dest, res)
            }

            Instruction::Cmp {
                dest,
                src1,
                src2,
                kind,
            } => {
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

            Instruction::Select {
                dest,
                cond,
                if_true,
                if_false,
            } => {
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
            Instruction::ReadReg { dest, .. } | Instruction::LoadConst { dest, .. } => {
                if live_ids[dest.0 as usize] {
                    live_instruction_count += 1;
                }
            }

            Instruction::ReadMem { src, .. } => {
                // hack: I'm not sure when it's safe to remove unused memory reads,
                // so assume we never can.
                live_instruction_count += 1;
                mark_live(&mut live_ids, *src);
            }

            Instruction::WriteReg { src, .. } => {
                live_instruction_count += 1;
                mark_live(&mut live_ids, *src);
            }

            Instruction::WriteMem { addr, src, .. } => {
                live_instruction_count += 1;
                mark_live(&mut live_ids, *addr);
                mark_live(&mut live_ids, *src);
            }

            Instruction::BinOp {
                dest, src1, src2, ..
            }
            | Instruction::Cmp {
                dest, src1, src2, ..
            } => {
                if live_ids[dest.0 as usize] {
                    live_instruction_count += 1;
                    mark_live(&mut live_ids, *src1);
                    mark_live(&mut live_ids, *src2);
                }
            }

            Instruction::Select {
                dest,
                cond,
                if_true,
                if_false,
            } => {
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
            Instruction::WriteReg {
                dest,
                src: Source::Id(id),
            } => {
                stores.push((*dest, *id));
            }

            _ => instrs.push(instr.clone()),
        }
    }

    for idx in (0..instrs.len()).rev() {
        if let Some(store_idx) = instrs[idx]
            .id()
            .and_then(|id| stores.iter().copied().position(|(_, src)| src == id))
        {
            let store = stores.remove(store_idx);
            // insert _after_ the instruction we just looked at
            instrs.insert(
                idx + 1,
                Instruction::WriteReg {
                    dest: store.0,
                    src: Source::Id(store.1),
                },
            );
        }
    }

    instrs
}

#[cfg(test)]
mod test {
    use crate::ssa::lower;
    use crate::DisplayDeferSlice;
    use crate::{instruction, opcode, register};

    use insta::assert_display_snapshot;

    #[test]
    fn jal_basic_const_prop() {
        let ctx = lower::Context::new(0);

        let mut instrs = lower::terminal(
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
    fn max() {
        let mut instrs = crate::ssa::max_fn();

        super::fold_and_prop_consts(&mut instrs);
        let instrs = super::dead_instruction_elimination(&instrs);
        let instrs = super::register_writeback_shrinking(&instrs);

        assert_display_snapshot!(DisplayDeferSlice(&instrs));
    }

    #[test]
    fn jal_basic_die() {
        let ctx = lower::Context::new(0);

        let mut instrs = lower::terminal(
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
