use crate::{Block, Id, Instruction, Source, Terminator};

fn mark_live(live_instructions: &mut [bool; 0x1_0000], src: Source) {
    if let Source::Ref(r) = src {
        mark_id_live(live_instructions, r.id);
    }
}

fn mark_id_live(live_instructions: &mut [bool; 0x1_0000], id: Id) {
    live_instructions[id.0 as usize] = true;
}

pub fn run(block: &mut Block) {
    let mut live_ids = [false; 0x1_0000];

    match &block.terminator {
        Terminator::Ret { addr, code } => {
            mark_live(&mut live_ids, *addr);
            mark_live(&mut live_ids, *code);
        }
    }

    let mut live_instruction_count = 0;

    for instruction in block.instructions.iter().rev() {
        match instruction {
            Instruction::Fence => live_instruction_count += 1,

            Instruction::ReadReg { src: _, dest, base } => {
                if live_ids[dest.0 as usize] {
                    mark_live(&mut live_ids, *base);
                    live_instruction_count += 1;
                }
            }

            Instruction::ReadStack { dest, src: _ } | Instruction::Arg { dest, .. } => {
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
                mark_id_live(&mut live_ids, src.id);
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

    for instr in block.instructions.iter().filter(|it| match it.id() {
        Some(id) => live_ids[id.0 as usize],
        None => true,
    }) {
        instructions.push(instr.clone());
    }

    block.instructions = instructions;
}