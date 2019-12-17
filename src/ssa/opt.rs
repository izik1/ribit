use crate::ssa::{BinOp, CmpKind, Id, Instruction, Source};
use std::collections::{HashMap, HashSet};

fn const_id_lookup(consts: &HashMap<Id, u32>, src: &Source) -> Option<u32> {
    match src {
        Source::Id(id) => Some(id),
        Source::Val(_) => None,
    }
    .and_then(|it| consts.get(it).copied())
}

fn const_prop(consts: &HashMap<Id, u32>, src: &mut Source) -> Option<u32> {
    if let Some(v) = const_id_lookup(consts, src) {
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
            Instruction::ReadReg { .. } => continue,

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
                    (Some(src1), Some(src2)) => match op {
                        BinOp::And => src1 & src2,
                        BinOp::Add => src1.wrapping_add(src2),
                        BinOp::Or => src1 | src2,
                        BinOp::Sll => src1 << (src2 & 0x1f),
                        BinOp::Srl => src1 >> (src2 & 0x1f),
                        BinOp::Sra => ((src1 as i32) << (src2 & 0x1f)) as u32,
                        BinOp::Sub => src1.wrapping_sub(src2),
                        BinOp::Xor => src1 ^ src2,
                    },
                    _ => continue,
                };

                dbg!("test");
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
                    (Some(src1), Some(src2)) => match kind {
                        CmpKind::Eq => src1 == src2,
                        CmpKind::Ne => src1 != src2,
                        CmpKind::Sge => (src1 as i32) >= (src2 as i32),
                        CmpKind::Sl => (src1 as i32) < (src2 as i32),
                        CmpKind::Uge => src1 >= src2,
                        CmpKind::Ul => src1 < src2,
                    },
                    _ => continue,
                };

                (*dest, res as u32)
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

                match (cond, if_true, if_false) {
                    (Some(cond), Some(if_true), _) if cond > 0 => (*dest, if_true),
                    (Some(cond), _, Some(if_false)) if cond == 0 => (*dest, if_false),
                    _ => continue,
                }
            }

            Instruction::Ret { addr, code } => {
                const_prop(&consts, addr);
                const_prop(&consts, code);

                continue;
            }

            Instruction::Fence => continue,
        };

        *instruction = Instruction::LoadConst { dest, src: val };

        consts.insert(dest, val);
    }
}

#[cfg(test)]
mod test {
    use crate::ssa::{cmp_instrs, lower};
    use crate::{instruction, opcode, register};

    #[test]
    fn jal_basic() {
        let ctx = lower::Context::new(0);

        let mut instrs = lower::lower_terminal(
            ctx,
            instruction::Instruction::J(instruction::J {
                imm: 4096,
                rd: Some(register::RiscV::X4),
                opcode: opcode::J::JAL,
            }),
            4,
        );

        super::fold_and_prop_consts(&mut instrs);

        cmp_instrs(
            &["%0 = 0", "%1 = 4", "x4 = 4", "%2 = 4096", "ret 0, 4096"],
            &instrs,
        );
    }
}
