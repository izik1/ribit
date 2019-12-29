use crate::ssa::{Id, Instruction, Source};
use std::collections::HashMap;
use std::{fmt, mem};

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Lifetime {
    pub start: usize,
    pub end: usize,
}

impl Lifetime {
    pub fn is_alive_after(self, idx: usize) -> bool {
        idx >= self.start && idx <= self.end
    }
}

pub type Lifetimes = HashMap<Id, Lifetime>;

fn update_lifetime(lifetimes: &mut Lifetimes, src: Source, idx: usize) {
    if let Some(id) = src.id() {
        mem::replace(
            &mut lifetimes
                .entry(id)
                .or_insert(Lifetime {
                    start: idx,
                    end: idx,
                })
                .end,
            idx,
        );
    }
}

pub fn lifetimes(instrs: &[Instruction]) -> Lifetimes {
    let mut lifetimes = HashMap::with_capacity(instrs.len());

    for (idx, instr) in instrs.iter().enumerate() {
        match instr {
            Instruction::Fence => {}

            Instruction::BinOp {
                dest, src1, src2, ..
            }
            | Instruction::Cmp {
                dest, src1, src2, ..
            } => {
                update_lifetime(&mut lifetimes, Source::Id(*dest), idx);
                update_lifetime(&mut lifetimes, *src1, idx);
                update_lifetime(&mut lifetimes, *src2, idx);
            }

            Instruction::ReadReg { dest, base, .. } => {
                update_lifetime(&mut lifetimes, *base, idx);
                update_lifetime(&mut lifetimes, Source::Id(*dest), idx);
            }

            Instruction::Arg { dest, .. } | Instruction::LoadConst { dest, .. } => {
                update_lifetime(&mut lifetimes, Source::Id(*dest), idx);
            }

            Instruction::WriteReg { src, base, .. } => {
                update_lifetime(&mut lifetimes, *base, idx);
                update_lifetime(&mut lifetimes, *src, idx);
            }

            Instruction::ReadMem { dest, src, base, .. } => {
                update_lifetime(&mut lifetimes, *base, idx);
                update_lifetime(&mut lifetimes, Source::Id(*dest), idx);
                update_lifetime(&mut lifetimes, *src, idx);
            }

            Instruction::WriteMem { addr, src, base, .. } => {
                update_lifetime(&mut lifetimes, *base, idx);
                update_lifetime(&mut lifetimes, *addr, idx);
                update_lifetime(&mut lifetimes, *src, idx);
            }

            Instruction::Select {
                dest,
                cond,
                if_true,
                if_false,
            } => {
                update_lifetime(&mut lifetimes, Source::Id(*dest), idx);
                update_lifetime(&mut lifetimes, *cond, idx);
                update_lifetime(&mut lifetimes, *if_true, idx);
                update_lifetime(&mut lifetimes, *if_false, idx);
            }

            Instruction::Ret { addr, code } => {
                update_lifetime(&mut lifetimes, *addr, idx);
                update_lifetime(&mut lifetimes, *code, idx);
            }
        }
    }

    lifetimes
}

pub struct ShowLifetimes<'a, 'b> {
    lifetimes: &'a Lifetimes,
    instrs: &'b [Instruction],
}

impl<'a, 'b> ShowLifetimes<'a, 'b> {
    pub fn new(lifetimes: &'a Lifetimes, instrs: &'b [Instruction]) -> Self {
        Self { lifetimes, instrs }
    }
}

impl fmt::Display for ShowLifetimes<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (idx, instr) in self.instrs.iter().enumerate() {
            let mut lifetimes: Vec<Lifetime> = self.lifetimes.values().cloned().collect();
            lifetimes.sort_by(|a, b| a.start.cmp(&b.start));
            let mut lifetimes = lifetimes.iter();

            write!(f, "[")?;

            fn show_lifetime(
                f: &mut fmt::Formatter,
                lifetime: &Lifetime,
                idx: usize,
            ) -> fmt::Result {
                let width = (lifetime.end - lifetime.start) / 10 + 1;
                if lifetime.is_alive_after(idx) {
                    write!(f, "{:1$}", lifetime.end - idx, width)
                } else {
                    f.write_str(&"-".repeat(width))
                }
            };

            if let Some(lifetime) = lifetimes.next() {
                show_lifetime(f, lifetime, idx)?;
            }

            for lifetime in lifetimes {
                f.write_str(", ")?;
                show_lifetime(f, lifetime, idx)?;
            }

            f.write_str("] ")?;

            writeln!(f, "{}", instr)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::ShowLifetimes;
    use crate::ssa::{lower, opt};
    use crate::{instruction, opcode, register};

    use insta::assert_display_snapshot;

    #[test]
    fn max() {
        let mut instrs = crate::ssa::max_fn();

        opt::fold_and_prop_consts(&mut instrs);
        let instrs = opt::dead_instruction_elimination(&instrs);
        let instrs = opt::register_writeback_shrinking(&instrs);

        let lifetimes = super::lifetimes(&instrs);

        assert_display_snapshot!("max_analysis", ShowLifetimes::new(&lifetimes, &instrs));
    }
}
