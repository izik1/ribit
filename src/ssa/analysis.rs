use crate::ssa::{Id, Instruction, Source, StackIndex};
use std::collections::HashMap;
use std::{fmt, mem};

fn lifetime_instruction<F: FnMut(&mut Lifetimes, Source, usize)>(
    instr: &Instruction,
    idx: usize,
    lifetimes: &mut Lifetimes,
    mut update: F,
) {
    match instr {
        Instruction::Fence => {}

        Instruction::BinOp {
            dest, src1, src2, ..
        }
        | Instruction::Cmp {
            dest, src1, src2, ..
        } => {
            update(lifetimes, Source::Id(*dest), idx);
            update(lifetimes, *src1, idx);
            update(lifetimes, *src2, idx);
        }

        Instruction::ReadReg { dest, base, .. } => {
            update(lifetimes, *base, idx);
            update(lifetimes, Source::Id(*dest), idx);
        }

        Instruction::Arg { dest, .. }
        | Instruction::LoadConst { dest, .. }
        | Instruction::ReadStack { dest, .. } => {
            update(lifetimes, Source::Id(*dest), idx);
        }

        Instruction::WriteStack { dest: _, src } => {
            update(lifetimes, Source::Id(*src), idx);
        }

        Instruction::WriteReg { src, base, .. } => {
            update(lifetimes, *base, idx);
            update(lifetimes, *src, idx);
        }

        Instruction::ReadMem {
            dest, src, base, ..
        } => {
            update(lifetimes, *base, idx);
            update(lifetimes, Source::Id(*dest), idx);
            update(lifetimes, *src, idx);
        }

        Instruction::WriteMem {
            addr, src, base, ..
        } => {
            update(lifetimes, *base, idx);
            update(lifetimes, *addr, idx);
            update(lifetimes, *src, idx);
        }

        Instruction::Select {
            dest,
            cond,
            if_true,
            if_false,
        } => {
            update(lifetimes, Source::Id(*dest), idx);
            update(lifetimes, *cond, idx);
            update(lifetimes, *if_true, idx);
            update(lifetimes, *if_false, idx);
        }

        Instruction::Ret { addr, code } => {
            update(lifetimes, *addr, idx);
            update(lifetimes, *code, idx);
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Lifetime {
    pub start: usize,
    pub end: usize,
}

impl Lifetime {
    pub fn is_alive_after(self, idx: usize) -> bool {
        idx >= self.start && idx <= self.end
    }

    pub fn is_empty(self) -> bool {
        self.start == self.end
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
        lifetime_instruction(instr, idx, &mut lifetimes, update_lifetime);
    }

    lifetimes
}

// todo: replace this with a Vec<...> rather than the hashmap,
//  this is because StackIndexes should always be _used_ linearly.
pub fn stack_lifetimes(graph: &[Instruction]) -> HashMap<StackIndex, Vec<(usize, usize)>> {
    let mut lifetimes = HashMap::new();
    for (idx, instr) in graph.iter().enumerate() {
        match instr {
            Instruction::WriteStack { dest, src: _ } => lifetimes
                .entry(*dest)
                .or_insert_with(Vec::new)
                .push((idx, idx)),
            Instruction::ReadStack { dest: _, src } => {
                lifetimes.get_mut(src).unwrap().last_mut().unwrap().1 = idx;
            }
            _ => {}
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
    use crate::ssa::opt;

    use insta::assert_display_snapshot;

    #[test]
    fn max_lifetimes() {
        let mut instrs = crate::ssa::max_fn();

        opt::fold_and_prop_consts(&mut instrs);
        let instrs = opt::dead_instruction_elimination(&instrs);
        let instrs = opt::register_writeback_shrinking(&instrs);

        let lifetimes = super::lifetimes(&instrs);

        assert_display_snapshot!(ShowLifetimes::new(&lifetimes, &instrs));
    }
}
