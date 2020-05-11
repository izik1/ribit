use crate::ssa::{Id, Instruction, Source, StackIndex};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;

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
    #[must_use]
    pub fn is_alive_after(self, idx: usize) -> bool {
        idx >= self.start && idx <= self.end
    }

    #[must_use]
    pub fn is_empty(self) -> bool {
        self.start == self.end
    }
}

pub type Lifetimes = HashMap<Id, Lifetime>;

fn update_lifetime(lifetimes: &mut Lifetimes, src: Source, idx: usize) {
    if let Some(id) = src.id() {
        lifetimes
            .entry(id)
            .and_modify(|it| it.end = idx)
            .or_insert(Lifetime {
                start: idx,
                end: idx,
            });
    }
}

#[must_use]
pub fn lifetimes(instrs: &[Instruction]) -> Lifetimes {
    let mut lifetimes = HashMap::with_capacity(instrs.len());

    for (idx, instr) in instrs.iter().enumerate() {
        lifetime_instruction(instr, idx, &mut lifetimes, update_lifetime);
    }

    lifetimes
}

#[must_use]
pub fn surrounding_usages(instrs: &[Instruction], needle: usize) -> Lifetimes {
    let mut lifetimes = HashMap::new();

    for (idx, instr) in instrs.iter().enumerate().take(needle) {
        lifetime_instruction(instr, idx, &mut lifetimes, |lifetimes, src, idx| {
            if let Some(id) = src.id() {
                lifetimes.insert(
                    id,
                    Lifetime {
                        start: idx,
                        end: idx,
                    },
                );
            }
        })
    }

    for (idx, instr) in instrs.iter().enumerate().skip(needle) {
        lifetime_instruction(instr, idx, &mut lifetimes, |lifetimes, src, idx| {
            if let Some(id) = src.id() {
                lifetimes.entry(id).and_modify(|it| {
                    if it.is_empty() {
                        it.end = idx
                    }
                });
            }
        })
    }

    lifetimes
}

// todo: replace this with a Vec<...> `rather` than the hashmap,
//  this is because StackIndexes should always be _used_ linearly.
#[must_use]
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

/// Get the highest used `StackIndex` for a given graph.
#[must_use]
pub fn max_stack(graph: &[Instruction]) -> Option<StackIndex> {
    let mut max: Option<StackIndex> = None;

    for instr in graph {
        match instr {
            Instruction::WriteStack {
                dest: stack_idx,
                src: _,
            }
            | Instruction::ReadStack {
                dest: _,
                src: stack_idx,
            } => {
                max = std::cmp::max(max, Some(*stack_idx));
            }

            _ => {}
        }
    }

    max
}

#[must_use]
pub fn min_stack(
    lifetime: Lifetime,
    stack_lts: &HashMap<StackIndex, Vec<(usize, usize)>>,
) -> StackIndex {
    stack_lts
        .iter()
        .filter_map(|(idx, stack_lts)| {
            (stack_lts
                .iter()
                .all(|stack_lt| stack_lt.1 < lifetime.start || stack_lt.0 > lifetime.end))
            .then_some(*idx)
        })
        .min()
        .unwrap_or_else(|| {
            StackIndex(u8::try_from(stack_lts.len()).expect("Ran out of stack indexes"))
        })
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

fn show_lifetime(f: &mut fmt::Formatter, lifetime: &Lifetime, idx: usize) -> fmt::Result {
    let width = (lifetime.end - lifetime.start) / 10 + 1;
    if lifetime.is_alive_after(idx) {
        write!(f, "{:1$}", lifetime.end - idx, width)
    } else {
        f.write_str(&"-".repeat(width))
    }
}

impl fmt::Display for ShowLifetimes<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (idx, instr) in self.instrs.iter().enumerate() {
            let mut lifetimes: Vec<Lifetime> = self.lifetimes.values().cloned().collect();
            lifetimes.sort_by(|a, b| a.start.cmp(&b.start));
            let mut lifetimes = lifetimes.iter();

            write!(f, "[")?;

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
        let (mut instrs, _) = crate::ssa::max_fn();

        opt::fold_and_prop_consts(&mut instrs);
        let instrs = opt::dead_instruction_elimination(&instrs);
        let instrs = opt::register_writeback_shrinking(&instrs);

        let lifetimes = super::lifetimes(&instrs);

        assert_display_snapshot!(ShowLifetimes::new(&lifetimes, &instrs));
    }
}
