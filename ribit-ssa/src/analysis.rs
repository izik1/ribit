use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;

use crate::{Block, Id, Instruction, Source, StackIndex, Terminator};

#[inline]
fn update_source<F: FnMut(&mut Lifetimes, Id, usize)>(
    lifetimes: &mut Lifetimes,
    source: &Source,
    idx: usize,
    update: &mut F,
) {
    if let Source::Ref(r) = source {
        update(lifetimes, r.id, idx)
    }
}

fn lifetime_instruction<F: FnMut(&mut Lifetimes, Id, usize)>(
    instr: &Instruction,
    idx: usize,
    lifetimes: &mut Lifetimes,
    mut update: F,
) {
    match instr {
        Instruction::Fence => {}

        Instruction::BinOp { dest, src1, src2, .. } | Instruction::Cmp { dest, src1, src2, .. } => {
            update(lifetimes, *dest, idx);
            update_source(lifetimes, src1, idx, &mut update);
            update_source(lifetimes, src2, idx, &mut update);
        }

        Instruction::ReadReg { dest, base, .. } => {
            update(lifetimes, *dest, idx);
            update_source(lifetimes, base, idx, &mut update);
        }

        Instruction::Arg { dest, .. } | Instruction::ReadStack { dest, .. } => {
            update(lifetimes, *dest, idx);
        }

        Instruction::WriteStack { dest: _, src } => {
            update(lifetimes, src.id, idx);
        }

        Instruction::WriteReg { src, base, .. } => {
            update_source(lifetimes, base, idx, &mut update);
            update_source(lifetimes, src, idx, &mut update);
        }

        Instruction::ReadMem { dest, src, base, .. } => {
            update(lifetimes, *dest, idx);

            update_source(lifetimes, base, idx, &mut update);
            update_source(lifetimes, src, idx, &mut update);
        }

        Instruction::WriteMem { addr, src, base, .. } => {
            update_source(lifetimes, base, idx, &mut update);
            update_source(lifetimes, addr, idx, &mut update);
            update_source(lifetimes, src, idx, &mut update);
        }

        Instruction::Select { dest, cond, if_true, if_false } => {
            update(lifetimes, *dest, idx);
            update_source(lifetimes, cond, idx, &mut update);
            update_source(lifetimes, if_true, idx, &mut update);
            update_source(lifetimes, if_false, idx, &mut update);
        }
    }
}

fn lifetime_terminator<F: FnMut(&mut Lifetimes, Id, usize)>(
    term: &Terminator,
    idx: usize,
    lifetimes: &mut Lifetimes,
    mut update: F,
) {
    match term {
        Terminator::Ret { addr, code } => {
            update_source(lifetimes, addr, idx, &mut update);
            update_source(lifetimes, code, idx, &mut update);
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

fn update_lifetime(lifetimes: &mut Lifetimes, id: Id, idx: usize) {
    lifetimes.entry(id).and_modify(|it| it.end = idx).or_insert(Lifetime { start: idx, end: idx });
}

#[must_use]
pub fn lifetimes(block: &Block) -> Lifetimes {
    let mut lifetimes = HashMap::with_capacity(block.instructions.len());

    for (idx, instr) in block.instructions.iter().enumerate() {
        lifetime_instruction(instr, idx, &mut lifetimes, update_lifetime);
    }

    lifetime_terminator(
        &block.terminator,
        block.instructions.len(),
        &mut lifetimes,
        update_lifetime,
    );

    lifetimes
}

#[must_use]
pub fn surrounding_usages(block: &Block, needle: usize) -> Lifetimes {
    fn update_post_needle(lifetimes: &mut HashMap<Id, Lifetime>, id: Id, idx: usize) {
        lifetimes.entry(id).and_modify(|it| {
            if it.is_empty() {
                it.end = idx
            }
        });
    }

    let mut lifetimes = HashMap::new();

    for (idx, instr) in block.instructions.iter().enumerate().take(needle) {
        lifetime_instruction(instr, idx, &mut lifetimes, |lifetimes, id, idx| {
            lifetimes.insert(id, Lifetime { start: idx, end: idx });
        })
    }

    for (idx, instr) in block.instructions.iter().enumerate().skip(needle) {
        lifetime_instruction(instr, idx, &mut lifetimes, update_post_needle)
    }

    lifetime_terminator(
        &block.terminator,
        block.instructions.len(),
        &mut lifetimes,
        update_post_needle,
    );

    lifetimes
}

// todo: replace this with a Vec<...> `rather` than the hashmap,
//  this is because StackIndexes should always be _used_ linearly.
#[must_use]
pub fn stack_lifetimes(block: &Block) -> HashMap<StackIndex, Vec<(usize, usize)>> {
    let mut lifetimes = HashMap::new();
    for (idx, instr) in block.instructions.iter().enumerate() {
        match instr {
            Instruction::WriteStack { dest, src: _ } => {
                lifetimes.entry(*dest).or_insert_with(Vec::new).push((idx, idx))
            }
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
pub fn max_stack(instructions: &[Instruction]) -> Option<StackIndex> {
    let mut max: Option<StackIndex> = None;

    for instr in instructions {
        match instr {
            Instruction::WriteStack { dest: stack_idx, src: _ }
            | Instruction::ReadStack { dest: _, src: stack_idx } => {
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
            stack_lts
                .iter()
                .all(|stack_lt| stack_lt.1 < lifetime.start || stack_lt.0 > lifetime.end)
                .then(|| *idx)
        })
        .min()
        .unwrap_or_else(|| {
            StackIndex(u8::try_from(stack_lts.len()).expect("Ran out of stack indexes"))
        })
}

pub struct ShowLifetimes<'a, 'b> {
    lifetimes: &'a Lifetimes,
    instrs: &'b [Instruction],
    terminator: &'b Terminator,
}

impl<'a, 'b> ShowLifetimes<'a, 'b> {
    pub fn new(
        lifetimes: &'a Lifetimes,
        instrs: &'b [Instruction],
        terminator: &'b Terminator,
    ) -> Self {
        Self { lifetimes, instrs, terminator }
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
        let mut lifetimes: Vec<Lifetime> = self.lifetimes.values().cloned().collect();
        lifetimes.sort_by(|a, b| a.start.cmp(&b.start));
        for (idx, instr) in self
            .instrs
            .iter()
            .map(|it| it as &dyn fmt::Display)
            .chain(std::iter::once(self.terminator as &dyn fmt::Display))
            .enumerate()
        {
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
    use expect_test::expect;

    use super::ShowLifetimes;
    use crate::opt;
    use crate::test::{max_fn, MEM_SIZE};

    #[test]
    fn dead_reads_give_valid_lifetimes() {
        enum BinaryInstruction {
            Small(u16),
            Big(u32),
        }

        let instructions = {
            use BinaryInstruction::*;
            vec![
                Big(0b11111110111011011100000010110111),
                Big(0b11101010110100001000000010010011),
                Big(0b11111111011101101110000100110111),
                Big(0b11110101011000010000000100010011),
                Big(0b01111111101110110111000110110111),
                Big(0b11111010101100011000000110010011),
                Big(0b10111111110111011011001000110111),
                Big(0b01111101010100100000001000010011),
                Big(0b00000000000000000000001010010111),
                Big(0b00001101100000101000001010010011),
                Big(0b00000000000000000010001100010111),
                Big(0b11111101100000110000001100010011),
                Big(0b10110111111110111011001110110111),
                Big(0b01101111101000111000001110010011),
                Big(0b01011011111111011110010000110111),
                Big(0b10110111110101000000010000010011),
                Big(0b10101101111111101111010010110111),
                Big(0b11011011111001001000010010010011),
                Big(0b01010110111111110111010100110111),
                Big(0b01101101111101010000010100010011),
                Big(0b10101011011111111100010110110111),
                Big(0b10110110111101011000010110010011),
                Big(0b11010101101111111110011000110111),
                Big(0b11011011011101100000011000010011),
                Big(0b11101010110111111111011010110111),
                Big(0b11101101101101101000011010010011),
                Big(0b11110101011011111111011100110111),
                Big(0b01110110110101110000011100010011),
                Big(0b11111010101110000000011110110111),
                Big(0b10111011011001111000011110010011),
                Big(0b01111101010111000000100000110111),
                Big(0b11011101101110000000100000010011),
                Big(0b10111110101011100000100010110111),
                Big(0b11101110110110001000100010010011),
                Big(0b11011111010101110000100100110111),
                Big(0b11110111011010010000100100010011),
                Big(0b01101111101010111000100110110111),
                Big(0b11111011101110011000100110010011),
                Big(0b10110111110101011100101000110111),
                Big(0b11111101110110100000101000010011),
                Big(0b11011011111010101110101010110111),
                Big(0b11111110111010101000101010010011),
                Big(0b01101101111101010111101100110111),
                Big(0b11111111011110110000101100010011),
                Big(0b10110110111110101011101110110111),
                Big(0b01111111101110111000101110010011),
                Big(0b11011011011111010110110000110111),
                Big(0b10111111110111000000110000010011),
                Big(0b11101101101111101011110010110111),
                Big(0b11011111111011001000110010010011),
                Big(0b01110110110111110101110100110111),
                Big(0b01101111111111010000110100010011),
                Big(0b10111011011011111011110110110111),
                Big(0b10110111111111011000110110010011),
                Big(0b11011101101101111101111000110111),
                Big(0b01011011111111100000111000010011),
                Big(0b11101110110110111111111010110111),
                Big(0b10101101111111101000111010010011),
                Big(0b11110111011011011111111100110111),
                Big(0b01010110111111110000111100010011),
                Big(0b11111011101101110000111110110111),
                Big(0b10101011011111111000111110010011),
                Big(0b00000000000000000010000010010111),
                Big(0b00010000110000001000000010010011),
                Big(0b00000000000000000010011000010111),
                Big(0b11101101000001100000011000010011),
                Small(0b0101101000010000),
                Small(0b0000000000000001),
                Small(0b0000000000000001),
                Big(0b00000000110000001010000000100011),
                Big(0b00000000000000000010010100010111),
                Big(0b11101110111001010000010100010011),
                Small(0b0100000100001100),
                Small(0b0000000000000001),
                Small(0b0000000000000001),
                Big(0b00000000101100001010001000100011),
                Big(0b00000000000000000010010010010111),
                Big(0b11101101100001001000010010010011),
                Small(0b0100000011011000),
                Small(0b0000000000000001),
                Small(0b0000000000000001),
                Big(0b00000000111000001010010000100011),
                Big(0b00000000000000000010011110010111),
                Big(0b11101100001001111000011110010011),
                Small(0b0100011110010100),
                Small(0b0000000000000001),
                Small(0b0000000000000001),
                Big(0b00000000110100001010011000100011),
                Big(0b00000000000000000010011100010111),
                Big(0b11101010100001110000011100010011),
                Small(0b0100101100001000),
                Small(0b0000000000000001),
                Small(0b0000000000000001),
                Big(0b00000000101000001010100000100011),
                Big(0b00000000000000000010010000010111),
                Big(0b11101000011001000000010000010011),
                Small(0b0101000000011100),
                Small(0b0000000000000001),
                Small(0b0000000000000001),
                Big(0b00000000111100001010101000100011),
                Big(0b00000000000000000010010110010111),
                Big(0b11100101010001011000010110010011),
                Small(0b0100000110100000),
                Small(0b0000000000000001),
                Small(0b0000000000000001),
                Big(0b00000000100000001010110000100011),
                Big(0b00000000000000000010011010010111),
                Big(0b11100000101001101000011010010011),
                Small(0b0101111010100100),
                Small(0b0000000000000001),
                Small(0b0000000000000001),
                Big(0b00000000100100001010111000100011),
                Big(0b00000000000000000010010110010111),
                Big(0b11011111110001011000010110010011),
                Small(0b0101100111101000),
                Small(0b0000000000000001),
                Small(0b0000000000000001),
                Big(0b00000010101000001010000000100011),
                Big(0b00000000000000000010010110010111),
                Big(0b11011111001001011000010110010011),
                Small(0b0101010111101000),
                Small(0b0000000000000001),
                Small(0b0000000000000001),
                Big(0b00000010101000001010001000100011),
                Big(0b00000000000000000010010110010111),
                Big(0b11011111000001011000010110010011),
                Small(0b0100110111101000),
                Small(0b0000000000000001),
                Small(0b0000000000000001),
                Big(0b00000010101000001010010000100011),
                Big(0b00000000000000000010010110010111),
                Big(0b11011111111001011000010110010011),
                Small(0b0101110111001000),
                Small(0b0000000000000001),
                Small(0b0000000000000001),
                Big(0b00000010101000001010011000100011),
                Big(0b00000000000000000010010110010111),
                Big(0b11011101010001011000010110010011),
                Small(0b0100100111101000),
                Small(0b0000000000000001),
                Small(0b0000000000000001),
                Big(0b00000010101000001010100000100011),
                Big(0b00000000000000000010010110010111),
                Big(0b11011110111001011000010110010011),
                Small(0b0101010110001000),
                Small(0b0000000000000001),
                Small(0b0000000000000001),
                Big(0b00000010101000001010101000100011),
                Big(0b00000000000000000000000000010011),
                Small(0b0100000010000101),
                Big(0b00000000000000000001111100010111),
                Big(0b11011110000111110010111100100011),
                Small(0b1011111111100101),
            ]
        };

        let mut ctx = crate::lower::Context::new(0x10000, MEM_SIZE);

        let (last, rest) = instructions.split_last().unwrap();

        for instr in rest {
            match instr {
                BinaryInstruction::Big(it) => {
                    crate::lower::non_terminal(
                        &mut ctx,
                        ribit_decode::instruction(*it).unwrap(),
                        4,
                    );
                }

                BinaryInstruction::Small(it) => {
                    crate::lower::non_terminal(
                        &mut ctx,
                        ribit_decode::compressed::decode_instruction(*it).unwrap(),
                        2,
                    );
                }
            }
        }

        let mut block;
        match last {
            BinaryInstruction::Big(it) => {
                block = crate::lower::terminal(ctx, ribit_decode::instruction(*it).unwrap(), 4);
            }

            BinaryInstruction::Small(it) => {
                block = crate::lower::terminal(
                    ctx,
                    ribit_decode::compressed::decode_instruction(*it).unwrap(),
                    2,
                );
            }
        };

        opt::fold_and_prop_consts(&mut block);
        opt::dead_instruction_elimination(&mut block);
        opt::register_writeback_shrinking(&mut block);

        let lifetimes = super::lifetimes(&mut block);

        expect![[r#"
            [     61, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] %0 = args[0]
            [     60,   34, -, -, -, -, -, -, -, -, -, -, -, -, -, -] %1 = args[1]
            [     59,   33, 2, -, -, -, -, -, -, -, -, -, -, -, -, -] %2 = signed dword m(%1)00012000
            [     58,   32, 1, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)12 = %2
            [     57,   31, 0, -, -, -, -, -, -, -, -, -, -, -, -, -] m(%1)00012204 = dword %2
            [     56,   30, -, 1, -, -, -, -, -, -, -, -, -, -, -, -] %3 = signed dword m(%1)00012000
            [     55,   29, -, 0, -, -, -, -, -, -, -, -, -, -, -, -] m(%1)00012208 = dword %3
            [     54,   28, -, -, 1, -, -, -, -, -, -, -, -, -, -, -] %4 = signed dword m(%1)00012000
            [     53,   27, -, -, 0, -, -, -, -, -, -, -, -, -, -, -] m(%1)0001220c = dword %4
            [     52,   26, -, -, -, 1, -, -, -, -, -, -, -, -, -, -] %5 = signed dword m(%1)00012000
            [     51,   25, -, -, -, 0, -, -, -, -, -, -, -, -, -, -] m(%1)00012210 = dword %5
            [     50,   24, -, -, -, -, 1, -, -, -, -, -, -, -, -, -] %6 = signed dword m(%1)00012000
            [     49,   23, -, -, -, -, 0, -, -, -, -, -, -, -, -, -] m(%1)00012214 = dword %6
            [     48,   22, -, -, -, -, -, 2, -, -, -, -, -, -, -, -] %7 = signed dword m(%1)00012000
            [     47,   21, -, -, -, -, -, 1, -, -, -, -, -, -, -, -] x(%0)15 = %7
            [     46,   20, -, -, -, -, -, 0, -, -, -, -, -, -, -, -] m(%1)00012218 = dword %7
            [     45,   19, -, -, -, -, -, -, 2, -, -, -, -, -, -, -] %8 = signed dword m(%1)00012000
            [     44,   18, -, -, -, -, -, -, 1, -, -, -, -, -, -, -] x(%0)8 = %8
            [     43,   17, -, -, -, -, -, -, 0, -, -, -, -, -, -, -] m(%1)0001221c = dword %8
            [     42,   16, -, -, -, -, -, -, -, 2, -, -, -, -, -, -] %9 = signed dword m(%1)00012000
            [     41,   15, -, -, -, -, -, -, -, 1, -, -, -, -, -, -] x(%0)9 = %9
            [     40,   14, -, -, -, -, -, -, -, 0, -, -, -, -, -, -] m(%1)00012220 = dword %9
            [     39,   13, -, -, -, -, -, -, -, -, 1, -, -, -, -, -] %10 = signed dword m(%1)00012000
            [     38,   12, -, -, -, -, -, -, -, -, 0, -, -, -, -, -] m(%1)00012224 = dword %10
            [     37,   11, -, -, -, -, -, -, -, -, -, 1, -, -, -, -] %11 = signed dword m(%1)00012000
            [     36,   10, -, -, -, -, -, -, -, -, -, 0, -, -, -, -] m(%1)00012228 = dword %11
            [     35,    9, -, -, -, -, -, -, -, -, -, -, 1, -, -, -] %12 = signed dword m(%1)00012000
            [     34,    8, -, -, -, -, -, -, -, -, -, -, 0, -, -, -] m(%1)0001222c = dword %12
            [     33,    7, -, -, -, -, -, -, -, -, -, -, -, 1, -, -] %13 = signed dword m(%1)00012000
            [     32,    6, -, -, -, -, -, -, -, -, -, -, -, 0, -, -] m(%1)00012230 = dword %13
            [     31,    5, -, -, -, -, -, -, -, -, -, -, -, -, 1, -] %14 = signed dword m(%1)00012000
            [     30,    4, -, -, -, -, -, -, -, -, -, -, -, -, 0, -] m(%1)00012234 = dword %14
            [     29,    3, -, -, -, -, -, -, -, -, -, -, -, -, -, 2] %15 = signed dword m(%1)00012000
            [     28,    2, -, -, -, -, -, -, -, -, -, -, -, -, -, 1] x(%0)10 = %15
            [     27,    1, -, -, -, -, -, -, -, -, -, -, -, -, -, 0] m(%1)00012238 = dword %15
            [     26,    0, -, -, -, -, -, -, -, -, -, -, -, -, -, -] m(%1)00011000 = dword 00000001
            [     25, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)1 = 00000001
            [     24, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)2 = ff76df56
            [     23, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)3 = 7fbb6fab
            [     22, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)4 = bfddb7d5
            [     21, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)5 = 000100f8
            [     20, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)6 = 00012000
            [     19, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)7 = b7fbb6fa
            [     18, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)11 = 00011fd8
            [     17, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)13 = 00011f88
            [     16, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)14 = 00011ff0
            [     15, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)16 = 7d5bfddb
            [     14, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)17 = beadfeed
            [     13, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)18 = df56ff76
            [     12, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)19 = 6fab7fbb
            [     11, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)20 = b7d5bfdd
            [     10, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)21 = dbeadfee
            [      9, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)22 = 6df56ff7
            [      8, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)23 = b6fab7fb
            [      7, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)24 = db7d5bfd
            [      6, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)25 = edbeadfe
            [      5, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)26 = 76df56ff
            [      4, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)27 = bb6fab7f
            [      3, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)28 = ddb7d5bf
            [      2, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)29 = eedbeadf
            [      1, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)30 = 00011202
            [      0, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%0)31 = fbb6fab7
            [-------, ----, -, -, -, -, -, -, -, -, -, -, -, -, -, -] ret 00000000, 00010202
        "#]].assert_eq(
            &ShowLifetimes::new(&lifetimes, &block.instructions, &block.terminator).to_string(),
        );

        // code block:
        // LUI x1, 4276994048
        // ADDI x1, x1, 3757
        // LUI x2, 4285980672
        // ADDI x2, x2, 3926
        // LUI x3, 2142990336
        // ADDI x3, x3, 4011
        // LUI x4, 3218976768
        // ADDI x4, x4, 2005
        // AUIPC x5, 0
        // ADDI x5, x5, 216
        // AUIPC x6, 8192
        // ADDI x6, x6, 4056
        // LUI x7, 3086725120
        // ADDI x7, x7, 1786
        // LUI x8, 1543364608
        // ADDI x8, x8, 2941
        // LUI x9, 2919165952
        // ADDI x9, x9, 3518
        // LUI x10, 1459580928
        // ADDI x10, x10, 1759
        // LUI x11, 2877276160
        // ADDI x11, x11, 2927
        // LUI x12, 3586121728
        // ADDI x12, x12, 3511
        // LUI x13, 3940544512
        // ADDI x13, x13, 3803
        // LUI x14, 4117753856
        // ADDI x14, x14, 1901
        // LUI x15, 4206362624
        // ADDI x15, x15, 2998
        // LUI x16, 2103181312
        // ADDI x16, x16, 3547
        // LUI x17, 3199074304
        // ADDI x17, x17, 3821
        // LUI x18, 3747020800
        // ADDI x18, x18, 3958
        // LUI x19, 1873510400
        // ADDI x19, x19, 4027
        // LUI x20, 3084238848
        // ADDI x20, x20, 4061
        // LUI x21, 3689603072
        // ADDI x21, x21, 4078
        // LUI x22, 1844801536
        // ADDI x22, x22, 4087
        // LUI x23, 3069882368
        // ADDI x23, x23, 2043
        // LUI x24, 3682426880
        // ADDI x24, x24, 3069
        // LUI x25, 3988697088
        // ADDI x25, x25, 3582
        // LUI x26, 1994346496
        // ADDI x26, x26, 1791
        // LUI x27, 3144658944
        // ADDI x27, x27, 2943
        // LUI x28, 3719811072
        // ADDI x28, x28, 1471
        // LUI x29, 4007391232
        // ADDI x29, x29, 2783
        // LUI x30, 4151177216
        // ADDI x30, x30, 1391
        // LUI x31, 4223074304
        // ADDI x31, x31, 2743
        // AUIPC x1, 8192
        // ADDI x1, x1, 268
        // AUIPC x12, 8192
        // ADDI x12, x12, 3792
        // C.LW x28, 48(x28)
        // C.ADDI x0, x0, 0
        // C.ADDI x0, x0, 0
        // SW x12, x1(0)
        // AUIPC x10, 8192
        // ADDI x10, x10, 3822
        // C.LW x27, 0(x26)
        // C.ADDI x0, x0, 0
        // C.ADDI x0, x0, 0
        // SW x11, x1(4)
        // AUIPC x9, 8192
        // ADDI x9, x9, 3800
        // C.LW x30, 4(x25)
        // C.ADDI x0, x0, 0
        // C.ADDI x0, x0, 0
        // SW x14, x1(8)
        // AUIPC x15, 8192
        // ADDI x15, x15, 3778
        // C.LW x29, 8(x31)
        // C.ADDI x0, x0, 0
        // C.ADDI x0, x0, 0
        // SW x13, x1(12)
        // AUIPC x14, 8192
        // ADDI x14, x14, 3752
        // C.LW x26, 16(x30)
        // C.ADDI x0, x0, 0
        // C.ADDI x0, x0, 0
        // SW x10, x1(16)
        // AUIPC x8, 8192
        // ADDI x8, x8, 3718
        // C.LW x31, 32(x24)
        // C.ADDI x0, x0, 0
        // C.ADDI x0, x0, 0
        // SW x15, x1(20)
        // AUIPC x11, 8192
        // ADDI x11, x11, 3668
        // C.LW x24, 64(x27)
        // C.ADDI x0, x0, 0
        // C.ADDI x0, x0, 0
        // SW x8, x1(24)
        // AUIPC x13, 8192
        // ADDI x13, x13, 3594
        // C.LW x25, 120(x29)
        // C.ADDI x0, x0, 0
        // C.ADDI x0, x0, 0
        // SW x9, x1(28)
        // AUIPC x11, 8192
        // ADDI x11, x11, 3580
        // C.LW x26, 116(x27)
        // C.ADDI x0, x0, 0
        // C.ADDI x0, x0, 0
        // SW x10, x1(32)
        // AUIPC x11, 8192
        // ADDI x11, x11, 3570
        // C.LW x26, 108(x27)
        // C.ADDI x0, x0, 0
        // C.ADDI x0, x0, 0
        // SW x10, x1(36)
        // AUIPC x11, 8192
        // ADDI x11, x11, 3568
        // C.LW x26, 92(x27)
        // C.ADDI x0, x0, 0
        // C.ADDI x0, x0, 0
        // SW x10, x1(40)
        // AUIPC x11, 8192
        // ADDI x11, x11, 3582
        // C.LW x26, 60(x27)
        // C.ADDI x0, x0, 0
        // C.ADDI x0, x0, 0
        // SW x10, x1(44)
        // AUIPC x11, 8192
        // ADDI x11, x11, 3540
        // C.LW x26, 84(x27)
        // C.ADDI x0, x0, 0
        // C.ADDI x0, x0, 0
        // SW x10, x1(48)
        // AUIPC x11, 8192
        // ADDI x11, x11, 3566
        // C.LW x26, 40(x27)
        // C.ADDI x0, x0, 0
        // C.ADDI x0, x0, 0
        // SW x10, x1(52)
        // ADDI x0, x0, 0
        // C.ADDI x1, x0, 1
        // AUIPC x30, 4096
        // SW x1, x30(65022)
        // C.JAL x0, 4294967288
    }

    #[test]
    fn max_lifetimes() {
        let mut block = max_fn();

        opt::fold_and_prop_consts(&mut block);
        opt::dead_instruction_elimination(&mut block);
        opt::register_writeback_shrinking(&mut block);

        let lifetimes = super::lifetimes(&mut block);

        expect![[r#"
            [10, -, -, -, -, -, -, -, -] %0 = args[0]
            [ 9, 7, -, -, -, -, -, -, -] %2 = x(%0)10
            [ 8, 6, 1, -, -, -, -, -, -] %3 = x(%0)11
            [ 7, 5, 0, 3, -, -, -, -, -] %4 = add %2, %3
            [ 6, 4, -, 2, 2, -, -, -, -] %5 = srl %4, 0000001f
            [ 5, 3, -, 1, 1, -, -, -, -] x(%0)12 = %5
            [ 4, 2, -, 0, 0, 2, -, -, -] %6 = and %4, %5
            [ 3, 1, -, -, -, 1, -, -, -] x(%0)11 = %6
            [ 2, 0, -, -, -, 0, 1, -, -] %7 = add %2, %6
            [ 1, -, -, -, -, -, 0, -, -] x(%0)10 = %7
            [ 0, -, -, -, -, -, -, 1, -] %8 = x(%0)1
            [--, -, -, -, -, -, -, 0, 1] %9 = and %8, fffffffe
            [--, -, -, -, -, -, -, -, 0] ret 00000000, %9
        "#]].assert_eq(
            &ShowLifetimes::new(&lifetimes, &block.instructions, &block.terminator).to_string(),
        );
    }
}
