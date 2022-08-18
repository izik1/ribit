use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt::{self, Write};

use crate::{Block, Id, Instruction, Source, StackIndex, Terminator};

fn lifetime_instruction<F: FnMut(&mut Lifetimes, Id, usize)>(
    instr: &Instruction,
    idx: usize,
    lifetimes: &mut Lifetimes,
    mut update: F,
) {
    if let Some(id) = instr.id() {
        update(lifetimes, id, idx);
    }

    instr.visit_arg_ids(|id| update(lifetimes, id, idx));
}

fn lifetime_terminator<F: FnOnce(&mut Lifetimes, Id, usize)>(
    term: &Terminator,
    idx: usize,
    lifetimes: &mut Lifetimes,
    update: F,
) {
    match term {
        Terminator::Ret { addr, .. } => {
            if let Source::Ref(id) = addr {
                update(lifetimes, *id, idx);
            }
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
                it.end = idx;
            }
        });
    }

    let mut lifetimes = HashMap::new();

    for (idx, instr) in block.instructions.iter().enumerate().take(needle) {
        lifetime_instruction(instr, idx, &mut lifetimes, |lifetimes, id, idx| {
            lifetimes.insert(id, Lifetime { start: idx, end: idx });
        });
    }

    for (idx, instr) in block.instructions.iter().enumerate().skip(needle) {
        lifetime_instruction(instr, idx, &mut lifetimes, update_post_needle);
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
                lifetimes.entry(*dest).or_insert_with(Vec::new).push((idx, idx));
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
    terminator: &'b Terminator,
}

impl<'a, 'b> ShowLifetimes<'a, 'b> {
    #[must_use]
    pub fn new(
        lifetimes: &'a Lifetimes,
        instrs: &'b [Instruction],
        terminator: &'b Terminator,
    ) -> Self {
        Self { lifetimes, instrs, terminator }
    }
}

fn log10(mut v: usize) -> usize {
    let mut result = 0;
    while v >= 10 {
        v /= 10;
        result += 1;
    }

    result
}

fn show_lifetime(f: &mut fmt::Formatter, lifetime: &Lifetime, idx: usize) -> fmt::Result {
    let width = log10(lifetime.end - lifetime.start) + 1;
    if lifetime.is_alive_after(idx) {
        write!(f, "{:1$}", lifetime.end - idx, width)
    } else {
        f.write_str(&"-".repeat(width))
    }
}

impl fmt::Display for ShowLifetimes<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut lifetimes: Vec<Lifetime> = self.lifetimes.values().copied().collect();
        lifetimes.sort_by(|a, b| a.start.cmp(&b.start));
        for (idx, instr) in self
            .instrs
            .iter()
            .map(|it| it as &dyn fmt::Display)
            .chain(std::iter::once(self.terminator as &dyn fmt::Display))
            .enumerate()
        {
            let mut lifetimes = lifetimes.iter();

            f.write_char('[')?;

            if let Some(lifetime) = lifetimes.next() {
                show_lifetime(f, lifetime, idx)?;
            }

            for lifetime in lifetimes {
                f.write_str(", ")?;
                show_lifetime(f, lifetime, idx)?;
            }

            writeln!(f, "] {instr}")?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use expect_test::expect;

    use super::ShowLifetimes;
    use crate::opt;
    use crate::opt::pass_manager::InplacePass;
    use crate::tests::{max_fn, MEM_SIZE};

    #[test]
    fn dead_reads_give_valid_lifetimes() {
        let mut block = crate::tests::assemble_block_with_context(
            crate::lower::Context::new(0x10000, MEM_SIZE),
            r#"
                lui x1, 0xfeedc
                addi x1, x1, 0xead
                lui x2, 0xff76e
                addi x2, x2, 0xf56
                lui x3, 0x7fbb7
                addi x3, x3, 0xfab
                lui x4, 0xbfddb
                addi x4, x4, 0x7d5
                auipc x5, 0
                addi x5, x5, 0xd8
                auipc x6, 2
                addi x6, x6, 0xfd8
                lui x7, 0xb7fbb
                addi x7, x7, 0x6fa
                lui x8, 0x5bfde
                addi x8, x8, 0xb7d
                lui x9, 0xadfef
                addi x9, x9, 0xdbe
                lui x10, 0x56ff7
                addi x10, x10, 0x6df
                lui x11, 0xab7fc
                addi x11, x11, 0xb6f
                lui x12, 0xd5bfe
                addi x12, x12, 0xdb7
                lui x13, 0xeadff
                addi x13, x13, 0xedb
                lui x14, 0xf56ff
                addi x14, x14, 0x76d
                lui x15, 0xfab80
                addi x15, x15, 0xbb6
                lui x16, 0x7d5c0
                addi x16, x16, 0xddb
                lui x17, 0xbeae0
                addi x17, x17, 0xeed
                lui x18, 0xdf570
                addi x18, x18, 0xf76
                lui x19, 0x6fab8
                addi x19, x19, 0xfbb
                lui x20, 0xb7d5c
                addi x20, x20, 0xfdd
                lui x21, 0xdbeae
                addi x21, x21, 0xfee
                lui x22, 0x6df57
                addi x22, x22, 0xff7
                lui x23, 0xb6fab
                addi x23, x23, 0x7fb
                lui x24, 0xdb7d6
                addi x24, x24, 0xbfd
                lui x25, 0xedbeb
                addi x25, x25, 0xdfe
                lui x26, 0x76df5
                addi x26, x26, 0x6ff
                lui x27, 0xbb6fb
                addi x27, x27, 0xb7f
                lui x28, 0xddb7d
                addi x28, x28, 0x5bf
                lui x29, 0xeedbf
                addi x29, x29, 0xadf
                lui x30, 0xf76df
                addi x30, x30, 0x56f
                lui x31, 0xfbb70
                addi x31, x31, 0xab7
                auipc x1, 2                
                addi x1, x1, 0x10c
                auipc x12, 2
                addi x12, x12, 0xed0
                c.lw x12, 0xc(x12)
                c.nop
                c.nop
                sw x12, 0(x1)
                auipc x10, 2
                addi x10, x10, 0xeee
                c.lw x11, 0(x10)
                c.nop
                c.nop
                sw x11, 4(x1)
                auipc x9, 2
                addi x9, x9, 3800
                c.lw x14, 1(x9)
                c.nop
                c.nop
                sw x14, 8(x1)
                auipc x15, 2
                addi x15, x15, 3778
                c.lw x13, 2(x15)
                c.nop
                c.nop
                sw x13, 12(x1)
                auipc x14, 2
                addi x14, x14, 3752
                c.lw x10, 4(x14)
                c.nop
                c.nop
                sw x10, 16(x1)
                auipc x8, 2
                addi x8, x8, 3718
                c.lw x15, 8(x8)
                c.nop
                c.nop
                sw x15, 20(x1)
                auipc x11, 2
                addi x11, x11, 3668
                c.lw x8, 16(x11)
                c.nop
                c.nop
                sw x8, 24(x1)
                auipc x13, 2
                addi x13, x13, 3594
                c.lw x9, 30(x13)
                c.nop
                c.nop
                sw x9, 28(x1)
                auipc x11, 2
                addi x11, x11, 3580
                c.lw x10, 29(x11)
                c.nop
                c.nop
                sw x10, 32(x1)
                auipc x11, 2
                addi x11, x11, 3570
                c.lw x10, 27(x11)
                c.nop
                c.nop
                sw x10, 36(x1)
                auipc x11, 2
                addi x11, x11, 3568
                c.lw x10, 23(x11)
                c.nop
                c.nop
                sw x10, 40(x1)
                auipc x11, 2
                addi x11, x11, 3582
                c.lw x10, 15(x11)
                c.nop
                c.nop
                sw x10, 44(x1)
                auipc x11, 2
                addi x11, x11, 3540
                c.lw x10, 21(x11)
                c.nop
                c.nop
                sw x10, 0x30(x1)
                auipc x11, 2
                addi x11, x11, 3566
                c.lw x10, 10(x11)
                c.nop
                c.nop
                sw x10, 0x34(x1)
                addi x0, x0, 0
                c.li x1, 1
                auipc x30, 1
                sw x1, 0xdfe(x30)
                c.j -4
            "#,
        );

        opt::PassManager::optimized().run(&mut block);

        let lifetimes = super::lifetimes(&mut block);

        expect![[r#"
            [61, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] %2 = args[0]
            [60, 34, -, -, -, -, -, -, -, -, -, -, -, -, -, -] %3 = args[1]
            [59, 33, 2, -, -, -, -, -, -, -, -, -, -, -, -, -] %4 = signed dword m(%3)00012000
            [58, 32, 1, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)12 = %4
            [57, 31, 0, -, -, -, -, -, -, -, -, -, -, -, -, -] m(%3)00012204 = dword %4
            [56, 30, -, 1, -, -, -, -, -, -, -, -, -, -, -, -] %5 = signed dword m(%3)00012000
            [55, 29, -, 0, -, -, -, -, -, -, -, -, -, -, -, -] m(%3)00012208 = dword %5
            [54, 28, -, -, 1, -, -, -, -, -, -, -, -, -, -, -] %6 = signed dword m(%3)00012000
            [53, 27, -, -, 0, -, -, -, -, -, -, -, -, -, -, -] m(%3)0001220c = dword %6
            [52, 26, -, -, -, 1, -, -, -, -, -, -, -, -, -, -] %7 = signed dword m(%3)00012000
            [51, 25, -, -, -, 0, -, -, -, -, -, -, -, -, -, -] m(%3)00012210 = dword %7
            [50, 24, -, -, -, -, 1, -, -, -, -, -, -, -, -, -] %8 = signed dword m(%3)00012000
            [49, 23, -, -, -, -, 0, -, -, -, -, -, -, -, -, -] m(%3)00012214 = dword %8
            [48, 22, -, -, -, -, -, 2, -, -, -, -, -, -, -, -] %9 = signed dword m(%3)00012000
            [47, 21, -, -, -, -, -, 1, -, -, -, -, -, -, -, -] x(%2)15 = %9
            [46, 20, -, -, -, -, -, 0, -, -, -, -, -, -, -, -] m(%3)00012218 = dword %9
            [45, 19, -, -, -, -, -, -, 2, -, -, -, -, -, -, -] %10 = signed dword m(%3)00012000
            [44, 18, -, -, -, -, -, -, 1, -, -, -, -, -, -, -] x(%2)8 = %10
            [43, 17, -, -, -, -, -, -, 0, -, -, -, -, -, -, -] m(%3)0001221c = dword %10
            [42, 16, -, -, -, -, -, -, -, 2, -, -, -, -, -, -] %11 = signed dword m(%3)00012000
            [41, 15, -, -, -, -, -, -, -, 1, -, -, -, -, -, -] x(%2)9 = %11
            [40, 14, -, -, -, -, -, -, -, 0, -, -, -, -, -, -] m(%3)00012220 = dword %11
            [39, 13, -, -, -, -, -, -, -, -, 1, -, -, -, -, -] %12 = signed dword m(%3)00012000
            [38, 12, -, -, -, -, -, -, -, -, 0, -, -, -, -, -] m(%3)00012224 = dword %12
            [37, 11, -, -, -, -, -, -, -, -, -, 1, -, -, -, -] %13 = signed dword m(%3)00012000
            [36, 10, -, -, -, -, -, -, -, -, -, 0, -, -, -, -] m(%3)00012228 = dword %13
            [35,  9, -, -, -, -, -, -, -, -, -, -, 1, -, -, -] %14 = signed dword m(%3)00012000
            [34,  8, -, -, -, -, -, -, -, -, -, -, 0, -, -, -] m(%3)0001222c = dword %14
            [33,  7, -, -, -, -, -, -, -, -, -, -, -, 1, -, -] %15 = signed dword m(%3)00012000
            [32,  6, -, -, -, -, -, -, -, -, -, -, -, 0, -, -] m(%3)00012230 = dword %15
            [31,  5, -, -, -, -, -, -, -, -, -, -, -, -, 1, -] %16 = signed dword m(%3)00012000
            [30,  4, -, -, -, -, -, -, -, -, -, -, -, -, 0, -] m(%3)00012234 = dword %16
            [29,  3, -, -, -, -, -, -, -, -, -, -, -, -, -, 2] %17 = signed dword m(%3)00012000
            [28,  2, -, -, -, -, -, -, -, -, -, -, -, -, -, 1] x(%2)10 = %17
            [27,  1, -, -, -, -, -, -, -, -, -, -, -, -, -, 0] m(%3)00012238 = dword %17
            [26,  0, -, -, -, -, -, -, -, -, -, -, -, -, -, -] m(%3)00011000 = dword 00000001
            [25, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)1 = 00000001
            [24, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)2 = ff76df56
            [23, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)3 = 7fbb6fab
            [22, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)4 = bfddb7d5
            [21, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)5 = 000100f8
            [20, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)6 = 00012000
            [19, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)7 = b7fbb6fa
            [18, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)11 = 00011fd8
            [17, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)13 = 00011f88
            [16, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)14 = 00011ff0
            [15, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)16 = 7d5bfddb
            [14, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)17 = beadfeed
            [13, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)18 = df56ff76
            [12, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)19 = 6fab7fbb
            [11, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)20 = b7d5bfdd
            [10, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)21 = dbeadfee
            [ 9, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)22 = 6df56ff7
            [ 8, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)23 = b6fab7fb
            [ 7, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)24 = db7d5bfd
            [ 6, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)25 = edbeadfe
            [ 5, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)26 = 76df56ff
            [ 4, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)27 = bb6fab7f
            [ 3, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)28 = ddb7d5bf
            [ 2, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)29 = eedbeadf
            [ 1, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)30 = 00011202
            [ 0, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(%2)31 = fbb6fab7
            [--, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -] ret 0, 00010202
        "#]]
        .assert_eq(
            &ShowLifetimes::new(&lifetimes, &block.instructions, &block.terminator).to_string(),
        );
    }

    #[test]
    fn max_lifetimes() {
        let mut block = max_fn();

        opt::PassManager::optimized().run(&mut block);

        let lifetimes = super::lifetimes(&mut block);

        expect![[r#"
            [10, -, -, -, -, -, -, -, -] %2 = args[0]
            [ 9, 7, -, -, -, -, -, -, -] %4 = x(%2)10
            [ 8, 6, 1, -, -, -, -, -, -] %5 = x(%2)11
            [ 7, 5, 0, 3, -, -, -, -, -] %6 = add %4, %5
            [ 6, 4, -, 2, 2, -, -, -, -] %7 = srl %6, 0000001f
            [ 5, 3, -, 1, 1, -, -, -, -] x(%2)12 = %7
            [ 4, 2, -, 0, 0, 2, -, -, -] %8 = and %6, %7
            [ 3, 1, -, -, -, 1, -, -, -] x(%2)11 = %8
            [ 2, 0, -, -, -, 0, 1, -, -] %9 = add %4, %8
            [ 1, -, -, -, -, -, 0, -, -] x(%2)10 = %9
            [ 0, -, -, -, -, -, -, 1, -] %10 = x(%2)1
            [--, -, -, -, -, -, -, 0, 1] %11 = and %10, fffffffe
            [--, -, -, -, -, -, -, -, 0] ret 0, %11
        "#]]
        .assert_eq(
            &ShowLifetimes::new(&lifetimes, &block.instructions, &block.terminator).to_string(),
        );
    }
}
