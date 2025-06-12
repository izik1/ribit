use std::collections::HashMap;

use rasen::params::Register;
use ribit_ssa as ssa;
use ribit_ssa::analysis::Lifetimes;
use ribit_ssa::reference::Reference;
use ribit_ssa::{Arg, Id, Instruction, analysis};

use crate::rt::x86_64::legalise;

const fn system_v_abi_saved() -> u16 {
    (1 << Register::Zbx as u16)
        | (1 << Register::Zbp as u16)
        | (1 << Register::R12 as u16)
        | (1 << Register::R13 as u16)
        | (1 << Register::R14 as u16)
        | (1 << Register::R15 as u16)
}

fn register_from_index(bit: u8) -> Register {
    // we don't have saved registers implemented.
    assert_ne!(bit, Register::Zbx as u8);
    assert_ne!(bit, Register::R12 as u8);
    assert_ne!(bit, Register::R13 as u8);
    assert_ne!(bit, Register::R14 as u8);
    assert_ne!(bit, Register::R15 as u8);
    assert_ne!(bit, Register::Zsp as u8);

    // sp shouldn't be used as a register here.
    assert_ne!(bit, Register::Zbp as u8);
    match bit {
        0 => Register::Zax,
        1 => Register::Zcx,
        2 => Register::Zdx,
        3 => Register::Zbx,
        4 => Register::Zsp,
        5 => Register::Zbp,
        6 => Register::Zsi,
        7 => Register::Zdi,
        8 => Register::R8,
        9 => Register::R9,
        10 => Register::R10,
        11 => Register::R11,
        12 => Register::R12,
        13 => Register::R13,
        14 => Register::R14,
        15 => Register::R15,
        _ => panic!("Bit number {bit} isn't a valid register"),
    }
}

fn arg_register(arg: Arg) -> Register {
    match arg {
        Arg::Register => Register::Zdi,
        Arg::Memory => Register::Zsi,
    }
}

#[derive(Debug)]
struct RegisterAllocator {
    currently_allocated: u16,
    currently_clobbered: u16,
    current_ids: Vec<Id>,
    allocations: HashMap<Id, Register>,
    clobbers: HashMap<usize, Vec<Register>>,
}

impl RegisterAllocator {
    fn new() -> Self {
        Self {
            currently_allocated: system_v_abi_saved() | (1 << Register::Zsp as u16),
            currently_clobbered: 0,
            allocations: HashMap::new(),
            clobbers: HashMap::new(),
            current_ids: Vec::new(),
        }
    }

    // we only do a cast if the number is < 16, so, it can't truncate.
    #[allow(clippy::cast_possible_truncation)]
    fn first_unallocated(&self) -> Option<Register> {
        match (!(self.currently_allocated | self.currently_clobbered)).trailing_zeros() {
            it if it < 16 => Some(register_from_index(it as u8)),
            // this should only ever be _equal_ to 16, due to the contract of `trailing zeros`
            _ => None,
        }
    }

    fn allocate(&mut self, id: Id, reg: Register) -> Result<(), RegisterSpill> {
        self.currently_allocated |= 1 << (reg as u16);
        self.allocations.insert(id, reg);
        self.current_ids.push(id);
        Ok(())
    }

    fn allocate_clobber(&mut self, idx: usize, reg: Register) {
        self.currently_clobbered |= 1 << (reg as u16);
        self.clobbers.entry(idx).or_insert_with(|| Vec::with_capacity(1)).push(reg);
    }

    fn deallocate_unused(&mut self, lifetimes: &Lifetimes, idx: usize) {
        let mut removed = Vec::new();
        for (offset, id) in self.current_ids.iter().copied().enumerate() {
            if lifetimes[&id].end <= idx {
                let reg = self.allocations[&id];

                // ensure that there is indeed a register allocated there.
                assert_eq!(!self.currently_allocated & (1 << reg as u16), 0);
                self.currently_allocated &= !(1 << reg as u16);
                removed.push(offset);
            }
        }
        for idx in removed.into_iter().rev() {
            self.current_ids.swap_remove(idx);
        }
    }

    fn clear_clobbers(&mut self) {
        self.currently_clobbered = 0;
    }

    fn allocate_args(&mut self, graph: &[Instruction]) -> Option<usize> {
        for (idx, instr) in graph.iter().enumerate() {
            match instr {
                Instruction::Arg { dest, src } => {
                    let reg = arg_register(*src);
                    // shouldn't ever happen, but better to check anyway.
                    assert_eq!(self.currently_allocated & (1 << reg as u16), 0);
                    self.currently_allocated |= 1 << (reg as u16);

                    self.allocations.insert(*dest, reg);
                }
                _ => return Some(idx),
            }
        }

        None
    }

    fn finish(self) -> AllocMap {
        AllocMap { allocs: self.allocations, clobbers: self.clobbers }
    }
}

impl Default for RegisterAllocator {
    fn default() -> Self {
        Self::new()
    }
}

// todo: fill this out
#[derive(Debug, Copy, Clone)]
pub struct RegisterSpill(pub usize);

pub struct AllocMap {
    pub allocs: HashMap<Id, Register>,
    pub clobbers: HashMap<usize, Vec<Register>>,
}

pub fn try_alloc(block: &ribit_ssa::Block) -> Result<AllocMap, RegisterSpill> {
    let mut allocator = RegisterAllocator::new();

    let Some(start) = allocator.allocate_args(&block.instructions) else {
        return Ok(allocator.finish());
    };

    // todo: find an efficient way of pre-checking lifetimes to avoid unneeded work on spills.
    let lifetimes = analysis::lifetimes(block);

    for (idx, instr) in block.instructions[start..].iter().enumerate() {
        let idx = idx + start;

        allocator.deallocate_unused(&lifetimes, idx);
        allocator.clear_clobbers();

        if let Some(id) = instr.id() {
            match allocator.first_unallocated() {
                Some(reg) => allocator.allocate(id, reg)?,
                None => return Err(RegisterSpill(idx)),
            }
        }

        // make sure we allocate clobber registers
        let clobbers = legalise::count_clobbers_for(instr, &allocator.allocations);

        for _ in 0..clobbers {
            match allocator.first_unallocated() {
                Some(reg) => allocator.allocate_clobber(idx, reg),
                None => return Err(RegisterSpill(idx)),
            }
        }
    }

    {
        let idx = block.instructions.len();

        allocator.deallocate_unused(&lifetimes, idx);
        allocator.clear_clobbers();

        let clobbers =
            legalise::count_clobbers_for_terminal(&block.terminator, &allocator.allocations);

        for _ in 0..clobbers {
            match allocator.first_unallocated() {
                Some(reg) => allocator.allocate_clobber(idx, reg),
                None => return Err(RegisterSpill(idx)),
            }
        }
    }

    Ok(allocator.finish())
}

pub fn spill(block: &mut ribit_ssa::Block, spill: RegisterSpill) {
    let spill = spill.0;
    let surrounds = analysis::surrounding_usages(block, spill);

    // find the ID with the largest gap in uses before and after the spill locations.
    let (id, lt) = surrounds
        .into_iter()
        .max_by_key(|(id, lt)| (lt.len(), *id))
        .expect("Impossible to solve spill");

    // fixme: no unwrap
    let def = block.find(id).unwrap();
    let r = Reference { id, ty: def.ty() };

    // if the instruction we're spilling is a register read (specifically *not* a memory read), we can just... read it again.
    let end_id = if let Instruction::ReadReg { dest: _, base, src } = *def {
        let end_id = block.allocator.allocate();

        block.instructions.insert(lt.end, Instruction::ReadReg { dest: end_id, base, src });

        end_id
    } else {
        // todo: try to do more optimal spills

        let end_id = block.allocator.allocate();

        let stack_index = analysis::min_stack(lt, &analysis::stack_lifetimes(block));

        let start = Instruction::WriteStack { src: r, dest: stack_index };

        let end = Instruction::ReadStack { src: stack_index, dest: end_id };

        block.instructions.reserve(2);

        block.instructions.insert(lt.start + 1, start);
        block.instructions.insert(lt.end, end);

        end_id
    };

    ssa::update_references(block, lt.end, id, end_id);
}

pub fn alloc(block: &mut ribit_ssa::Block) -> AllocMap {
    loop {
        match try_alloc(block) {
            Ok(allocs) => break allocs,
            Err(spill) => self::spill(block, spill),
        }
    }
}

#[cfg(test)]
mod test {
    use expect_test::{Expect, expect};
    use ribit_ssa::opt::PassManager;
    use ribit_ssa::opt::pass_manager::{InplacePass, Pass};
    use ribit_ssa::{Block, analysis};

    use crate::test::{assemble_block, max_fn};

    fn expect_optimized_block(
        pm: PassManager,
        mut block: Block,
        main: Expect,
        assignments: Expect,
    ) {
        pm.run(&mut block);

        let super::AllocMap { allocs, clobbers } = super::alloc(&mut block);

        let lifetimes = analysis::lifetimes(&block);
        let mut lifetimes = format!(
            "{}",
            analysis::ShowLifetimes::new(&lifetimes, &block.instructions, &block.terminator)
        );

        // we want to print the allocs in place of instruction IDs, naively you'd assume you can just `str::replace` them...
        // That's *true*, but you have to do it in the right order, because if you replace %1, and then replace %10, you now misprinted %10, as say, `zax0`.
        // As that would be counterproductive for our purposes, just... Sort them and replace them in big-to-small order.
        // this will replace %10 before %1, and everything is fine.
        let mut tmp_allocs: Vec<_> = allocs.iter().collect();
        tmp_allocs.sort_by_key(|it| std::cmp::Reverse(*it.0));

        for (id, register) in tmp_allocs {
            lifetimes = lifetimes
                .replace(&id.to_string(), &crate::test::FmtRegister(*register).to_string());
        }

        // the main snapshot (long form, allows human readability)
        main.assert_eq(&lifetimes);

        let actual = &crate::test::ShowAllocs { allocs: &allocs, clobbers: &clobbers }.to_string();
        assignments.assert_eq(actual);
    }

    #[test]
    fn alloc_max() {
        expect_optimized_block(
            PassManager::optimized(),
            max_fn(),
            expect![[r#"
                [10, -, -, -, -, -, -, -, -] zdi = args[0]
                [ 9, 7, -, -, -, -, -, -, -] zax = x(zdi)10
                [ 8, 6, 1, -, -, -, -, -, -] zcx = x(zdi)11
                [ 7, 5, 0, 3, -, -, -, -, -] zcx = add zax, zcx
                [ 6, 4, -, 2, 2, -, -, -, -] zdx = srl zcx, 0000001f
                [ 5, 3, -, 1, 1, -, -, -, -] x(zdi)12 = zdx
                [ 4, 2, -, 0, 0, 2, -, -, -] zcx = and zcx, zdx
                [ 3, 1, -, -, -, 1, -, -, -] x(zdi)11 = zcx
                [ 2, 0, -, -, -, 0, 1, -, -] zax = add zax, zcx
                [ 1, -, -, -, -, -, 0, -, -] x(zdi)10 = zax
                [ 0, -, -, -, -, -, -, 1, -] zax = x(zdi)1
                [--, -, -, -, -, -, -, 0, 1] zax = and zax, fffffffe
                [--, -, -, -, -, -, -, -, 0] ret 0, zax
            "#]],
            expect![[r#"
                %0 => zdi
                %2 => zax
                %3 => zcx
                %4 => zcx
                %5 => zdx
                %6 => zcx
                %7 => zax
                %8 => zax
                %9 => zax
                ---------
                12 => [zax, zcx]
            "#]],
        );
    }

    #[test]
    fn alloc_spills() {
        expect_optimized_block(
            PassManager::optimized(),
            assemble_block(
                r#"
                    addi x1, x1, 1
                    addi x2, x2, 1
                    addi x3, x3, 1
                    addi x4, x4, 1
                    addi x5, x5, 1
                    addi x6, x6, 1
                    addi x7, x7, 1
                    addi x8, x8, 1
                    addi x9, x9, 1
                    addi x10, x10, 1
                    addi x11, x11, 1
                    addi x12, x12, 1
                    addi x13, x13, 1
                    addi x1, x1, 1
                    addi x2, x2, 1
                    addi x3, x3, 1
                    addi x4, x4, 1
                    addi x5, x5, 1
                    addi x6, x6, 1
                    addi x7, x7, 1
                    addi x8, x8, 1
                    addi x9, x9, 1
                    addi x10, x10, 1
                    addi x11, x11, 1
                    addi x12, x12, 1
                    addi x13, x13, 1
                    ebreak
                "#,
            ),
            expect![[r#"
                [44, --, --, --, --, --, --, --, -, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] zdi = args[0]
                [43, 13, --, --, --, --, --, --, -, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] zax = x(zdi)1
                [42, 12, 14, --, --, --, --, --, -, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] zcx = x(zdi)2
                [41, 11, 13, 15, --, --, --, --, -, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] zdx = x(zdi)3
                [40, 10, 12, 14, 16, --, --, --, -, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] zsi = x(zdi)4
                [39,  9, 11, 13, 15, 17, --, --, -, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] r8 = x(zdi)5
                [38,  8, 10, 12, 14, 16, 18, --, -, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] r9 = x(zdi)6
                [37,  7,  9, 11, 13, 15, 17, 19, -, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] r10 = x(zdi)7
                [36,  6,  8, 10, 12, 14, 16, 18, 0, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] r11 = x(zdi)8
                [35,  5,  7,  9, 11, 13, 15, 17, -, 0, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] r11 = x(zdi)9
                [34,  4,  6,  8, 10, 12, 14, 16, -, -, 0, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] r11 = x(zdi)10
                [33,  3,  5,  7,  9, 11, 13, 15, -, -, -, 0, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] r11 = x(zdi)11
                [32,  2,  4,  6,  8, 10, 12, 14, -, -, -, -, 0, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] r11 = x(zdi)12
                [31,  1,  3,  5,  7,  9, 11, 13, -, -, -, -, -, 30, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] r11 = x(zdi)13
                [30,  0,  2,  4,  6,  8, 10, 12, -, -, -, -, -, 29, 1, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] zax = add zax, 00000002
                [29, --,  1,  3,  5,  7,  9, 11, -, -, -, -, -, 28, 0, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(zdi)1 = zax
                [28, --,  0,  2,  4,  6,  8, 10, -, -, -, -, -, 27, -, 1, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] zax = add zcx, 00000002
                [27, --, --,  1,  3,  5,  7,  9, -, -, -, -, -, 26, -, 0, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(zdi)2 = zax
                [26, --, --,  0,  2,  4,  6,  8, -, -, -, -, -, 25, -, -, 1, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] zax = add zdx, 00000002
                [25, --, --, --,  1,  3,  5,  7, -, -, -, -, -, 24, -, -, 0, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(zdi)3 = zax
                [24, --, --, --,  0,  2,  4,  6, -, -, -, -, -, 23, -, -, -, 1, -, -, -, -, -, -, -, -, -, -, -, -, -, -] zax = add zsi, 00000002
                [23, --, --, --, --,  1,  3,  5, -, -, -, -, -, 22, -, -, -, 0, -, -, -, -, -, -, -, -, -, -, -, -, -, -] x(zdi)4 = zax
                [22, --, --, --, --,  0,  2,  4, -, -, -, -, -, 21, -, -, -, -, 1, -, -, -, -, -, -, -, -, -, -, -, -, -] zax = add r8, 00000002
                [21, --, --, --, --, --,  1,  3, -, -, -, -, -, 20, -, -, -, -, 0, -, -, -, -, -, -, -, -, -, -, -, -, -] x(zdi)5 = zax
                [20, --, --, --, --, --,  0,  2, -, -, -, -, -, 19, -, -, -, -, -, 1, -, -, -, -, -, -, -, -, -, -, -, -] zax = add r9, 00000002
                [19, --, --, --, --, --, --,  1, -, -, -, -, -, 18, -, -, -, -, -, 0, -, -, -, -, -, -, -, -, -, -, -, -] x(zdi)6 = zax
                [18, --, --, --, --, --, --,  0, -, -, -, -, -, 17, -, -, -, -, -, -, 1, -, -, -, -, -, -, -, -, -, -, -] zax = add r10, 00000002
                [17, --, --, --, --, --, --, --, -, -, -, -, -, 16, -, -, -, -, -, -, 0, -, -, -, -, -, -, -, -, -, -, -] x(zdi)7 = zax
                [16, --, --, --, --, --, --, --, -, -, -, -, -, 15, -, -, -, -, -, -, -, 1, -, -, -, -, -, -, -, -, -, -] zax = x(zdi)8
                [15, --, --, --, --, --, --, --, -, -, -, -, -, 14, -, -, -, -, -, -, -, 0, 1, -, -, -, -, -, -, -, -, -] zax = add zax, 00000002
                [14, --, --, --, --, --, --, --, -, -, -, -, -, 13, -, -, -, -, -, -, -, -, 0, -, -, -, -, -, -, -, -, -] x(zdi)8 = zax
                [13, --, --, --, --, --, --, --, -, -, -, -, -, 12, -, -, -, -, -, -, -, -, -, 1, -, -, -, -, -, -, -, -] zax = x(zdi)9
                [12, --, --, --, --, --, --, --, -, -, -, -, -, 11, -, -, -, -, -, -, -, -, -, 0, 1, -, -, -, -, -, -, -] zax = add zax, 00000002
                [11, --, --, --, --, --, --, --, -, -, -, -, -, 10, -, -, -, -, -, -, -, -, -, -, 0, -, -, -, -, -, -, -] x(zdi)9 = zax
                [10, --, --, --, --, --, --, --, -, -, -, -, -,  9, -, -, -, -, -, -, -, -, -, -, -, 1, -, -, -, -, -, -] zax = x(zdi)10
                [ 9, --, --, --, --, --, --, --, -, -, -, -, -,  8, -, -, -, -, -, -, -, -, -, -, -, 0, 1, -, -, -, -, -] zax = add zax, 00000002
                [ 8, --, --, --, --, --, --, --, -, -, -, -, -,  7, -, -, -, -, -, -, -, -, -, -, -, -, 0, -, -, -, -, -] x(zdi)10 = zax
                [ 7, --, --, --, --, --, --, --, -, -, -, -, -,  6, -, -, -, -, -, -, -, -, -, -, -, -, -, 1, -, -, -, -] zax = x(zdi)11
                [ 6, --, --, --, --, --, --, --, -, -, -, -, -,  5, -, -, -, -, -, -, -, -, -, -, -, -, -, 0, 1, -, -, -] zax = add zax, 00000002
                [ 5, --, --, --, --, --, --, --, -, -, -, -, -,  4, -, -, -, -, -, -, -, -, -, -, -, -, -, -, 0, -, -, -] x(zdi)11 = zax
                [ 4, --, --, --, --, --, --, --, -, -, -, -, -,  3, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, 1, -, -] zax = x(zdi)12
                [ 3, --, --, --, --, --, --, --, -, -, -, -, -,  2, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, 0, 1, -] zax = add zax, 00000002
                [ 2, --, --, --, --, --, --, --, -, -, -, -, -,  1, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, 0, -] x(zdi)12 = zax
                [ 1, --, --, --, --, --, --, --, -, -, -, -, -,  0, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, 1] zax = add r11, 00000002
                [ 0, --, --, --, --, --, --, --, -, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, 0] x(zdi)13 = zax
                [--, --, --, --, --, --, --, --, -, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] ret 1, 0000046c
            "#]],
            expect![[r#"
                %0 => zdi
                %2 => zax
                %4 => zcx
                %6 => zdx
                %8 => zsi
                %10 => r8
                %12 => r9
                %14 => r10
                %16 => r11
                %18 => r11
                %20 => r11
                %22 => r11
                %24 => r11
                %26 => r11
                %28 => zax
                %29 => zax
                %30 => zax
                %31 => zax
                %32 => zax
                %33 => zax
                %34 => zax
                %35 => zax
                %36 => zax
                %37 => zax
                %38 => zax
                %39 => zax
                %40 => zax
                %41 => zax
                %42 => zax
                %43 => zax
                %44 => zax
                %45 => zax
            "#]],
        );
    }

    #[test]
    fn alloc_reload() {
        expect_optimized_block(
            PassManager::with_passes(Vec::from([
                Pass::DeadInstructionElimination,
                Pass::RegisterWritebackShrinking,
            ])),
            assemble_block(
                r#"
                add x3, x2, x1
                add x6, x5, x4
                add x9, x8, x7
                add x12, x11, x10
                add x15, x14, x13
                add x16, x2, x1
                add x17, x5, x4
                add x18, x8, x7
                add x19, x11, x10
                add x20, x14, x13
                ebreak
            "#,
            ),
            expect![[r#"
                [34, -, -, -, -, -, -, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zdi = args[0]
                [33, 2, -, -, -, -, -, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zax = x(zdi)2
                [32, 1, 1, -, -, -, -, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zcx = x(zdi)1
                [31, 0, 0, 1, -, -, -, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zax = add zax, zcx
                [30, -, -, 0, -, -, -, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] x(zdi)3 = zax
                [29, -, -, -, 2, -, -, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zax = x(zdi)5
                [28, -, -, -, 1, 1, -, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zcx = x(zdi)4
                [27, -, -, -, 0, 0, 1, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zax = add zax, zcx
                [26, -, -, -, -, -, 0, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] x(zdi)6 = zax
                [25, -, -, -, -, -, -, 20, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zax = x(zdi)8
                [24, -, -, -, -, -, -, 19, 19, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zcx = x(zdi)7
                [23, -, -, -, -, -, -, 18, 18, 1, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zdx = add zax, zcx
                [22, -, -, -, -, -, -, 17, 17, 0, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] x(zdi)9 = zdx
                [21, -, -, -, -, -, -, 16, 16, -, 18, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zdx = x(zdi)11
                [20, -, -, -, -, -, -, 15, 15, -, 17, 17, -, --, --, -, -, -, -, -, -, -, -, -, -] zsi = x(zdi)10
                [19, -, -, -, -, -, -, 14, 14, -, 16, 16, 1, --, --, -, -, -, -, -, -, -, -, -, -] r8 = add zdx, zsi
                [18, -, -, -, -, -, -, 13, 13, -, 15, 15, 0, --, --, -, -, -, -, -, -, -, -, -, -] x(zdi)12 = r8
                [17, -, -, -, -, -, -, 12, 12, -, 14, 14, -, 16, --, -, -, -, -, -, -, -, -, -, -] r8 = x(zdi)14
                [16, -, -, -, -, -, -, 11, 11, -, 13, 13, -, 15, 15, -, -, -, -, -, -, -, -, -, -] r9 = x(zdi)13
                [15, -, -, -, -, -, -, 10, 10, -, 12, 12, -, 14, 14, 1, -, -, -, -, -, -, -, -, -] r10 = add r8, r9
                [14, -, -, -, -, -, -,  9,  9, -, 11, 11, -, 13, 13, 0, -, -, -, -, -, -, -, -, -] x(zdi)15 = r10
                [13, -, -, -, -, -, -,  8,  8, -, 10, 10, -, 12, 12, -, 2, -, -, -, -, -, -, -, -] r10 = x(zdi)1
                [12, -, -, -, -, -, -,  7,  7, -,  9,  9, -, 11, 11, -, 1, 1, -, -, -, -, -, -, -] r11 = x(zdi)2
                [11, -, -, -, -, -, -,  6,  6, -,  8,  8, -, 10, 10, -, 0, 0, 1, -, -, -, -, -, -] r10 = add r11, r10
                [10, -, -, -, -, -, -,  5,  5, -,  7,  7, -,  9,  9, -, -, -, 0, -, -, -, -, -, -] x(zdi)16 = r10
                [ 9, -, -, -, -, -, -,  4,  4, -,  6,  6, -,  8,  8, -, -, -, -, 2, -, -, -, -, -] r10 = x(zdi)4
                [ 8, -, -, -, -, -, -,  3,  3, -,  5,  5, -,  7,  7, -, -, -, -, 1, 1, -, -, -, -] r11 = x(zdi)5
                [ 7, -, -, -, -, -, -,  2,  2, -,  4,  4, -,  6,  6, -, -, -, -, 0, 0, 1, -, -, -] r10 = add r11, r10
                [ 6, -, -, -, -, -, -,  1,  1, -,  3,  3, -,  5,  5, -, -, -, -, -, -, 0, -, -, -] x(zdi)17 = r10
                [ 5, -, -, -, -, -, -,  0,  0, -,  2,  2, -,  4,  4, -, -, -, -, -, -, -, 1, -, -] zax = add zax, zcx
                [ 4, -, -, -, -, -, -, --, --, -,  1,  1, -,  3,  3, -, -, -, -, -, -, -, 0, -, -] x(zdi)18 = zax
                [ 3, -, -, -, -, -, -, --, --, -,  0,  0, -,  2,  2, -, -, -, -, -, -, -, -, 1, -] zax = add zdx, zsi
                [ 2, -, -, -, -, -, -, --, --, -, --, --, -,  1,  1, -, -, -, -, -, -, -, -, 0, -] x(zdi)19 = zax
                [ 1, -, -, -, -, -, -, --, --, -, --, --, -,  0,  0, -, -, -, -, -, -, -, -, -, 1] zax = add r8, r9
                [ 0, -, -, -, -, -, -, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, 0] x(zdi)20 = zax
                [--, -, -, -, -, -, -, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] ret 1, 0000042c
            "#]],
            expect![[r#"
                %0 => zdi
                %2 => zax
                %3 => zcx
                %4 => zax
                %5 => zax
                %6 => zcx
                %7 => zax
                %8 => zax
                %9 => zcx
                %10 => zdx
                %11 => zdx
                %12 => zsi
                %13 => r8
                %14 => r8
                %15 => r9
                %16 => r10
                %17 => r10
                %18 => r10
                %19 => zax
                %20 => zax
                %21 => zax
                %22 => r10
                %23 => r11
                %24 => r10
                %25 => r11
            "#]],
        );
    }

    #[test]
    fn alloc_reload_lvn() {
        expect_optimized_block(
            PassManager::with_passes(Vec::from([
                Pass::LocalValueNumbering,
                Pass::DeadInstructionElimination,
                Pass::RegisterWritebackShrinking,
            ])),
            assemble_block(
                r#"
                add x3, x2, x1
                add x6, x5, x4
                add x9, x8, x7
                add x12, x11, x10
                add x15, x14, x13
                sll x16, x2, x1
                sll x17, x5, x4
                sll x18, x8, x7
                sll x19, x11, x10
                sll x20, x14, x13
                ebreak
            "#,
            ),
            expect![[r#"
                [34, -, -, -, -, -, -, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zdi = args[0]
                [33, 2, -, -, -, -, -, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zax = x(zdi)2
                [32, 1, 1, -, -, -, -, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zcx = x(zdi)1
                [31, 0, 0, 1, -, -, -, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zax = add zax, zcx
                [30, -, -, 0, -, -, -, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] x(zdi)3 = zax
                [29, -, -, -, 2, -, -, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zax = x(zdi)5
                [28, -, -, -, 1, 1, -, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zcx = x(zdi)4
                [27, -, -, -, 0, 0, 1, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zax = add zax, zcx
                [26, -, -, -, -, -, 0, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] x(zdi)6 = zax
                [25, -, -, -, -, -, -, 20, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zax = x(zdi)8
                [24, -, -, -, -, -, -, 19, 19, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zcx = x(zdi)7
                [23, -, -, -, -, -, -, 18, 18, 1, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zdx = add zax, zcx
                [22, -, -, -, -, -, -, 17, 17, 0, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] x(zdi)9 = zdx
                [21, -, -, -, -, -, -, 16, 16, -, 18, --, -, --, --, -, -, -, -, -, -, -, -, -, -] zdx = x(zdi)11
                [20, -, -, -, -, -, -, 15, 15, -, 17, 17, -, --, --, -, -, -, -, -, -, -, -, -, -] zsi = x(zdi)10
                [19, -, -, -, -, -, -, 14, 14, -, 16, 16, 1, --, --, -, -, -, -, -, -, -, -, -, -] r8 = add zdx, zsi
                [18, -, -, -, -, -, -, 13, 13, -, 15, 15, 0, --, --, -, -, -, -, -, -, -, -, -, -] x(zdi)12 = r8
                [17, -, -, -, -, -, -, 12, 12, -, 14, 14, -, 16, --, -, -, -, -, -, -, -, -, -, -] r8 = x(zdi)14
                [16, -, -, -, -, -, -, 11, 11, -, 13, 13, -, 15, 15, -, -, -, -, -, -, -, -, -, -] r9 = x(zdi)13
                [15, -, -, -, -, -, -, 10, 10, -, 12, 12, -, 14, 14, 1, -, -, -, -, -, -, -, -, -] r10 = add r8, r9
                [14, -, -, -, -, -, -,  9,  9, -, 11, 11, -, 13, 13, 0, -, -, -, -, -, -, -, -, -] x(zdi)15 = r10
                [13, -, -, -, -, -, -,  8,  8, -, 10, 10, -, 12, 12, -, 2, -, -, -, -, -, -, -, -] r10 = x(zdi)1
                [12, -, -, -, -, -, -,  7,  7, -,  9,  9, -, 11, 11, -, 1, 1, -, -, -, -, -, -, -] r11 = x(zdi)2
                [11, -, -, -, -, -, -,  6,  6, -,  8,  8, -, 10, 10, -, 0, 0, 1, -, -, -, -, -, -] r10 = sll r11, r10
                [10, -, -, -, -, -, -,  5,  5, -,  7,  7, -,  9,  9, -, -, -, 0, -, -, -, -, -, -] x(zdi)16 = r10
                [ 9, -, -, -, -, -, -,  4,  4, -,  6,  6, -,  8,  8, -, -, -, -, 2, -, -, -, -, -] r10 = x(zdi)4
                [ 8, -, -, -, -, -, -,  3,  3, -,  5,  5, -,  7,  7, -, -, -, -, 1, 1, -, -, -, -] r11 = x(zdi)5
                [ 7, -, -, -, -, -, -,  2,  2, -,  4,  4, -,  6,  6, -, -, -, -, 0, 0, 1, -, -, -] r10 = sll r11, r10
                [ 6, -, -, -, -, -, -,  1,  1, -,  3,  3, -,  5,  5, -, -, -, -, -, -, 0, -, -, -] x(zdi)17 = r10
                [ 5, -, -, -, -, -, -,  0,  0, -,  2,  2, -,  4,  4, -, -, -, -, -, -, -, 1, -, -] zax = sll zax, zcx
                [ 4, -, -, -, -, -, -, --, --, -,  1,  1, -,  3,  3, -, -, -, -, -, -, -, 0, -, -] x(zdi)18 = zax
                [ 3, -, -, -, -, -, -, --, --, -,  0,  0, -,  2,  2, -, -, -, -, -, -, -, -, 1, -] zax = sll zdx, zsi
                [ 2, -, -, -, -, -, -, --, --, -, --, --, -,  1,  1, -, -, -, -, -, -, -, -, 0, -] x(zdi)19 = zax
                [ 1, -, -, -, -, -, -, --, --, -, --, --, -,  0,  0, -, -, -, -, -, -, -, -, -, 1] zax = sll r8, r9
                [ 0, -, -, -, -, -, -, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, 0] x(zdi)20 = zax
                [--, -, -, -, -, -, -, --, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -] ret 1, 0000042c
            "#]],
            expect![[r#"
                %0 => zdi
                %2 => zax
                %3 => zcx
                %4 => zax
                %5 => zax
                %6 => zcx
                %7 => zax
                %8 => zax
                %9 => zcx
                %10 => zdx
                %11 => zdx
                %12 => zsi
                %13 => r8
                %14 => r8
                %15 => r9
                %16 => r10
                %17 => r10
                %18 => r10
                %19 => zax
                %20 => zax
                %21 => zax
                %22 => r10
                %23 => r11
                %24 => r10
                %25 => r11
            "#]],
        );
    }
}
