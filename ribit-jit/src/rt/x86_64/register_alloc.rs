use std::collections::HashMap;

use rasen::params::Register;
use ribit_ssa as ssa;
use ribit_ssa::analysis::Lifetimes;
use ribit_ssa::{analysis, Arg, Id, Instruction};

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
        _ => panic!("Bit number {} isn't a valid register", bit),
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

    let start = match allocator.allocate_args(&block.instructions) {
        Some(idx) => idx,
        None => return Ok(allocator.finish()),
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

    let (id, lt) = surrounds
        .into_iter()
        .filter(|(_, lt)| !lt.is_empty())
        .max_by_key(|(_, lt)| lt.end - lt.start)
        .expect("Impossible to solve spill");

    // fixme: no unwrap
    let r = block.reference(id).unwrap();

    // todo: try to do more optimal spills
    //  for instance, if it's a register read (of the latest value),
    // we should avoid stack ops and instead just re-read it

    let end_id = block.allocator.allocate();

    let stack_index = analysis::min_stack(lt, &analysis::stack_lifetimes(block));

    let start = Instruction::WriteStack { src: r, dest: stack_index };

    let end = Instruction::ReadStack { src: stack_index, dest: end_id };

    block.instructions.insert(lt.start + 1, start);

    block.instructions.insert(lt.end, end);

    ssa::update_references(block, lt.end, id, end_id);
}

pub fn alloc(block: &mut ribit_ssa::Block) -> AllocMap {
    loop {
        match try_alloc(&block) {
            Ok(allocs) => break allocs,
            Err(spill) => self::spill(block, spill),
        }
    }
}

#[cfg(test)]
mod test {
    use expect_test::expect;
    use ribit_ssa::{analysis, opt};

    use crate::test::{assemble_block, max_fn};

    #[test]
    fn alloc_max() {
        let mut block = max_fn();

        opt::fold_and_prop_consts(&mut block);
        opt::dead_instruction_elimination(&mut block);
        opt::register_writeback_shrinking(&mut block);

        let super::AllocMap { allocs, clobbers } = super::alloc(&mut block);

        let lifetimes = analysis::lifetimes(&mut block);
        let mut lifetimes = format!(
            "{}",
            analysis::ShowLifetimes::new(&lifetimes, &block.instructions, &block.terminator)
        );

        for (id, register) in &allocs {
            lifetimes = lifetimes
                .replace(&id.to_string(), &crate::test::FmtRegister(*register).to_string());
        }

        // the main snapshot (long form, allows human readability)
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
        "#]]
        .assert_eq(&lifetimes);

        expect![[r#"
            %2 => zdi
            %4 => zax
            %5 => zcx
            %6 => zcx
            %7 => zdx
            %8 => zcx
            %9 => zax
            %10 => zax
            %11 => zax
            ---------
            12 => [zax, zcx]
        "#]]
        .assert_eq(&crate::test::ShowAllocs { allocs: &allocs, clobbers: &clobbers }.to_string());
    }

    #[test]
    fn alloc_spills() {
        let mut block = assemble_block(
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
        );

        opt::fold_and_prop_consts(&mut block);
        opt::dead_instruction_elimination(&mut block);
        opt::register_writeback_shrinking(&mut block);

        let super::AllocMap { allocs, clobbers } = super::alloc(&mut block);

        let lifetimes = analysis::lifetimes(&mut block);
        let mut lifetimes = format!(
            "{}",
            analysis::ShowLifetimes::new(&lifetimes, &block.instructions, &block.terminator)
        );

        for (id, register) in &allocs {
            lifetimes = lifetimes
                .replace(&id.to_string(), &crate::test::FmtRegister(*register).to_string());
        }

        // the main snapshot (long form, allows human readability)
        expect![[r#"
            [49, --, --, --, --, --, --, --, -, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] zdi = args[0]
            [48, 18, --, --, --, --, --, --, -, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] zax = x(zdi)1
            [47, 17, 19, --, --, --, --, --, -, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] zcx = x(zdi)2
            [46, 16, 18, 20, --, --, --, --, -, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] zdx = x(zdi)3
            [45, 15, 17, 19, 21, --, --, --, -, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] zsi = x(zdi)4
            [44, 14, 16, 18, 20, 22, --, --, -, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] r8 = x(zdi)5
            [43, 13, 15, 17, 19, 21, 23, --, -, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] r9 = x(zdi)6
            [42, 12, 14, 16, 18, 20, 22, 24, -, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] r10 = x(zdi)7
            [41, 11, 13, 15, 17, 19, 21, 23, 1, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] r11 = x(zdi)8
            [40, 10, 12, 14, 16, 18, 20, 22, 0, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] !s0 = r11
            [39,  9, 11, 13, 15, 17, 19, 21, -, 1, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] zdi0 = x(zdi)9
            [38,  8, 10, 12, 14, 16, 18, 20, -, 0, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] !s1 = zdi0
            [37,  7,  9, 11, 13, 15, 17, 19, -, -, 1, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] zdi2 = x(zdi)10
            [36,  6,  8, 10, 12, 14, 16, 18, -, -, 0, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] !s2 = zdi2
            [35,  5,  7,  9, 11, 13, 15, 17, -, -, -, 1, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] zdi4 = x(zdi)11
            [34,  4,  6,  8, 10, 12, 14, 16, -, -, -, 0, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] !s3 = zdi4
            [33,  3,  5,  7,  9, 11, 13, 15, -, -, -, -, 1, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] r11 = x(zdi)12
            [32,  2,  4,  6,  8, 10, 12, 14, -, -, -, -, 0, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] !s4 = r11
            [31,  1,  3,  5,  7,  9, 11, 13, -, -, -, -, -, 30, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] zdi8 = x(zdi)13
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
            [18, --, --, --, --, --, --,  0, -, -, -, -, -, 17, -, -, -, -, -, -, 2, -, -, -, -, -, -, -, -, -, -, -] zax = add r10, 00000002
            [17, --, --, --, --, --, --, --, -, -, -, -, -, 16, -, -, -, -, -, -, 1, 2, -, -, -, -, -, -, -, -, -, -] zax3 = !s0
            [16, --, --, --, --, --, --, --, -, -, -, -, -, 15, -, -, -, -, -, -, 0, 1, -, -, -, -, -, -, -, -, -, -] x(zdi)7 = zax
            [15, --, --, --, --, --, --, --, -, -, -, -, -, 14, -, -, -, -, -, -, -, 0, 2, -, -, -, -, -, -, -, -, -] zax = add zax3, 00000002
            [14, --, --, --, --, --, --, --, -, -, -, -, -, 13, -, -, -, -, -, -, -, -, 1, 2, -, -, -, -, -, -, -, -] zcx = !s1
            [13, --, --, --, --, --, --, --, -, -, -, -, -, 12, -, -, -, -, -, -, -, -, 0, 1, -, -, -, -, -, -, -, -] x(zdi)8 = zax
            [12, --, --, --, --, --, --, --, -, -, -, -, -, 11, -, -, -, -, -, -, -, -, -, 0, 2, -, -, -, -, -, -, -] zax = add zcx, 00000002
            [11, --, --, --, --, --, --, --, -, -, -, -, -, 10, -, -, -, -, -, -, -, -, -, -, 1, 2, -, -, -, -, -, -] zcx = !s2
            [10, --, --, --, --, --, --, --, -, -, -, -, -,  9, -, -, -, -, -, -, -, -, -, -, 0, 1, -, -, -, -, -, -] x(zdi)9 = zax
            [ 9, --, --, --, --, --, --, --, -, -, -, -, -,  8, -, -, -, -, -, -, -, -, -, -, -, 0, 2, -, -, -, -, -] zax = add zcx, 00000002
            [ 8, --, --, --, --, --, --, --, -, -, -, -, -,  7, -, -, -, -, -, -, -, -, -, -, -, -, 1, 2, -, -, -, -] zax6 = !s3
            [ 7, --, --, --, --, --, --, --, -, -, -, -, -,  6, -, -, -, -, -, -, -, -, -, -, -, -, 0, 1, -, -, -, -] x(zdi)10 = zax
            [ 6, --, --, --, --, --, --, --, -, -, -, -, -,  5, -, -, -, -, -, -, -, -, -, -, -, -, -, 0, 2, -, -, -] zax = add zax6, 00000002
            [ 5, --, --, --, --, --, --, --, -, -, -, -, -,  4, -, -, -, -, -, -, -, -, -, -, -, -, -, -, 1, 2, -, -] zcx = !s4
            [ 4, --, --, --, --, --, --, --, -, -, -, -, -,  3, -, -, -, -, -, -, -, -, -, -, -, -, -, -, 0, 1, -, -] x(zdi)11 = zax
            [ 3, --, --, --, --, --, --, --, -, -, -, -, -,  2, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, 0, 1, -] zax = add zcx, 00000002
            [ 2, --, --, --, --, --, --, --, -, -, -, -, -,  1, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, 0, -] x(zdi)12 = zax
            [ 1, --, --, --, --, --, --, --, -, -, -, -, -,  0, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, 1] zax = add zdi8, 00000002
            [ 0, --, --, --, --, --, --, --, -, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, 0] x(zdi)13 = zax
            [--, --, --, --, --, --, --, --, -, -, -, -, -, --, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -, -] ret 1, 0000046c
        "#]]
        .assert_eq(&lifetimes);

        expect![[r#"
            %2 => zdi
            %4 => zax
            %6 => zcx
            %8 => zdx
            %10 => zsi
            %12 => r8
            %14 => r9
            %16 => r10
            %18 => r11
            %20 => r11
            %22 => r11
            %24 => r11
            %26 => r11
            %28 => r11
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
            %43 => zcx
            %44 => zcx
            %45 => zcx
            %46 => zcx
            %47 => zcx
        "#]]
        .assert_eq(&crate::test::ShowAllocs { allocs: &allocs, clobbers: &clobbers }.to_string());
    }

    #[test]
    fn alloc_reload() {
        let mut block = assemble_block(
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
        );

        opt::fold_and_prop_consts(&mut block);
        opt::dead_instruction_elimination(&mut block);
        opt::register_writeback_shrinking(&mut block);

        let super::AllocMap { allocs, clobbers } = super::alloc(&mut block);

        let lifetimes = analysis::lifetimes(&mut block);
        let mut lifetimes = format!(
            "{}",
            analysis::ShowLifetimes::new(&lifetimes, &block.instructions, &block.terminator)
        );

        for (id, register) in &allocs {
            lifetimes = lifetimes
                .replace(&id.to_string(), &crate::test::FmtRegister(*register).to_string());
        }

        // the main snapshot (long form, allows human readability)
        expect![[r#"
            [40, -, -, -, -, -, -, -, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -, -] zdi = args[0]
            [39, 4, -, -, -, -, -, -, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -, -] zax = x(zdi)2
            [38, 3, 2, -, -, -, -, -, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -, -] zcx = x(zdi)1
            [37, 2, 1, 3, -, -, -, -, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -, -] zdx = add zax, zcx
            [36, 1, 0, 2, -, -, -, -, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -, -] !s1 = zcx
            [35, 0, -, 1, -, -, -, -, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -, -] !s0 = zax
            [34, -, -, 0, -, -, -, -, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -, -] x(zdi)3 = zdx
            [33, -, -, -, 3, -, -, -, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -, -] zax = x(zdi)5
            [32, -, -, -, 2, 3, -, -, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -, -] zcx = x(zdi)4
            [31, -, -, -, 1, 2, 3, -, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -, -] zdx = add zax, zcx
            [30, -, -, -, 0, 1, 2, -, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -, -] !s3 = zax
            [29, -, -, -, -, 0, 1, -, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -, -] !s2 = zcx
            [28, -, -, -, -, -, 0, -, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -, -] x(zdi)6 = zdx
            [27, -, -, -, -, -, -, 3, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -, -] zax = x(zdi)8
            [26, -, -, -, -, -, -, 2, 21, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -, -] zcx = x(zdi)7
            [25, -, -, -, -, -, -, 1, 20, 2, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -, -] zdx = add zax, zcx
            [24, -, -, -, -, -, -, 0, 19, 1, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -, -] !s4 = zax
            [23, -, -, -, -, -, -, -, 18, 0, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -, -] x(zdi)9 = zdx
            [22, -, -, -, -, -, -, -, 17, -, 19, --, -, --, --, -, -, -, -, -, -, -, -, -, -, -] zax = x(zdi)11
            [21, -, -, -, -, -, -, -, 16, -, 18, 18, -, --, --, -, -, -, -, -, -, -, -, -, -, -] zdx = x(zdi)10
            [20, -, -, -, -, -, -, -, 15, -, 17, 17, 1, --, --, -, -, -, -, -, -, -, -, -, -, -] zsi = add zax, zdx
            [19, -, -, -, -, -, -, -, 14, -, 16, 16, 0, --, --, -, -, -, -, -, -, -, -, -, -, -] x(zdi)12 = zsi
            [18, -, -, -, -, -, -, -, 13, -, 15, 15, -, 17, --, -, -, -, -, -, -, -, -, -, -, -] zsi = x(zdi)14
            [17, -, -, -, -, -, -, -, 12, -, 14, 14, -, 16, 16, -, -, -, -, -, -, -, -, -, -, -] r8 = x(zdi)13
            [16, -, -, -, -, -, -, -, 11, -, 13, 13, -, 15, 15, 3, -, -, -, -, -, -, -, -, -, -] r9 = add zsi, r8
            [15, -, -, -, -, -, -, -, 10, -, 12, 12, -, 14, 14, 2, 3, -, -, -, -, -, -, -, -, -] r10 = !s0
            [14, -, -, -, -, -, -, -,  9, -, 11, 11, -, 13, 13, 1, 2, 2, -, -, -, -, -, -, -, -] r11 = !s1
            [13, -, -, -, -, -, -, -,  8, -, 10, 10, -, 12, 12, 0, 1, 1, -, -, -, -, -, -, -, -] x(zdi)15 = r9
            [12, -, -, -, -, -, -, -,  7, -,  9,  9, -, 11, 11, -, 0, 0, 3, -, -, -, -, -, -, -] r9 = add r10, r11
            [11, -, -, -, -, -, -, -,  6, -,  8,  8, -, 10, 10, -, -, -, 2, 3, -, -, -, -, -, -] r10 = !s2
            [10, -, -, -, -, -, -, -,  5, -,  7,  7, -,  9,  9, -, -, -, 1, 2, 2, -, -, -, -, -] r11 = !s3
            [ 9, -, -, -, -, -, -, -,  4, -,  6,  6, -,  8,  8, -, -, -, 0, 1, 1, -, -, -, -, -] x(zdi)16 = r9
            [ 8, -, -, -, -, -, -, -,  3, -,  5,  5, -,  7,  7, -, -, -, -, 0, 0, 2, -, -, -, -] r9 = add r11, r10
            [ 7, -, -, -, -, -, -, -,  2, -,  4,  4, -,  6,  6, -, -, -, -, -, -, 1, 2, -, -, -] r10 = !s4
            [ 6, -, -, -, -, -, -, -,  1, -,  3,  3, -,  5,  5, -, -, -, -, -, -, 0, 1, -, -, -] x(zdi)17 = r9
            [ 5, -, -, -, -, -, -, -,  0, -,  2,  2, -,  4,  4, -, -, -, -, -, -, -, 0, 1, -, -] zcx = add r10, zcx
            [ 4, -, -, -, -, -, -, -, --, -,  1,  1, -,  3,  3, -, -, -, -, -, -, -, -, 0, -, -] x(zdi)18 = zcx
            [ 3, -, -, -, -, -, -, -, --, -,  0,  0, -,  2,  2, -, -, -, -, -, -, -, -, -, 1, -] zax = add zax, zdx
            [ 2, -, -, -, -, -, -, -, --, -, --, --, -,  1,  1, -, -, -, -, -, -, -, -, -, 0, -] x(zdi)19 = zax
            [ 1, -, -, -, -, -, -, -, --, -, --, --, -,  0,  0, -, -, -, -, -, -, -, -, -, -, 1] zax = add zsi, r8
            [ 0, -, -, -, -, -, -, -, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -, 0] x(zdi)20 = zax
            [--, -, -, -, -, -, -, -, --, -, --, --, -, --, --, -, -, -, -, -, -, -, -, -, -, -] ret 1, 0000042c
        "#]]
        .assert_eq(&lifetimes);

        expect![[r#"
            %2 => zdi
            %4 => zax
            %5 => zcx
            %6 => zdx
            %7 => zax
            %8 => zcx
            %9 => zdx
            %10 => zax
            %11 => zcx
            %12 => zdx
            %13 => zax
            %14 => zdx
            %15 => zsi
            %16 => zsi
            %17 => r8
            %18 => r9
            %19 => r9
            %20 => r9
            %21 => zcx
            %22 => zax
            %23 => zax
            %24 => r10
            %25 => r11
            %26 => r10
            %27 => r11
            %28 => r10
        "#]]
        .assert_eq(&crate::test::ShowAllocs { allocs: &allocs, clobbers: &clobbers }.to_string());
    }
}
