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

    use crate::test::max_fn;

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
}
