use crate::ssa::analysis::Lifetimes;
use crate::ssa::{analysis, Arg, Id, Instruction};
use rasen::params::Register;
use std::collections::HashMap;
use std::fmt;

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

fn first_unallocated(allocated: u16) -> Option<u8> {
    match (!allocated).trailing_zeros() {
        it if it < 16 => Some(it as u8),
        _ => None,
    }
}

fn arg_register(arg: Arg) -> Register {
    match arg {
        Arg::Register => Register::Zdi,
        Arg::Memory => Register::Zsi,
    }
}

// todo: fill this out
#[derive(Debug)]
pub struct RegisterSpill;

// todo: this doesn't handle instructions with a lifetime of 1 (dies the same instruction it's created)
fn deallocate_unused(
    allocated: &mut u16,
    lifetimes: &Lifetimes,
    allocations: &mut HashMap<Id, Register>,
    idx: usize,
) {
    for reg in allocations
        .iter()
        .filter_map(|(id, reg)| (lifetimes[id].end == idx).then_some(*reg))
    {
        // ensure that there is indeed a register allocated there.
        assert_eq!(!*allocated & (1 << reg as u16), 0);
        *allocated &= !(1 << reg as u16)
    }
}

fn allocate_args(
    allocated: &mut u16,
    allocations: &mut HashMap<Id, Register>,
    graph: &[Instruction],
) -> Option<usize> {
    for (idx, instr) in graph.iter().enumerate() {
        match instr {
            Instruction::Arg { dest, src } => {
                let reg = arg_register(*src);
                // shouldn't ever happen, but better to check anyway.
                assert_eq!(*allocated & (reg as u16), 0);
                *allocated |= 1 << (reg as u16);

                allocations.insert(*dest, reg);
            }
            _ => return Some(idx),
        }
    }

    None
}

pub fn allocate_registers(graph: &[Instruction]) -> Result<HashMap<Id, Register>, RegisterSpill> {
    let mut allocated = system_v_abi_saved() | (1 << Register::Zsp as u16);
    let lifetimes = analysis::lifetimes(&graph);
    // todo: find an efficient way of pre-checking lifetimes to avoid unneeded work on spills.

    let mut allocations = HashMap::new();

    // avoid any ids where it dies right when it's created.
    // this prevents allocating instructions where a register would be leaked.
    // todo: handle the actual problem rather than this bandaid.
    assert!(!lifetimes.values().any(|it| it.start == it.end));

    // use loop here to avoid having to use `!#[feature(label_break_value)]`

    let start = match allocate_args(&mut allocated, &mut allocations, graph) {
        Some(idx) => idx,
        None => return Ok(allocations),
    };

    for (idx, instr) in graph[start..].iter().enumerate() {
        let idx = idx + start;
        deallocate_unused(&mut allocated, &lifetimes, &mut allocations, idx);

        if let Some(id) = instr.id() {
            // todo: create and return a RegisterSpill
            let reg = register_from_index(first_unallocated(allocated).unwrap());
            allocated |= 1 << (reg as u16);
            allocations.insert(id, reg);
        }
    }

    Ok(allocations)
}

struct FmtRegister(Register);

impl fmt::Display for FmtRegister {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            Register::Zax => write!(f, "zax"),
            Register::Zcx => write!(f, "zcx"),
            Register::Zdx => write!(f, "zdx"),
            Register::Zbx => write!(f, "zbx"),
            Register::Zsp => write!(f, "zsp"),
            Register::Zbp => write!(f, "zbp"),
            Register::Zsi => write!(f, "zsi"),
            Register::Zdi => write!(f, "zdi"),
            Register::R8 => write!(f, "r8"),
            Register::R9 => write!(f, "r9"),
            Register::R10 => write!(f, "r10"),
            Register::R11 => write!(f, "r11"),
            Register::R12 => write!(f, "r12"),
            Register::R13 => write!(f, "r13"),
            Register::R14 => write!(f, "r14"),
            Register::R15 => write!(f, "r15"),
        }
    }
}

pub struct ShowAllocs<'a> {
    allocs: &'a HashMap<Id, Register>,
}

impl fmt::Display for ShowAllocs<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut allocs: Vec<_> = self.allocs.iter().collect();
        allocs.sort_by_key(|(id, _)| *id);

        for (id, reg) in allocs {
            id.fmt(f)?;
            f.write_str(" => ")?;
            writeln!(f, "{}", FmtRegister(*reg))?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::ssa::{analysis, opt};
    use insta::assert_display_snapshot;
    use insta::assert_snapshot;

    #[test]
    fn alloc_max() {
        let mut instrs = crate::ssa::max_fn();

        opt::fold_and_prop_consts(&mut instrs);
        let instrs = opt::dead_instruction_elimination(&instrs);
        let instrs = opt::register_writeback_shrinking(&instrs);

        let allocs = super::allocate_registers(&instrs).unwrap();
        let lifetimes = analysis::lifetimes(&instrs);
        let mut lifetimes = format!("{}", analysis::ShowLifetimes::new(&lifetimes, &instrs));
        for (id, register) in &allocs {
            lifetimes =
                lifetimes.replace(&id.to_string(), &super::FmtRegister(*register).to_string());
        }

        // the main snapshot (long form, allows human readability)
        assert_snapshot!(lifetimes);
        assert_display_snapshot!(super::ShowAllocs { allocs: &allocs });
    }
}
