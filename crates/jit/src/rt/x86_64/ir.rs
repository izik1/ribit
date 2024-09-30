pub mod instruction;
mod lower;
mod memory;
mod register;

use std::collections::HashMap;

pub use instruction::Instruction;
use instruction::Terminator;
pub use lower::lower;
pub use memory::Memory;
pub use register::Register;

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash, PartialOrd, Ord)]
pub struct Id(ribit_ssa::Id);

#[derive(Default)]
pub struct IdAllocator(ribit_ssa::IdAllocator);

impl IdAllocator {
    pub fn allocate(&mut self) -> Id {
        Id(self.0.allocate())
    }
}
pub struct Block {
    pub args: HashMap<Id, crate::Register>,
    pub instructions: Vec<Instruction>,
    pub term: Terminator,
    pub id_alloc: IdAllocator,
}
