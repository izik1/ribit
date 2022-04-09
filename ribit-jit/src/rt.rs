use ribit_core::{instruction, ReturnCode};
use ribit_ssa::opt;
use ribit_ssa::opt::pass_manager::InplacePass;

mod interpreter;

pub use interpreter::Interpreter;

#[cfg(any(target_arch = "x86_64"))]
pub mod x86_64;

mod common;

pub struct Runtime<Rt: Target + Default> {
    inner: Rt,
    opt_pass_manager: opt::PassManager,
}

pub trait Target {
    type Block;

    fn generate_block(&mut self, block: ribit_ssa::Block, start_pc: u32, end_pc: u32);

    fn lookup_block(&self, start_address: u32) -> Option<&Self::Block>;

    fn execute_block(
        &mut self,
        pc: u32,
        regs: &mut [u32; crate::XLEN],
        memory: &mut [u8],
    ) -> (u32, ReturnCode);
}

impl<Rt: Target + Default> Runtime<Rt> {
    pub fn execute_basic_block(
        &mut self,
        pc: &mut u32,
        regs: &mut [u32; crate::XLEN],
        memory: &mut [u8],
    ) {
        // assert memory size for now
        assert_eq!(memory.len(), crate::MEMORY_SIZE as usize);

        let (address, return_code) = self.inner.execute_block(*pc, regs, memory);

        *pc = address;

        match return_code {
            ReturnCode::Normal => {}
            ReturnCode::EBreak => todo!("EBREAK"),
            ReturnCode::ECall => crate::sbi::call(regs),
        }
    }

    #[must_use]
    pub fn new() -> Self {
        Self { inner: Rt::default(), opt_pass_manager: opt::PassManager::optimized() }
    }

    #[must_use]
    pub fn lookup_block(&self, start_address: u32) -> bool {
        self.inner.lookup_block(start_address).is_some()
    }

    pub fn generate_basic_block(
        &mut self,
        block_instrs: Vec<instruction::Info>,
        branch: instruction::Info,
        start_pc: u32,
        end_pc: u32,
    ) {
        let mut lower_context = ribit_ssa::lower::Context::new(start_pc, crate::MEMORY_SIZE);

        for instr in block_instrs {
            ribit_ssa::lower::non_terminal(&mut lower_context, instr.instruction, instr.len);
        }

        let mut block = ribit_ssa::lower::terminal(lower_context, branch.instruction, branch.len);

        self.opt_pass_manager.run(&mut block);

        let _ = self.inner.generate_block(block, start_pc, end_pc);
    }
}

impl<Rt: Target + Default> Default for Runtime<Rt> {
    fn default() -> Self {
        Self::new()
    }
}
