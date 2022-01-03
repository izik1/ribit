use crate::Block;

pub trait InplacePass {
    fn run(&self, graph: &mut Block);
}

impl<F> InplacePass for F
where
    F: Fn(&mut Block),
{
    fn run(&self, graph: &mut Block) {
        self(graph);
    }
}

pub enum Pass {
    Inplace(Box<dyn InplacePass>),
    ConstProp,
    DeadInstructionElimination,
    RegisterWritebackShrinking,
}

#[derive(Default)]
pub struct PassManager {
    pub passes: Vec<Pass>,
}

impl PassManager {
    #[must_use]
    pub fn unoptimized() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn with_passes(passes: Vec<Pass>) -> Self {
        Self { passes }
    }

    // Theoretically the best for most situations where optimized output is desired.
    #[must_use]
    pub fn optimized() -> Self {
        Self {
            passes: vec![
                Pass::ConstProp,
                Pass::DeadInstructionElimination,
                Pass::RegisterWritebackShrinking,
            ],
        }
    }
}

impl InplacePass for PassManager {
    fn run(&self, block: &mut Block) {
        for pass in &self.passes {
            match pass {
                Pass::Inplace(p) => p.run(block),
                Pass::ConstProp => super::fold_and_prop_consts(block),
                Pass::DeadInstructionElimination => super::dead_instruction_elimination(block),
                Pass::RegisterWritebackShrinking => super::register_writeback_shrinking(block),
            }
        }
    }
}
