use crate::Instruction;

pub trait InplacePass {
    fn run(&self, graph: &mut [Instruction]);
}

pub trait ReplacePass {
    fn run(&self, graph: &[Instruction]) -> Vec<Instruction>;
}

impl<F> InplacePass for F
where
    F: Fn(&mut [Instruction]),
{
    fn run(&self, graph: &mut [Instruction]) {
        self(graph)
    }
}

impl<F> ReplacePass for F
where
    F: Fn(&[Instruction]) -> Vec<Instruction>,
{
    fn run(&self, graph: &[Instruction]) -> Vec<Instruction> {
        self(graph)
    }
}

pub enum Pass {
    Inplace(Box<dyn InplacePass>),
    Replace(Box<dyn ReplacePass>),
    ConstProp,
    DeadInstructionElimination,
    RegisterWritebackShrinking,
}

pub struct PassManager {
    pub passes: Vec<Pass>,
}

impl PassManager {
    pub fn new() -> Self {
        Self { passes: Vec::new() }
    }

    // Theoretically the best for most situations where optimized output is desired.
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

impl ReplacePass for PassManager {
    fn run(&self, graph: &[Instruction]) -> Vec<Instruction> {
        let mut graph = graph.to_owned();

        for pass in &self.passes {
            match pass {
                Pass::Inplace(p) => p.run(&mut graph),
                Pass::Replace(p) => graph = p.run(&graph),
                Pass::ConstProp => super::fold_and_prop_consts(&mut graph),
                Pass::DeadInstructionElimination => {
                    graph = super::dead_instruction_elimination(&graph)
                }
                Pass::RegisterWritebackShrinking => {
                    graph = super::register_writeback_shrinking(&graph)
                }
            }
        }

        graph
    }
}
