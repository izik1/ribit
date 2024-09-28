pub mod pass_manager;

pub use const_prop::run_basic as fold_and_prop_consts;
pub use dead_instruction_elimination::run as dead_instruction_elimination;
pub use pass_manager::PassManager;
pub use register_writeback_shrinking::run as register_writeback_shrinking;

mod const_prop;
mod dead_instruction_elimination;
mod register_writeback_shrinking;

#[cfg(test)]
mod tests;
