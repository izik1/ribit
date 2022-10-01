use crate::Instruction;

#[test]
fn check_for_size_regression() {
    // if the size is improved, just reduce this,
    // if it regresses, attempt to fix it before falling back to whatever it regressed to.
    // any commit that changes this should explain *why* in its message.
    // Unfortunately `Instruction` is a *very* hot (commonly used) type,
    // so the size actually ends up mattering.
    assert_eq!(std::mem::size_of::<Instruction>(), 28);
}
