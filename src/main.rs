fn main() {
    use rv32i_mc_interpreter::instruction::Instruction;
    println!("{}", std::mem::size_of::<Instruction>());
}
