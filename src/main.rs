use rv32i_mc_interpreter::{CompressedDecodeError, DecodeError};
use std::io::Read;

fn main() {
    env_logger::init();
    let mut buf = vec![];
    std::fs::File::open("/home/zg/git/rv32imc-test-programs/bf.bin")
        .unwrap()
        .read_to_end(&mut buf)
        .unwrap();
    let mut cpu = rv32i_mc_interpreter::Cpu::new(&buf);
    loop {
        if let Err(e) = cpu.run() {
            match e {
                DecodeError::Other => panic!("Non specfic decode error"),
                DecodeError::InvalidInstruction(instruction) => {
                    panic!("Tried to parse invalid instruction: `{:08x}` @ pc {:08x}", instruction, cpu.pc())
                }
                DecodeError::UnimplementedExtension(ext, instruction) => panic!(
                    "Tried to parse instruction with unimplemented extension `{}`: `{:84x}`",
                    ext, instruction
                ),
                DecodeError::Compressed(CompressedDecodeError::InvalidInstruction(instruction)) => {
                    panic!(
                        "Tried to parse invalid instruction: `{:04x}`.C @ pc {:08x}",
                        instruction, cpu.pc()
                    )
                }
                DecodeError::Compressed(CompressedDecodeError::UnimplementedExtension(
                    ext,
                    instruction,
                )) => panic!(
                    "Tried to parse instruction with unimplemented extension `{}`: `{:04x}`.C",
                    ext, instruction
                ),
            }
        }
    }
}
