use rv32i_mc_interpreter::{CompressedDecodeError, DecodeError};
use std::io::Read;

fn main() {
    let mut buf = vec![];
    std::fs::File::open("rv32ui-p-simple.bin")
        .unwrap()
        .read_to_end(&mut buf)
        .unwrap();
    let mut cpu = rv32i_mc_interpreter::Cpu::new(&buf);
    loop {
        if let Err(e) = cpu.run() {
            match e {
                DecodeError::Other => panic!("Non specfic decode error"),
                DecodeError::InvalidInstruction(instruction) => {
                    panic!("Tried to parse invalid instruction: `{:08x}`", instruction)
                }
                DecodeError::UnimplementedExtension(ext, instruction) => panic!(
                    "Tried to parse instruction with unimplemented extension `{}`: `{:84x}`",
                    ext, instruction
                ),
                DecodeError::Compressed(CompressedDecodeError::InvalidInstruction(instruction)) => {
                    panic!(
                        "Tried to parse invalid instruction: `{:04x}`.C",
                        instruction
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
