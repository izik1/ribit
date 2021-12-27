use std::convert::TryInto;
use std::env;
use std::fs::File;
use std::io::{Read, Write};

use ribit_decode::{CompressedDecodeError, DecodeError};

fn main() {
    env_logger::init();

    // dbg!(ribit_core::register::RiscV::X1);

    let mut args = env::args().skip(1);

    let path = args.next().unwrap();

    let sig_file = args.next().unwrap();

    let mut buf = vec![];
    std::fs::File::open(path).unwrap().read_to_end(&mut buf).unwrap();
    let mut ee = ribit::ExecutionEngine::new_elf(&buf);
    loop {
        if let Err(e) = ee.run() {
            match e {
                DecodeError::Other => panic!("Non specfic decode error"),
                DecodeError::InvalidInstruction(instruction) => panic!(
                    "Tried to parse invalid instruction: `{:08x}` @ pc {:08x}",
                    instruction,
                    ee.pc()
                ),
                DecodeError::UnimplementedExtension(ext, instruction) => panic!(
                    "Tried to parse instruction with unimplemented extension `{}`: `{:84x}`",
                    ext, instruction
                ),
                DecodeError::Compressed(CompressedDecodeError::InvalidInstruction(instruction)) => {
                    panic!(
                        "Tried to parse invalid instruction: `{:04x}`.C @ pc {:08x}",
                        instruction,
                        ee.pc()
                    );
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

        if ee.to_host() {
            if let Some(signature) = ee.signature() {
                let mut file = File::create(sig_file).unwrap();

                for chunk in signature.chunks(4) {
                    writeln!(file, "{:08x}", u32::from_le_bytes(chunk.try_into().unwrap()))
                        .unwrap();
                }
                break;
            }
        }
    }
}
