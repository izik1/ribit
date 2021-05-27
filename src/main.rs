use ribit::{CompressedDecodeError, DecodeError};
use std::io::Write;
use std::{convert::TryInto, env, fs::File, io::Read};

fn main() {
    env_logger::init();

    let mut args = env::args().skip(1);

    let path = args.next().unwrap();

    let sig_file = args.next().unwrap();

    let mut buf = vec![];
    std::fs::File::open(path)
        .unwrap()
        .read_to_end(&mut buf)
        .unwrap();
    let mut cpu = ribit::Cpu::new_elf(&buf);
    loop {
        if let Err(e) = cpu.run() {
            match e {
                DecodeError::Other => panic!("Non specfic decode error"),
                DecodeError::InvalidInstruction(instruction) => panic!(
                    "Tried to parse invalid instruction: `{:08x}` @ pc {:08x}",
                    instruction,
                    cpu.pc()
                ),
                DecodeError::UnimplementedExtension(ext, instruction) => panic!(
                    "Tried to parse instruction with unimplemented extension `{}`: `{:84x}`",
                    ext, instruction
                ),
                DecodeError::Compressed(CompressedDecodeError::InvalidInstruction(instruction)) => {
                    panic!(
                        "Tried to parse invalid instruction: `{:04x}`.C @ pc {:08x}",
                        instruction,
                        cpu.pc()
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

        if cpu.to_host() {
            if let Some(signature) = cpu.signature() {
                let mut file = File::create(sig_file).unwrap();

                for chunk in signature.chunks(4) {
                    write!(
                        file,
                        "{:08x}\n",
                        u32::from_le_bytes(chunk.try_into().unwrap())
                    )
                    .unwrap();
                }
                break;
            }
        }
    }
}
