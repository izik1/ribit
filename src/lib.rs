#![feature(vec_remove_item)]

pub mod decode;
pub mod jit;

use decode::{decode_rd, decode_rs};
pub mod instruction;

const XLEN: usize = 32;

// todo: enum DecodeError { InvalidInstruction, Reserved, UnimplementedExtension(extension) }
#[derive(Debug)]
pub struct DecodeError;

pub struct Cpu {
    // xregs[0] is fixed to 0
    xregs: [u32; XLEN],
    pc: u32,
}
