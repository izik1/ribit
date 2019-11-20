#![feature(vec_remove_item, bool_to_option)]
#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    clippy::cast_lossless
)]

pub mod decode;
pub mod jit;

pub mod instruction;
pub mod opcode;
pub mod register;

const XLEN: usize = 32;

// todo: enum DecodeError { InvalidInstruction, Reserved, UnimplementedExtension(extension) }
#[derive(Debug)]
pub struct DecodeError;

pub struct Cpu {
    // xregs[0] is fixed to 0
    xregs: [u32; XLEN],
    pc: u32,
}
