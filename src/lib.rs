#![feature(vec_remove_item, bool_to_option)]
#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss,
    clippy::cast_lossless
)]

#[macro_use]
extern crate static_assertions;

pub mod decode;
pub mod instruction;
pub mod jit;
pub mod opcode;
pub mod register;

const XLEN: usize = 32;

const MEMORY_SIZE: u32 = (1024 * 1024 * 16);

// ensure that memory size is a power of two.
const_assert_eq!(MEMORY_SIZE.count_ones(), 1);

// todo: enum DecodeError { InvalidInstruction, Reserved, UnimplementedExtension(extension) }
#[derive(Debug)]
pub struct DecodeError;

pub struct Cpu {
    // xregs[0] is fixed to 0
    xregs: [u32; XLEN],
    pc: u32,
}
