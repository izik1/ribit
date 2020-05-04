use super::BlockBuilder;
use crate::jit::Source;

use rasen::params::{imm::Imm32, reg::Reg32, Register};
use std::io;

pub fn select(
    builder: &mut BlockBuilder<'_, '_>,
    dest: Register,
    cond: Register,
    if_true: Source,
    if_false: Source,
    clobber_reg: Option<Register>,
) -> io::Result<()> {
    // fixme: handle not having clobbers when we want them, it technically isn't a bug in this case.
    // First we need to compare  `cond` to 0
    // note: there are some optimizations that could be done by doing this lower, todo: do this later.
    builder.stream.test_reg_reg(Reg32(cond), Reg32(cond))?;

    let (mov_reg, equal) = match (if_true, if_false) {
        // clobber case
        (Source::Val(a), Source::Val(b)) => {
            // todo: optimize by checking for 0 and doing a xor if `dest != cond` (involves skipping the above `test` instr.)
            builder.stream.mov_reg_imm(dest, Imm32(b))?;

            let clobber_reg =
                clobber_reg.expect("todo: handle clobber not existing here (via branches)");
            builder.stream.mov_reg_imm(clobber_reg, Imm32(a))?;
            (clobber_reg, true)
        }

        // clobber case
        (Source::Register(r), Source::Val(v)) | (Source::Val(v), Source::Register(r))
            if r == dest =>
        {
            let clobber_reg =
                clobber_reg.expect("todo: handle clobber not existing here (via branches)");
            builder.stream.mov_reg_imm(clobber_reg, Imm32(v))?;
            (clobber_reg, if_true.val().is_some())
        }

        // we don't need to clobber here, since we can `mov dest, v` and then `cmov{n}e` the other one
        (Source::Register(r), Source::Val(v)) | (Source::Val(v), Source::Register(r)) => {
            // todo: optimize by checking for 0 and doing a xor if `dest != cond` (involves skipping the above `test` instr.)
            builder.stream.mov_reg_imm(dest, Imm32(v))?;
            (r, if_true.reg().is_some())
        }

        (Source::Register(if_true), Source::Register(if_false)) => {
            if dest == if_true {
                (if_false, false)
            } else {
                if dest != if_false {
                    builder.stream.mov_reg_reg(Reg32(dest), Reg32(if_false))?;
                }

                (if_true, true)
            }
        }
    };

    match equal {
        true => builder.stream.cmove_reg_reg(Reg32(dest), Reg32(mov_reg)),
        false => builder.stream.cmovne_reg_reg(Reg32(dest), Reg32(mov_reg)),
    }
}
