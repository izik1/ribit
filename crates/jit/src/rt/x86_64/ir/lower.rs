use std::collections::HashMap;

use ribit_core::register::RiscV;
use ribit_core::Width;
use ribit_ssa::instruction::{ExtInt, Select};
use ribit_ssa::{self as ssa, CmpKind, CommutativeBinOp};
use ssa::reference::Reference;
use ssa::ty::I32;
use ssa::{AnySource, Constant};

use super::instruction::{
    self, BinaryOpKind, Instruction, MaybeTiedBinaryArgs, Terminator, TiedBinaryArgs64, UnaryArgs,
    UnaryOpKind, UntiedBinaryArgs, UntiedBinaryArgs64,
};
use super::{Block, Id, IdAllocator, Memory, Register};
use crate::rt::x86_64::{register_alloc, BlockReturn};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Source {
    Val(u32),
    Register(Register),
}

impl Source {
    #[inline]
    #[must_use]
    pub(crate) fn is_reg(self) -> bool {
        matches!(self, Self::Register(_))
    }

    #[inline]
    #[must_use]
    pub(crate) fn is_val(self) -> bool {
        matches!(self, Self::Val(_))
    }
}

#[derive(Default)]
struct Context {
    id_map: HashMap<ribit_ssa::Id, Id>,
    id_alloc: IdAllocator,
    args: HashMap<Id, crate::Register>,
    instructions: Vec<Instruction>,
}

impl Context {
    fn map_src(&mut self, src: &AnySource) -> Source {
        match src {
            AnySource::Const(constant) => Source::Val(get_val(*constant)),
            AnySource::Ref(reference) => {
                Source::Register(Register::symbolic(self.map_id(reference.id)))
            }
        }
    }

    fn map_i32_src(&mut self, src: &ribit_ssa::Source<I32>) -> Source {
        match src {
            ribit_ssa::Source::Const(v) => Source::Val(*v),
            ribit_ssa::Source::Ref(id) => Source::Register(Register::symbolic(self.map_id(*id))),
        }
    }

    fn map_id(&mut self, id: ribit_ssa::Id) -> Id {
        *self.id_map.entry(id).or_insert_with(|| self.id_alloc.allocate())
    }

    fn reg_mem(&mut self, base: &ribit_ssa::Source<I32>, displacement: RiscV) -> Memory {
        let base = self.map_i32_src(base);
        let displacement = Source::Val((displacement.get() * 4) as u32);

        self.mem(base, displacement)
    }

    fn mem(&mut self, base: Source, displacement: Source) -> Memory {
        match (base, displacement) {
            (Source::Val(base), Source::Val(displacement)) => {
                Memory::displacement(base.checked_add(displacement).unwrap().try_into().unwrap())
            }
            (Source::Val(displacement), Source::Register(base))
            | (Source::Register(base), Source::Val(displacement)) => {
                Memory::base_displacement(base, displacement as i32)
            }
            (Source::Register(base), Source::Register(index)) => {
                Memory::base_index(base, index).unwrap()
            }
        }
    }

    fn mov_imm(&mut self, imm: u32) -> Id {
        let tmp = self.id_alloc.allocate();

        self.instructions.push(Instruction::Mov {
            args: UntiedBinaryArgs::RegImm(Register::symbolic(tmp), imm),
            width: Width::DWord,
        });

        tmp
    }

    fn mov_fixed_reg(&mut self, dest: crate::Register, width: Width, src: Register) {
        self.instructions.push(Instruction::Mov {
            args: UntiedBinaryArgs::RegReg(Register::Register(dest, false), src),
            width,
        });
    }

    fn unary_op(&mut self, args: UnaryArgs, kind: UnaryOpKind) {
        self.instructions.push(instruction::UnaryOp { args, kind }.into());
    }

    fn binary_op(&mut self, args: MaybeTiedBinaryArgs, kind: BinaryOpKind) {
        self.instructions.push(instruction::BinaryOp { args, kind }.into());
    }

    fn zeroize(&mut self, dest: Register) {
        self.binary_op(
            MaybeTiedBinaryArgs::RegReg { dest, src1: dest, src2: dest },
            BinaryOpKind::Xor,
        );
    }
}

fn get_val(konst: Constant) -> u32 {
    match konst {
        Constant::Int(i) => {
            assert_eq!(i.bits(), 32);
            i.unsigned()
        }
        // fixme: should we panic here?
        Constant::Bool(i) => i as u32,
    }
}

pub fn lower(ssa_block: &ribit_ssa::Block) -> Block {
    let mut ctx = Context::default();

    for instruction in ssa_block.instructions.iter() {
        match instruction {
            ribit_ssa::Instruction::Arg { dest, src } => {
                let dest = ctx.map_id(*dest);
                let src = register_alloc::arg_register(*src);
                ctx.args.insert(dest, src);
                let dest = Register::symbolic(dest);

                let src = Register::Register(src, false);

                ctx.instructions
                    .push(Instruction::Mov64 { args: UntiedBinaryArgs64::RegReg(dest, src) });
            }
            ribit_ssa::Instruction::WriteStack { dest, src } => {
                let src = Register::symbolic(ctx.map_id(src.id));
                let sp = Register::Register(crate::Register::Zsp, false);

                let dest = ctx.mem(Source::Register(sp), Source::Val(dest.offset(true) as u32));

                ctx.instructions.push(Instruction::Mov {
                    args: UntiedBinaryArgs::MemReg(dest, src),
                    width: Width::DWord,
                });
            }
            ribit_ssa::Instruction::ReadStack { dest, src } => {
                let sp = Register::Register(crate::Register::Zsp, false);

                let src = ctx.mem(Source::Register(sp), Source::Val(src.offset(true) as u32));

                let dest = Register::symbolic(ctx.map_id(*dest));

                ctx.instructions.push(Instruction::Mov {
                    args: UntiedBinaryArgs::RegMem(dest, src),
                    width: Width::DWord,
                });
            }
            ribit_ssa::Instruction::ReadReg { dest, base, src } => {
                let dest = ctx.map_id(*dest);
                let mem = ctx.reg_mem(base, *src);

                ctx.instructions.push(Instruction::Mov {
                    args: UntiedBinaryArgs::RegMem(Register::symbolic(dest), mem),
                    width: Width::DWord,
                });
            }
            ribit_ssa::Instruction::WriteReg { dest, base, src } => {
                let mem = ctx.reg_mem(base, *dest);

                let op = match src {
                    ribit_ssa::AnySource::Const(constant) => {
                        UntiedBinaryArgs::MemImm(mem, get_val(*constant))
                    }
                    ribit_ssa::AnySource::Ref(reference) => {
                        UntiedBinaryArgs::MemReg(mem, Register::symbolic(ctx.map_id(reference.id)))
                    }
                };

                ctx.instructions.push(Instruction::Mov { args: op, width: Width::DWord })
            }
            ribit_ssa::Instruction::ReadMem { dest, src, base, width, sign_extend } => {
                let dest = Register::symbolic(ctx.map_id(*dest));
                let base = ctx.map_i32_src(base);
                let offset = ctx.map_src(src);

                let mem = ctx.mem(base, offset);

                // todo: handle address space wraps
                match (width, sign_extend) {
                    (Width::DWord, _) => ctx.instructions.push(Instruction::Mov {
                        args: UntiedBinaryArgs::RegMem(dest, mem),
                        width: Width::DWord,
                    }),
                    (width, true) => {
                        ctx.instructions.push(Instruction::MovSx32 {
                            args: UntiedBinaryArgs::RegMem(dest, mem),
                            src_width: *width,
                        });
                    }
                    (width, false) => {
                        ctx.instructions.push(Instruction::MovZx32 {
                            args: UntiedBinaryArgs::RegMem(dest, mem),
                            src_width: *width,
                        });
                    }
                }
            }
            ribit_ssa::Instruction::WriteMem { addr, src, base, width } => {
                let base = ctx.map_i32_src(base);
                let offset = ctx.map_i32_src(addr);
                let mem = ctx.mem(base, offset);
                let src = ctx.map_src(src);

                let op = match src {
                    Source::Val(imm) => UntiedBinaryArgs::MemImm(mem, imm),
                    Source::Register(reg) => UntiedBinaryArgs::MemReg(mem, reg),
                };

                ctx.instructions.push(Instruction::Mov { args: op, width: *width });
            }
            ribit_ssa::Instruction::ShiftOp { dest, src, op } => {
                let dest = Register::symbolic(ctx.map_id(*dest));

                let (src1, src2) = match src {
                    ribit_ssa::SourcePair::RefRef(src1, src2) => (
                        Register::symbolic(ctx.map_id(src1.id)),
                        Source::Register(Register::symbolic(ctx.map_id(src2.id))),
                    ),
                    ribit_ssa::SourcePair::RefConst(src1, src2) => {
                        (Register::symbolic(ctx.map_id(src1.id)), Source::Val(get_val(*src2)))
                    }
                    // No instruction for `x = 1 << y` exists, unless we turn `1` into a register.
                    ribit_ssa::SourcePair::ConstRef(src1, src2) => (
                        Register::symbolic(ctx.mov_imm(get_val(*src1))),
                        Source::Register(Register::symbolic(ctx.map_id(src2.id))),
                    ),
                };

                let unary = UnaryArgs::Reg { dest, src: src1 };

                let instruction = match src2 {
                    Source::Val(src2) => {
                        Instruction::ShiftImm { args: unary, kind: *op, src2: src2 as u8 }
                    }
                    Source::Register(src2) => {
                        ctx.mov_fixed_reg(crate::Register::Zcx, Width::Byte, src2);
                        Instruction::ShiftCl { args: unary, kind: *op }
                    }
                };

                ctx.instructions.push(instruction);
            }
            ribit_ssa::Instruction::Sub { dest, src1, src2 } => {
                lower_sub(&mut ctx, *dest, src1, src2)
            }

            ribit_ssa::Instruction::Cmp { dest, args } => {
                let dest = Register::symbolic(ctx.map_id(*dest));
                let src1 = Register::symbolic(ctx.map_id(args.src1.id));
                let src2 = ctx.map_src(&args.src2);

                // zeroize `dest` (for a `setcc`).
                ctx.zeroize(dest);

                // compare `src1` and `src2`.
                let compare = match src2 {
                    Source::Val(0) => {
                        Instruction::Test { args: UntiedBinaryArgs::RegReg(src1, src1) }
                    }
                    Source::Val(src2) => {
                        Instruction::Cmp { args: UntiedBinaryArgs::RegImm(src1, src2) }
                    }

                    Source::Register(src2) => {
                        Instruction::Cmp { args: UntiedBinaryArgs::RegReg(src1, src2) }
                    }
                };

                ctx.instructions.push(compare);
                // and finally, set the output bit based on the condition.
                ctx.instructions.push(Instruction::SetCC { dest, condition: args.kind })
            }
            ribit_ssa::Instruction::CommutativeBinOp { dest, src1, src2, op } => {
                let dest = Register::symbolic(ctx.map_id(*dest));
                let src1 = Register::symbolic(ctx.map_id(src1.id));
                let src2 = ctx.map_src(src2);

                let args = match src2 {
                    // we only care about values that are idioms for certain operations, not IR simplifications,
                    // we'd expect those to be optimized out already.
                    Source::Val(1) if *op == CommutativeBinOp::Add => {
                        ctx.unary_op(UnaryArgs::Reg { dest, src: src1 }, UnaryOpKind::Inc);
                        continue;
                    }

                    Source::Val(u32::MAX) if *op == CommutativeBinOp::Add => {
                        ctx.unary_op(UnaryArgs::Reg { dest, src: src1 }, UnaryOpKind::Dec);

                        continue;
                    }

                    Source::Val(u32::MAX) if *op == CommutativeBinOp::Xor => {
                        ctx.unary_op(UnaryArgs::Reg { dest, src: src1 }, UnaryOpKind::Not);

                        continue;
                    }
                    Source::Val(src2) => MaybeTiedBinaryArgs::RegImm { dest, src1, src2 },
                    Source::Register(src2) => MaybeTiedBinaryArgs::RegReg { dest, src1, src2 },
                };

                let kind = match op {
                    CommutativeBinOp::And => BinaryOpKind::And,
                    CommutativeBinOp::Add => BinaryOpKind::Add,
                    CommutativeBinOp::Or => BinaryOpKind::Or,
                    CommutativeBinOp::Xor => BinaryOpKind::Xor,
                };

                ctx.binary_op(args, kind);
            }
            ribit_ssa::Instruction::Select(select) => lower_select(&mut ctx, select),
            ribit_ssa::Instruction::ExtInt(ext_int) => lower_ext_int(&mut ctx, ext_int),
            // fixme: don't ignore fences
            ribit_ssa::Instruction::Fence => continue,
        }
    }

    let term = match &ssa_block.terminator {
        // note: addr is in the low dword, code is high dword
        ribit_ssa::Terminator::Ret { addr, code } => {
            let addr = ctx.map_i32_src(addr);
            let dest = Register::Register(crate::Register::Zax, false);

            match (addr, code) {
                (Source::Val(it), code) if *code as u32 == 0 => {
                    ctx.instructions.push(Instruction::Mov {
                        args: UntiedBinaryArgs::RegImm(dest, it),
                        width: Width::DWord,
                    })
                }
                (Source::Register(register), code) if *code as u32 == 0 => {
                    ctx.instructions.push(Instruction::Mov {
                        args: UntiedBinaryArgs::RegReg(dest, register),
                        width: Width::DWord,
                    })
                }

                (Source::Val(addr), code) => ctx.instructions.push(Instruction::Mov64 {
                    args: UntiedBinaryArgs64::RegImm(
                        dest,
                        BlockReturn::from_parts(addr, *code).as_u64(),
                    ),
                }),
                (Source::Register(register), code) => {
                    let tmp = Register::symbolic(ctx.id_alloc.allocate());
                    ctx.instructions.push(Instruction::Mov {
                        args: UntiedBinaryArgs::RegReg(tmp, register),
                        width: Width::DWord,
                    });

                    ctx.instructions.push(Instruction::Or64 {
                        args: TiedBinaryArgs64::RegImm {
                            dest,
                            src1: tmp,
                            src2: (*code as u64) << 32,
                        },
                    });
                }
            };

            Terminator::Ret
        }
    };

    Block { instructions: ctx.instructions, term, args: ctx.args, id_alloc: ctx.id_alloc }
}

fn lower_sub(ctx: &mut Context, dest: ribit_ssa::Id, src1: &AnySource, src2: &Reference) {
    let dest = ctx.map_id(dest);
    let src2 = ctx.map_id(src2.id);

    let src1 = match src1 {
        ribit_ssa::AnySource::Const(constant) => {
            let imm = get_val(*constant);
            // `0 - x` -> `neg x`
            if imm == 0 {
                ctx.unary_op(
                    UnaryArgs::Reg {
                        dest: Register::symbolic(dest),
                        src: Register::symbolic(src2),
                    },
                    UnaryOpKind::Neg,
                );

                return;
            }
            // can't write `{n} - x` without a `mov` as there's no 3 arg version.

            ctx.mov_imm(imm)
        }
        ribit_ssa::AnySource::Ref(reference) => ctx.map_id(reference.id),
    };

    ctx.binary_op(
        MaybeTiedBinaryArgs::RegReg {
            dest: Register::symbolic(dest),
            src1: Register::symbolic(src1),
            src2: Register::symbolic(src2),
        },
        BinaryOpKind::Sub,
    );
}

fn add_r32_imm(dest: Register, src: Register, v: u32) -> Instruction {
    // inc/dec is cheapest (on most modern cpus)
    if v == 1 {
        return instruction::UnaryOp::inc(UnaryArgs::Reg { dest, src }).into();
    }

    if v == u32::MAX {
        return instruction::UnaryOp::dec(UnaryArgs::Reg { dest, src }).into();
    }

    instruction::BinaryOp {
        args: MaybeTiedBinaryArgs::RegImm { dest, src1: src, src2: v },
        kind: BinaryOpKind::Add,
    }
    .into()
}

fn lower_select(ctx: &mut Context, Select { dest, cond, if_true, if_false }: &Select) {
    let dest = Register::symbolic(ctx.map_id(*dest));
    let cond = Register::symbolic(ctx.map_id(cond.id));
    let if_true = ctx.map_src(if_true);
    let if_false = ctx.map_src(if_false);

    // First we need to compare  `cond` to 0
    // note: there are some optimizations that could be done by doing this lower, todo: do this later.
    ctx.instructions.push(Instruction::Test { args: UntiedBinaryArgs::RegReg(cond, cond) });

    let (mov_reg, equal) = match (if_true, if_false) {
        // clobber case
        (Source::Val(a), Source::Val(b)) => {
            // todo: optimize by checking for 0 and doing a xor if `dest != cond` (involves skipping the above `test` instr.)
            // likewise, flipping this on its head
            ctx.instructions.push(Instruction::Mov {
                args: UntiedBinaryArgs::RegImm(dest, b),
                width: Width::DWord,
            });

            let clobber = ctx.mov_imm(a);

            (Register::symbolic(clobber), false)
        }

        // we don't need to clobber here, since we can `mov dest, v` and then `cmov{n}e` the other one
        (Source::Register(r), Source::Val(v)) | (Source::Val(v), Source::Register(r)) => {
            // todo: optimize by checking for 0 and doing a xor if `dest != cond` (involves skipping the above `test` instr.)
            ctx.instructions.push(Instruction::Mov {
                args: UntiedBinaryArgs::RegImm(dest, v),
                width: Width::DWord,
            });

            (r, matches!(if_true, Source::Register(_)))
        }

        (Source::Register(if_true), Source::Register(if_false)) => {
            ctx.instructions.push(Instruction::Mov {
                args: UntiedBinaryArgs::RegReg(dest, if_false),
                width: Width::Word,
            });

            (if_true, true)
        }
    };

    let condition = match equal {
        true => CmpKind::Eq,
        false => CmpKind::Ne,
    };

    ctx.instructions
        .push(Instruction::CMovCC { args: UntiedBinaryArgs::RegReg(dest, mov_reg), condition });
}

fn lower_ext_int(ctx: &mut Context, ExtInt { dest, width: _, src, signed }: &ExtInt) {
    let dest = Register::symbolic(ctx.map_id(*dest));

    let src_reg = Register::symbolic(ctx.map_id(src.id));

    // zero extending is refreshingly boring.
    if !signed {
        ctx.instructions.push(Instruction::MovZx32 {
            args: UntiedBinaryArgs::RegReg(dest, src_reg),
            src_width: match src.ty {
                ribit_ssa::Type::Int(bitness) => bitness.to_width(),
                ribit_ssa::Type::Boolean => Width::Byte,
                ribit_ssa::Type::Unit => panic!("Invalid extend type"),
            },
        });
        return;
    }

    let instruction = match src.ty {
        ribit_ssa::Type::Int(bitness) => Instruction::MovSx32 {
            args: UntiedBinaryArgs::RegReg(dest, src_reg),
            src_width: bitness.to_width(),
        },
        // 0 -> -0 -> 0
        // 1 -> -1 -> 0xffff_ffff
        // if width is < 32 the top `32 - x` bits become "don't care"
        ribit_ssa::Type::Boolean => {
            instruction::UnaryOp::neg(UnaryArgs::Reg { dest, src: src_reg }).into()
        }
        ribit_ssa::Type::Unit => panic!("Invalid extend type"),
    };

    ctx.instructions.push(instruction);
}
