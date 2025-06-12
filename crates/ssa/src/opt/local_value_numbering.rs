use fnv::FnvHashMap;
use ribit_core::Width;

use crate::instruction::{self, BinaryArgs, CmpArgs};
use crate::reference::{Ref, Reference};
use crate::ty::ConstTy;
use crate::{
    AnySource, Arg, Block, CommutativeBinOp, Constant, Id, Instruction, ShiftOp, Source,
    SourcePair, debug_assert_types_eq, eval, ty,
};

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub(crate) struct Select {
    pub cond: Ref<ty::Bool>,
    pub if_true: AnySource,
    pub if_false: AnySource,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub(crate) struct ExtInt {
    pub width: Width,
    pub src: Reference,
    pub signed: bool,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub(crate) enum EquivalenceInstruction {
    Arg { src: Arg },
    ShiftOp { src: SourcePair, op: ShiftOp },

    Sub { src1: AnySource, src2: Reference },
    Cmp { args: CmpArgs },
    // if src1 and src2 are both references, the lower IDed one must go first (just to pick one so that the equivalence relation works)
    CommutativeBinOp { src1: Reference, src2: AnySource, op: CommutativeBinOp },
    // todo: box this
    Select(Select),
    ExtInt(ExtInt),
}

#[derive(Default)]
struct Context {
    /// map of ID -> value number (ID in the new SSA) or constant.
    value_map: FnvHashMap<Id, AnySource>,
    /// map of instruction -> value number
    equivalence_map: FnvHashMap<EquivalenceInstruction, Reference>,
}

impl Context {
    #[must_use]
    fn lookup(&self, src: AnySource) -> AnySource {
        match src {
            src @ AnySource::Const(_) => src,
            AnySource::Ref(it) => match self.value_map.get(&it.id).copied() {
                Some(value) => value,
                None => panic!(),
            },
        }
    }

    #[must_use]
    fn typed_lookup<T: ConstTy>(&self, src: Source<T>) -> Source<T> {
        match src {
            src @ Source::Const(_) => src,
            Source::Ref(it) => match self.value_map.get(&it).copied() {
                Some(c) => c.downcast().unwrap(),
                None => panic!(),
            },
        }
    }

    fn map_value(&mut self, from: Id, to: AnySource) {
        let value = self.value_map.insert(from, to);

        debug_assert_eq!(value, None);
    }

    fn insert_equivalence(&mut self, dest: Reference, equivalence: EquivalenceInstruction) {
        let value = *self.equivalence_map.entry(equivalence).or_insert(dest);

        // this being incorrect implies that the `Eq` impl of the instruction is broken.
        debug_assert_types_eq!(dest.ty, value.ty);

        self.map_value(dest.id, AnySource::Ref(value));
    }
}

enum Action {
    Map { old: Id, new: AnySource },
    AddEquivalence { equivalence: EquivalenceInstruction, value_number: Reference },
}

impl Action {
    const fn identity(value: Reference) -> Self {
        Self::replace(value.id, value)
    }

    const fn replace(old: Id, new: Reference) -> Self {
        Self::Map { old, new: AnySource::Ref(new) }
    }

    const fn constify(old: Id, new: Constant) -> Self {
        Self::Map { old, new: AnySource::Const(new) }
    }
}

fn run_instruction(context: &Context, instruction: &mut Instruction) -> Option<Action> {
    let ty = instruction.ty();

    let reference = |id| Reference::new(ty, id);

    match instruction {
        Instruction::Arg { dest, src } => Some(Action::AddEquivalence {
            equivalence: EquivalenceInstruction::Arg { src: *src },
            value_number: reference(*dest),
        }),

        // todo: registers and stack can be simplified.
        Instruction::WriteStack { dest: _, src } => {
            // in practice this instruction will almost certainly never be seen by this pass, because stack reads/writes get inserted by regalloc.
            // still a good idea to fix it though.
            *src = context
                .lookup(AnySource::Ref(*src))
                .reference()
                .expect("fixme: handle src no longer being a reference");

            None
        }
        Instruction::ReadStack { dest, src: _ } => Some(Action::identity(reference(*dest))),

        Instruction::ReadReg { dest, base, src: _ } => {
            *base = context.typed_lookup(*base);

            Some(Action::identity(reference(*dest)))
        }
        Instruction::WriteReg { dest: _, base, src } => {
            *base = context.typed_lookup(*base);
            *src = context.lookup(*src);

            None
        }

        Instruction::ReadMem { dest, src, base, width: _, sign_extend: _ } => {
            *base = context.typed_lookup(*base);
            *src = context.lookup(*src);

            Some(Action::identity(reference(*dest)))
        }
        Instruction::WriteMem { addr, src, base, width: _ } => {
            *base = context.typed_lookup(*base);
            *addr = context.typed_lookup(*addr);
            *src = context.lookup(*src);

            None
        }
        Instruction::ShiftOp { dest, src, op } => {
            let lhs = context.lookup(src.lhs());
            let rhs = context.lookup(src.rhs());

            match SourcePair::try_from((lhs, rhs)) {
                Ok(new) => {
                    *src = new;

                    Some(Action::AddEquivalence {
                        equivalence: EquivalenceInstruction::ShiftOp { src: *src, op: *op },
                        value_number: reference(*dest),
                    })
                }
                Err((lhs, rhs)) => Some(Action::constify(*dest, eval::shift(lhs, rhs, *op))),
            }
        }
        Instruction::Sub { dest, src1, src2 } => {
            let lhs = context.lookup(*src1);
            let rhs = context.lookup(AnySource::Ref(*src2));

            match (lhs, rhs) {
                (AnySource::Const(lhs), AnySource::Const(rhs)) => {
                    return Some(Action::constify(*dest, eval::sub(lhs, rhs)));
                }

                (lhs, AnySource::Ref(rhs)) => {
                    *src1 = lhs;
                    *src2 = rhs;
                }

                (AnySource::Ref(src1), AnySource::Const(src2)) => {
                    let src2 = AnySource::Const(eval::neg(src2));

                    let dest = *dest;
                    *instruction = Instruction::CommutativeBinOp {
                        dest,
                        args: BinaryArgs { src1, src2, op: CommutativeBinOp::Add },
                    };

                    return Some(Action::AddEquivalence {
                        equivalence: EquivalenceInstruction::CommutativeBinOp {
                            src1,
                            src2,
                            op: CommutativeBinOp::Add,
                        },
                        value_number: reference(dest),
                    });
                }
            };

            Some(Action::AddEquivalence {
                equivalence: EquivalenceInstruction::Sub { src1: *src1, src2: *src2 },
                value_number: reference(*dest),
            })
        }
        Instruction::Cmp { dest, args } => {
            let new = CmpArgs::new(
                context.lookup(AnySource::Ref(args.src1)),
                context.lookup(args.src2),
                args.kind,
            );

            match new {
                Ok(new) => {
                    *args = new;

                    Some(Action::AddEquivalence {
                        equivalence: EquivalenceInstruction::Cmp { args: new },
                        value_number: reference(*dest),
                    })
                }
                Err(value) => Some(Action::constify(*dest, ty::Constant::Bool(value))),
            }
        }
        Instruction::CommutativeBinOp { dest, args: BinaryArgs { src1, src2, op } } => {
            let lhs = context.lookup(AnySource::Ref(*src1));
            let rhs = context.lookup(*src2);

            (*src1, *src2) = match (lhs, rhs) {
                (AnySource::Const(src1), AnySource::Const(src2)) => {
                    return Some(Action::constify(*dest, eval::commutative_binop(src1, src2, *op)));
                }
                (AnySource::Ref(src1), src2 @ AnySource::Const(_))
                | (src2 @ AnySource::Const(_), AnySource::Ref(src1)) => (src1, src2),
                (AnySource::Ref(src1), AnySource::Ref(src2)) if src1.id <= src2.id => {
                    (src1, AnySource::Ref(src2))
                }
                (src1 @ AnySource::Ref(_), AnySource::Ref(src2)) => (src2, src1),
            };

            let res = eval::commutative_absorb(*src1, *src2, *op)
                .map(AnySource::Const)
                .or_else(|| eval::commutative_identity(*src1, *src2, *op).map(AnySource::Ref));

            if let Some(res) = res {
                return Some(Action::Map { old: *dest, new: res });
            }

            Some(Action::AddEquivalence {
                equivalence: EquivalenceInstruction::CommutativeBinOp {
                    src1: *src1,
                    src2: *src2,
                    op: *op,
                },
                value_number: reference(*dest),
            })
        }
        Instruction::Select(instruction::Select { dest, cond, if_true, if_false }) => {
            *if_true = context.lookup(*if_true);
            *if_false = context.lookup(*if_false);

            let replacement = match context.typed_lookup(cond.to_source()) {
                Source::Const(true) => *if_true,
                Source::Const(false) => *if_false,
                Source::Ref(id) => {
                    *cond = Ref::new(id);

                    return Some(Action::AddEquivalence {
                        equivalence: EquivalenceInstruction::Select(Select {
                            cond: *cond,
                            if_true: *if_true,
                            if_false: *if_false,
                        }),
                        value_number: reference(*dest),
                    });
                }
            };

            Some(Action::Map { old: *dest, new: replacement })
        }

        Instruction::ExtInt(instruction::ExtInt { dest, width, src, signed }) => {
            *src = match context.lookup(AnySource::Ref(*src)) {
                AnySource::Const(src) => {
                    return Some(Action::constify(
                        *dest,
                        Constant::Int(eval::extend_int(*width, src, *signed)),
                    ));
                }
                AnySource::Ref(src) => src,
            };

            Some(Action::AddEquivalence {
                equivalence: EquivalenceInstruction::ExtInt(ExtInt {
                    width: *width,
                    src: *src,
                    signed: *signed,
                }),
                value_number: reference(*dest),
            })
        }
        Instruction::Fence => None,
    }
}

pub fn run(block: &mut Block) {
    let mut context = Context::default();

    // this is an upper bound on the required size, the exact number is approximately the number of instructions that have an `Id`.
    context.value_map.reserve(block.instructions.len());

    for instruction in &mut block.instructions {
        match run_instruction(&context, instruction) {
            Some(Action::Map { old, new }) => {
                context.map_value(old, new);
            }
            Some(Action::AddEquivalence { equivalence, value_number }) => {
                context.insert_equivalence(value_number, equivalence);
            }

            None => {}
        }
    }

    match &mut block.terminator {
        crate::Terminator::Ret { addr, code: _ } => {
            *addr = context.typed_lookup(*addr);
        }
    }
}
