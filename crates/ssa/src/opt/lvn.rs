use fnv::FnvHashMap;
use ribit_core::Width;

use crate::instruction::{self, BinaryArgs, CmpArgs, CommutativeBinArgs};
use crate::reference::{Ref, Reference};
use crate::ty::ConstTy;
use crate::{
    AnySource, Arg, Block, CommutativeBinOp, Constant, Id, Instruction, ShiftOp, Source,
    SourcePair, eval, ty,
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
    CommutativeBinOp { args: CommutativeBinArgs },
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
    #[inline(always)]
    fn lookup_id(&self, id: Id) -> AnySource {
        match self.value_map.get(&id).copied() {
            Some(value) => value,
            None => panic!(),
        }
    }

    #[must_use]
    #[inline(always)]
    fn lookup_ref(&self, src: Reference) -> AnySource {
        self.lookup_id(src.id)
    }

    #[must_use]
    #[inline(always)]
    fn lookup(&self, src: AnySource) -> AnySource {
        match src {
            src @ AnySource::Const(_) => src,
            AnySource::Ref(it) => self.lookup_ref(it),
        }
    }

    #[must_use]
    #[inline(always)]
    fn typed_lookup_ref<T: ConstTy>(&self, src: Ref<T>) -> Source<T> {
        self.lookup_id(src.id).downcast().unwrap()
    }

    #[must_use]
    #[inline(always)]
    fn typed_lookup<T: ConstTy>(&self, src: Source<T>) -> Source<T> {
        match src {
            src @ Source::Const(_) => src,
            Source::Ref(id) => self.typed_lookup_ref(Ref::<T>::new(id)),
        }
    }

    fn map_value(&mut self, from: Id, to: AnySource) {
        let value = self.value_map.insert(from, to);

        debug_assert_eq!(value, None);
    }

    fn insert_equivalence(&mut self, dest: Reference, equivalence: EquivalenceInstruction) {
        let value = *self.equivalence_map.entry(equivalence).or_insert(dest);

        // this being incorrect implies that the `Eq` impl of the instruction is broken.
        ty::debug_assert_types_eq!(dest.ty, value.ty);

        self.map_value(dest.id, AnySource::Ref(value));
    }
}

enum Action {
    Map { old: Id, new: AnySource },
    AddEquivalence { equivalence: EquivalenceInstruction, value_number: Reference },
}

impl Action {
    #[must_use]
    const fn identity(value: Reference) -> Self {
        Self::replace(value.id, value)
    }

    #[must_use]
    const fn replace(old: Id, new: Reference) -> Self {
        Self::Map { old, new: AnySource::Ref(new) }
    }

    #[must_use]
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
                .lookup_ref(*src)
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
        &mut Instruction::ShiftOp { dest, ref mut src, op } => {
            let lhs = context.lookup(src.lhs());
            let rhs = context.lookup(src.rhs());

            match SourcePair::try_from((lhs, rhs)) {
                Ok(new) => {
                    *src = new;

                    Some(Action::AddEquivalence {
                        equivalence: EquivalenceInstruction::ShiftOp { src: *src, op },
                        value_number: reference(dest),
                    })
                }
                Err((lhs, rhs)) => Some(Action::constify(dest, eval::shift(lhs, rhs, op))),
            }
        }
        &mut Instruction::Sub { dest, ref mut src1, ref mut src2 } => {
            let lhs = context.lookup(*src1);
            let rhs = context.lookup_ref(*src2);

            match (lhs, rhs) {
                (AnySource::Const(lhs), AnySource::Const(rhs)) => {
                    return Some(Action::constify(dest, eval::sub(lhs, rhs)));
                }

                (lhs, AnySource::Ref(rhs)) => {
                    (*src1, *src2) = (lhs, rhs);
                }

                (AnySource::Ref(src1), AnySource::Const(src2)) => {
                    let src2 = AnySource::Const(eval::neg(src2));

                    let mut args = BinaryArgs { src1, src2, op: CommutativeBinOp::Add };

                    let action = commutative_binop(context, reference(dest), &mut args);

                    *instruction = Instruction::CommutativeBinOp { dest, args };

                    return Some(action);
                }
            }

            Some(Action::AddEquivalence {
                equivalence: EquivalenceInstruction::Sub { src1: *src1, src2: *src2 },
                value_number: reference(dest),
            })
        }
        &mut Instruction::Cmp { dest, ref mut args } => {
            let new =
                CmpArgs::new(context.lookup_ref(args.src1), context.lookup(args.src2), args.op);

            match new {
                Ok(new) => {
                    *args = new;

                    Some(Action::AddEquivalence {
                        equivalence: EquivalenceInstruction::Cmp { args: new },
                        value_number: reference(dest),
                    })
                }
                Err(value) => Some(Action::constify(dest, Constant::Bool(value))),
            }
        }
        &mut Instruction::CommutativeBinOp { dest, ref mut args } => {
            Some(commutative_binop(context, reference(dest), args))
        }
        Instruction::Select(instruction::Select { dest, cond, if_true, if_false }) => {
            *if_true = context.lookup(*if_true);
            *if_false = context.lookup(*if_false);

            let replacement = match context.typed_lookup_ref(*cond) {
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

        &mut Instruction::ExtInt(instruction::ExtInt { dest, width, ref mut src, signed }) => {
            *src = match context.lookup_ref(*src) {
                AnySource::Const(src) => {
                    return Some(Action::constify(
                        dest,
                        Constant::Int(eval::extend_int(width, src, signed)),
                    ));
                }
                AnySource::Ref(src) => src,
            };

            Some(Action::AddEquivalence {
                equivalence: EquivalenceInstruction::ExtInt(ExtInt { width, src: *src, signed }),
                value_number: reference(dest),
            })
        }
        Instruction::Fence => None,
    }
}

fn commutative_binop(context: &Context, dest: Reference, args: &mut CommutativeBinArgs) -> Action {
    let new_args = CommutativeBinArgs::new(
        args.op,
        context.lookup(AnySource::Ref(args.src1)),
        context.lookup(args.src2),
    );

    *args = match new_args {
        Ok(args) => args,
        Err(res) => return Action::Map { old: dest.id, new: res },
    };

    Action::AddEquivalence {
        equivalence: EquivalenceInstruction::CommutativeBinOp { args: *args },
        value_number: dest,
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
