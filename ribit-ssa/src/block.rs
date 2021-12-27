use crate::reference::Reference;
use crate::{Arg, Id, IdAllocator, Instruction, Terminator, Type};

pub struct BlockDisplay<'a>(&'a [Instruction], &'a Terminator);

impl<'a> std::fmt::Display for BlockDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Some((first, rest)) = self.0.split_first() {
            write!(f, "{}", first)?;
            for item in rest {
                write!(f, "\n{}", item)?;
            }

            write!(f, "\n{}", self.1)?;
        } else {
            write!(f, "{}", self.1)?;
        }

        Ok(())
    }
}

pub struct Block {
    pub allocator: IdAllocator,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

impl Block {
    #[must_use]
    pub fn arg_ref(&self, arg: Arg) -> Option<Reference> {
        self.instructions.iter().find_map(|it| match it {
            Instruction::Arg { dest, src } if *src == arg => {
                Some(Reference { ty: Type::I32, id: *dest })
            }
            _ => None,
        })
    }

    #[must_use]
    pub fn reference(&self, id: Id) -> Option<Reference> {
        self.instructions
            .iter()
            .find_map(|it| it.id().filter(|it| *it == id).map(|id| Reference { ty: it.ty(), id }))
    }

    #[must_use]
    pub fn display_instructions(&self) -> BlockDisplay<'_> {
        BlockDisplay(&self.instructions, &self.terminator)
    }
}
