use crate::reference::Reference;
use crate::{Id, IdAllocator, Instruction, Terminator};

pub struct BlockDisplay<'a>(&'a [Instruction], &'a Terminator);

impl<'a> std::fmt::Display for BlockDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for item in self.0 {
            writeln!(f, "{item}")?;
        }

        self.1.fmt(f)
    }
}

pub struct Block {
    pub allocator: IdAllocator,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

impl Block {
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
