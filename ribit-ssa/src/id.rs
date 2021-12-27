use std::fmt;

pub struct IdAllocator {
    next_id: Id,
}

impl IdAllocator {
    #[must_use]
    pub fn new() -> Self {
        Self { next_id: Id(0) }
    }

    #[must_use = "ignoring the return of this function will leak an ID slot and is almost never what you want"]
    pub fn allocate(&mut self) -> Id {
        let id_num = self.next_id.0;

        assert!(id_num < u16::MAX);

        std::mem::replace(&mut self.next_id, Id(id_num + 1))
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash, PartialOrd, Ord)]
pub struct Id(pub(crate) u16);

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}
