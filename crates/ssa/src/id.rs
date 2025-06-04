use std::fmt;

pub struct IdAllocator {
    next_id: Id,
}

impl IdAllocator {
    #[must_use]
    pub fn new() -> Self {
        Self::with_start(Id(0))
    }

    #[must_use]
    pub fn with_start(start: Id) -> Self {
        Self { next_id: start }
    }

    #[must_use = "ignoring the return of this function will leak an ID slot and is almost never what you want"]
    pub fn allocate(&mut self) -> Id {
        let id_num = self.next_id.0;

        let new_id = id_num.checked_add(1).expect("ID overflow");

        std::mem::replace(&mut self.next_id, Id(new_id))
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash, PartialOrd, Ord)]
pub struct Id(pub(crate) u16);

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}
