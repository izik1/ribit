use std::cmp;

use crate::CmpKind;

pub trait PartialICmp {
    fn partial_unsigned_cmp(&self, other: &Self) -> Option<cmp::Ordering>;
    fn partial_signed_cmp(&self, other: &Self) -> Option<cmp::Ordering>;

    fn partial_cmp_with(&self, other: &Self, comparer: CmpKind) -> Option<bool> {
        match comparer {
            CmpKind::Eq => self.partial_unsigned_cmp(other).map(cmp::Ordering::is_eq),
            CmpKind::Ne => self.partial_unsigned_cmp(other).map(cmp::Ordering::is_ne),
            CmpKind::Inequality(inequality) => {
                let order = match inequality.is_signed() {
                    true => self.partial_signed_cmp(other)?,
                    false => self.partial_unsigned_cmp(other)?,
                };

                match (inequality.is_less(), order) {
                    (false, cmp::Ordering::Less) => Some(true),
                    (true, cmp::Ordering::Greater) => Some(true),
                    (_, cmp::Ordering::Equal) if inequality.include_eq() => Some(true),
                    _ => Some(false),
                }
            }
        }
    }
}

pub trait ICmp: PartialICmp {
    fn unsigned_cmp(&self, other: &Self) -> cmp::Ordering;
    fn signed_cmp(&self, other: &Self) -> cmp::Ordering;

    fn cmp_with(&self, other: &Self, comparer: CmpKind) -> bool {
        match comparer {
            CmpKind::Eq => self.unsigned_cmp(other).is_eq(),
            CmpKind::Ne => self.unsigned_cmp(other).is_ne(),
            CmpKind::Inequality(inequality) => {
                let order = match inequality.is_signed() {
                    true => self.signed_cmp(other),
                    false => self.unsigned_cmp(other),
                };

                match (inequality.is_less(), order) {
                    (false, cmp::Ordering::Less) => true,
                    (true, cmp::Ordering::Greater) => true,
                    (_, cmp::Ordering::Equal) if inequality.include_eq() => true,
                    _ => false,
                }
            }
        }
    }
}

impl ICmp for bool {
    fn unsigned_cmp(&self, other: &Self) -> cmp::Ordering {
        self.cmp(other)
    }

    // fixme: this makes much more sense if `Bool` is actually an i1
    fn signed_cmp(&self, other: &Self) -> cmp::Ordering {
        // `true` -> `-1`, which is less than `0`.
        self.cmp(other).reverse()
    }
}

impl PartialICmp for bool {
    fn partial_unsigned_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.unsigned_cmp(other))
    }

    fn partial_signed_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.signed_cmp(other))
    }

    fn partial_cmp_with(&self, other: &Self, comparer: CmpKind) -> Option<bool> {
        Some(self.cmp_with(other, comparer))
    }
}
