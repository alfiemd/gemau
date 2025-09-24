use std::cmp::Ordering;
use std::fmt;
use std::fmt::Debug;
use std::ops::Add;
use std::ops::Neg;
use std::ops::Sub;

use ref_cast::RefCast;

use super::augmented::Augmented;
use crate::Outcome;
use crate::ShortPartizan;
use crate::ShortRef;
use crate::forms::augmented::augmented::Tombstones;
use crate::impl_augmented_wrapper;

#[derive(RefCast, Clone)]
#[repr(transparent)]
pub struct Dicot(Augmented);

impl_augmented_wrapper!(Dicot);

impl Dicot {
    fn left_strong(&self) -> bool {
        self.left_tombstone() || self.left_outcome() == Outcome::L
    }

    fn right_strong(&self) -> bool {
        self.right_tombstone() || self.right_outcome() == Outcome::R
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simplest_form() {
        let g = Dicot::from_string("{0,*|*}").canonical();
        let target = Dicot::from_string("{s,0|*}");

        assert_eq!(g, target);
        assert_eq!(format!("{g:?}"), "{\u{220E},0|*}");
    }
}
