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
pub struct Deadending(Augmented);

impl_augmented_wrapper!(Deadending);

impl Deadending {
    // TODO: decide how to deal with cast wrap
    #[allow(clippy::cast_possible_wrap)]
    fn left_strong(&self) -> bool {
        (self.0.clone() + Augmented::n_waiting(-(self.birthday() as i64))).left_outcome()
            == Outcome::L
            && self.0.left_outcome() == Outcome::L
    }

    #[allow(clippy::cast_possible_wrap)]
    fn right_strong(&self) -> bool {
        (self.0.clone() + Augmented::n_waiting(self.birthday() as i64)).right_outcome()
            == Outcome::R
            && self.0.right_outcome() == Outcome::R
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simplest_form() {
        let g = Deadending::from_string("{-1|0,*}");
        let target = Deadending::from_string("{-1|0,s}");

        assert_eq!(g.canonical(), target);
    }
}
