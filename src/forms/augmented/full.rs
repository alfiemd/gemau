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
pub struct Full(Augmented);

impl_augmented_wrapper!(Full);

impl Full {
    fn left_strong(&self) -> bool {
        self.0.left_endlike()
    }

    fn right_strong(&self) -> bool {
        self.0.right_endlike()
    }
}
