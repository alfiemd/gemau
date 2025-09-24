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
pub struct Blocking(Augmented);

impl_augmented_wrapper!(Blocking);

impl Blocking {
    fn left_strong(&self) -> bool {
        self.0.left_endlike()
            || self.left().any(|g_l| {
                g_l.outcome() == Outcome::L
                    && g_l.left_strong()
                    && g_l.right().all(Self::left_strong)
            })
    }

    fn right_strong(&self) -> bool {
        self.0.right_endlike()
            || self.right().any(|g_r| {
                g_r.outcome() == Outcome::R
                    && g_r.right_strong()
                    && g_r.left().all(Self::right_strong)
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simplest_forms() {
        let b = Blocking::from_string("{|1,-1}");
        let target = Blocking::from_string("{|-1,s}");

        assert_eq!(b.canonical(), target);
    }

    #[test]
    fn test_strong() {
        let b = Blocking::from_string("{|1,-1}");
        assert!(b.right_strong());

        let b = Blocking::from_string("{|-1}");
        assert!(!b.right_strong());
    }

    #[test]
    fn test_debug_strings() {
        let mut b = Blocking::from_string("{|1,-1}");
        b = b.canonical();
        assert_eq!(format!("{b:?}"), "{|-1,\u{220E}}");

        let mut b = Blocking::from_string("{|1,*}");
        b = b.canonical();
        assert_eq!(format!("{b:?}"), "0");

        let mut b = Blocking::from_string("{|0,1,-1,*}");
        b = b.canonical();
        assert_eq!(format!("{b:?}"), "{|0,-1,\u{220E}}");
    }
}
