use std::cmp::Ordering;
use std::fmt::Debug;
use std::fmt::{self};
use std::ops::Add;
use std::ops::Neg;
use std::ops::Sub;

use itertools::Itertools;
use ref_cast::RefCast;
use serde::Deserialize;
use serde::Serialize;

use super::form::Form;
use crate::core::ShortPartizan;
use crate::core::ShortRef;
use crate::impl_form_wrapper;
use crate::outcome::Outcome;

#[derive(RefCast, Clone, Serialize, Deserialize)]
#[repr(transparent)]
pub struct Normal(Form);

impl_form_wrapper!(Normal);

impl Normal {
    fn left_outcome(&self) -> Outcome {
        if self.left().any(|x| x.right_outcome() == Outcome::L) {
            return Outcome::L;
        }

        Outcome::R
    }

    fn right_outcome(&self) -> Outcome {
        if self.right().any(|x| x.left_outcome() == Outcome::R) {
            return Outcome::R;
        }

        Outcome::L
    }

    /// Helper function for finding normal play remoteness values.
    fn find_remoteness<I, F>(iter: I, mapper: F) -> usize
    where
        I: Iterator,
        F: Fn(I::Item) -> usize,
    {
        let (min_even, max_odd, has_even) = iter.fold(
            (usize::MAX, 0, false),
            |(min_even, max_odd, has_even), x| {
                let value = mapper(x);
                if value % 2 == 0 {
                    (min_even.min(value), max_odd, true)
                } else {
                    (min_even, max_odd.max(value), has_even)
                }
            },
        );

        if has_even {
            return min_even;
        }

        max_odd
    }

    #[must_use]
    pub fn sensible_fmt(&self) -> String {
        if let Some(i) = self.is_integer() {
            return format!("{i}");
        }

        if let Some(i) = self.is_star() {
            if i == 1 {
                return "*".to_string();
            }
            return format!("*{i}");
        }

        let mut rep = String::new();

        let left_options: String = self
            .left()
            .map(|g_l| format!("{},", g_l.sensible_fmt()))
            .sorted_unstable()
            .collect::<_>();

        rep.push_str(&left_options);

        //for g in self.left() {
        //    rep.push_str(&format!("{g:?},"));
        //}
        if self.left().next().is_some() {
            rep = rep[..rep.len() - 1].to_string();
        }

        rep.push('|');

        let right_options: String = self
            .right()
            .map(|g_r| format!("{},", g_r.sensible_fmt()))
            .sorted_unstable()
            .collect::<_>();

        rep.push_str(&right_options);

        //for g in self.right() {
        //    rep.push_str(&format!("{g:?},"));
        //}
        if self.right().next().is_some() {
            rep = rep[..rep.len() - 1].to_string();
        }

        format!("{{{rep}}}")
    }
}

impl PartialOrd for Normal {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self.ge(other), other.ge(self)) {
            (true, true) => Some(Ordering::Equal),
            (true, false) => Some(Ordering::Greater),
            (false, true) => Some(Ordering::Less),
            (false, false) => None,
        }
    }

    fn ge(&self, other: &Self) -> bool {
        self.left_maint(other) && self.right_maint(other)
    }
}

impl Debug for Normal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(i) = self.is_integer() {
            return write!(f, "{i}");
        }

        if let Some(i) = self.is_star() {
            if i == 1 {
                return write!(f, "*");
            }
            return write!(f, "*{i}");
        }

        let mut rep = String::new();

        for g in self.left() {
            rep.push_str(&format!("{g:?},"));
        }
        if self.left().next().is_some() {
            rep = rep[..rep.len() - 1].to_string();
        }

        rep.push('|');

        for g in self.right() {
            rep.push_str(&format!("{g:?},"));
        }
        if self.right().next().is_some() {
            rep = rep[..rep.len() - 1].to_string();
        }

        write!(f, "{{{rep}}}")
    }
}
