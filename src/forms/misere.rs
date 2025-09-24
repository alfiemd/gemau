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
pub struct Misere(Form);

impl_form_wrapper!(Misere);

impl Misere {
    fn left_outcome(&self) -> Outcome {
        if self.left().any(|x| x.right_outcome() == Outcome::L) || self.left().next().is_none() {
            return Outcome::L;
        }

        Outcome::R
    }

    fn right_outcome(&self) -> Outcome {
        if self.right().any(|x| x.left_outcome() == Outcome::R) || self.right().next().is_none() {
            return Outcome::R;
        }

        Outcome::L
    }

    fn left_proviso(&self, other: &Self) -> bool {
        other.left().next().is_some() || self.left().next().is_none()
    }

    fn right_proviso(&self, other: &Self) -> bool {
        self.right().next().is_some() || other.right().next().is_none()
    }

    /// Helper function for finding misère play remoteness values.
    fn find_remoteness<I, F>(iter: I, mapper: F) -> usize
    where
        I: Iterator,
        F: Fn(I::Item) -> usize,
    {
        let (min_odd, max_even, has_odd) =
            iter.fold((usize::MAX, 0, false), |(min_odd, max_even, has_odd), x| {
                let value = mapper(x);
                if value % 2 == 1 {
                    (min_odd.min(value), max_even, true)
                } else {
                    (min_odd, max_even.max(value), has_odd)
                }
            });

        if has_odd {
            return min_odd;
        }

        max_even
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

        if self.right().next().is_some() {
            rep = rep[..rep.len() - 1].to_string();
        }

        format!("{{{rep}}}")
    }
}

impl Debug for Misere {
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

impl PartialOrd for Misere {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self.ge(other), other.ge(self)) {
            (true, true) => Some(Ordering::Equal),
            (true, false) => Some(Ordering::Greater),
            (false, true) => Some(Ordering::Less),
            (false, false) => None,
        }
    }

    fn ge(&self, other: &Self) -> bool {
        self.left_maint(other)
            && self.right_maint(other)
            && self.left_proviso(other)
            && self.right_proviso(other)
    }
}
