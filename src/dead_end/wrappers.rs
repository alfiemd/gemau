use ref_cast::RefCast;
use std::cmp::Ordering;
use std::fmt;
use std::ops::Add;
use std::str::FromStr;

use super::form::DeadEnd;
use super::parse::ParseDeadEndError;

/// Left dead end wrapper.
///
/// The ordering is reversed compared to [`DeadEnd`] and [`RightDeadEnd`].
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[repr(transparent)]
#[derive(Clone, Default, RefCast, Debug)]
pub struct LeftDeadEnd(DeadEnd);

/// Right dead end wrapper.
///
/// The ordering is reversed compared to [`LeftDeadEnd`], but the same as [`DeadEnd`].
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[repr(transparent)]
#[derive(Clone, Default, RefCast, Debug)]
pub struct RightDeadEnd(DeadEnd);

macro_rules! impl_handed_wrapper {
    ($wrapper:ident) => {
        impl $wrapper {
            /// The zero game; see [`DeadEnd::ZERO`].
            pub const ZERO: Self = Self(DeadEnd::ZERO);

            /// See [`DeadEnd::new`].
            #[must_use]
            pub fn new() -> Self {
                Self(DeadEnd::new())
            }

            fn slice_from_inner(values: &[DeadEnd]) -> &[Self] {
                // SAFETY: `$wrapper` is `#[repr(transparent)]` over [`DeadEnd`], so slices have
                // identical layout.
                unsafe { std::slice::from_raw_parts(values.as_ptr().cast::<Self>(), values.len()) }
            }

            /// Borrows the wrapped [`DeadEnd`].
            #[must_use]
            pub fn as_inner(&self) -> &DeadEnd {
                &self.0
            }

            /// Mutably borrows the wrapped [`DeadEnd`].
            #[must_use]
            pub fn as_inner_mut(&mut self) -> &mut DeadEnd {
                &mut self.0
            }

            /// Extracts the wrapped [`DeadEnd`].
            #[must_use]
            pub fn into_inner(self) -> DeadEnd {
                self.0
            }

            /// Wraps a [`DeadEnd`].
            #[must_use]
            pub fn from_inner(value: DeadEnd) -> Self {
                Self(value)
            }

            /// See [`DeadEnd::with_options`].
            #[must_use]
            pub fn with_options(options: impl IntoIterator<Item = impl Into<Self>>) -> Self {
                Self(DeadEnd::with_options(options.into_iter().map(Into::into)))
            }

            /// See [`DeadEnd::with_options_unchecked`].
            #[must_use]
            pub fn with_options_unchecked(
                options: impl IntoIterator<Item = impl Into<Self>>,
            ) -> Self {
                Self(DeadEnd::with_options_unchecked(
                    options.into_iter().map(Into::into),
                ))
            }

            /// See [`DeadEnd::options`].
            #[must_use]
            pub fn options(&self) -> &[Self] {
                Self::slice_from_inner(self.0.options())
            }

            /// See [`DeadEnd::is_zero`].
            #[must_use]
            pub fn is_zero(&self) -> bool {
                self.0.is_zero()
            }

            /// See [`DeadEnd::options_iter`].
            pub fn options_iter(&self) -> std::slice::Iter<'_, Self> {
                self.options().iter()
            }

            /// See [`DeadEnd::isomorphic`].
            #[must_use]
            pub fn isomorphic(&self, other: &Self) -> bool {
                self.0.isomorphic(&other.0)
            }

            /// See [`DeadEnd::integer`].
            #[must_use]
            pub fn integer(rank: usize) -> Self {
                Self(DeadEnd::integer(rank))
            }

            /// See [`DeadEnd::waiting`].
            #[must_use]
            pub fn waiting(rank: usize) -> Self {
                Self(DeadEnd::waiting(rank))
            }

            /// See [`DeadEnd::is_waiting`].
            #[must_use]
            pub fn is_waiting(&self) -> Option<usize> {
                self.0.is_waiting()
            }

            /// See [`DeadEnd::is_integer`].
            #[must_use]
            pub fn is_integer(&self) -> Option<usize> {
                self.0.is_integer()
            }

            /// See [`DeadEnd::integer_part`].
            #[must_use]
            pub fn integer_part(&self) -> Option<(usize, &Self)> {
                self.0.integer_part().map(|(a, b)| (a, Self::ref_cast(b)))
            }

            /// See [`DeadEnd::flex`].
            #[must_use]
            pub fn flex(&self) -> usize {
                self.0.flex()
            }

            /// See [`DeadEnd::birth`].
            #[must_use]
            pub fn birth(&self) -> usize {
                self.0.birth()
            }

            /// See [`DeadEnd::race`].
            #[must_use]
            pub fn race(&self) -> usize {
                self.0.race()
            }

            /// See [`DeadEnd::term_lengths`].
            #[must_use]
            pub fn term_lengths(&self) -> Vec<usize> {
                self.0.term_lengths()
            }

            /// See [`DeadEnd::vertex_count`].
            #[must_use]
            pub fn vertex_count(&self) -> usize {
                self.0.vertex_count()
            }

            /// See [`DeadEnd::subposition_count`].
            #[must_use]
            pub fn subposition_count(&self) -> usize {
                self.0.subposition_count()
            }

            /// See [`DeadEnd::novel_factors`].
            #[must_use]
            pub fn novel_factors(&self) -> Vec<Self> {
                self.0.novel_factors().into_iter().map(Self).collect()
            }

            /// See [`DeadEnd::factors`].
            #[must_use]
            pub fn factors(&self) -> Vec<Self> {
                self.0.factors().into_iter().map(Self).collect()
            }

            /// See [`DeadEnd::is_atom`].
            #[must_use]
            pub fn is_atom(&self) -> bool {
                self.0.is_atom()
            }

            /// See [`DeadEnd::canonical`].
            #[must_use]
            pub fn canonical(&self) -> Self {
                Self(self.0.canonical())
            }

            /// See [`DeadEnd::good_options`].
            #[must_use]
            pub fn good_options(&self) -> Vec<&Self> {
                self.0
                    .good_options()
                    .into_iter()
                    .map(Self::ref_cast)
                    .collect()
            }

            /// See [`DeadEnd::unique_good_option`].
            #[must_use]
            pub fn unique_good_option(&self) -> Option<&Self> {
                self.0.unique_good_option().map(Self::ref_cast)
            }

            /// See [`DeadEnd::bound_length`].
            #[must_use]
            pub fn bound_length(&self) -> usize {
                self.0.bound_length()
            }
        }

        impl From<$wrapper> for DeadEnd {
            fn from(value: $wrapper) -> Self {
                value.0
            }
        }

        impl From<usize> for $wrapper {
            fn from(value: usize) -> Self {
                Self::integer(value)
            }
        }

        impl<'a> From<&'a DeadEnd> for &'a $wrapper {
            fn from(value: &'a DeadEnd) -> Self {
                $wrapper::ref_cast(value)
            }
        }

        impl<'a> From<&'a mut DeadEnd> for &'a mut $wrapper {
            fn from(value: &'a mut DeadEnd) -> Self {
                $wrapper::ref_cast_mut(value)
            }
        }

        impl From<DeadEnd> for $wrapper {
            fn from(value: DeadEnd) -> Self {
                $wrapper(value)
            }
        }

        impl<'a> From<&'a $wrapper> for &'a DeadEnd {
            fn from(value: &'a $wrapper) -> Self {
                &value.0
            }
        }

        impl<'a> From<&'a mut $wrapper> for &'a mut DeadEnd {
            fn from(value: &'a mut $wrapper) -> Self {
                &mut value.0
            }
        }

        impl Eq for $wrapper {}

        impl PartialEq for $wrapper {
            fn eq(&self, other: &Self) -> bool {
                self >= other && other >= self
            }
        }

        impl fmt::Display for $wrapper {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", self.0)
            }
        }

        impl FromStr for $wrapper {
            type Err = ParseDeadEndError;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                DeadEnd::from_str(s).map(Self)
            }
        }

        impl Add for &$wrapper {
            type Output = $wrapper;

            fn add(self, other: Self) -> Self::Output {
                $wrapper(&self.0 + &other.0)
            }
        }

        impl<Rhs> Add<Rhs> for $wrapper
        where
            Rhs: Into<$wrapper>,
        {
            type Output = Self;

            fn add(self, rhs: Rhs) -> Self::Output {
                let rhs = rhs.into();
                $wrapper(self.0 + rhs.0)
            }
        }
    };
}

impl_handed_wrapper!(LeftDeadEnd);
impl_handed_wrapper!(RightDeadEnd);

macro_rules! impl_convert_handed {
    ($from:ident => $to:ident) => {
        impl From<$from> for $to {
            fn from(value: $from) -> Self {
                $to(value.0)
            }
        }

        impl<'a> From<&'a $from> for &'a $to {
            fn from(value: &'a $from) -> Self {
                $to::ref_cast(value.as_inner())
            }
        }

        impl<'a> From<&'a mut $from> for &'a mut $to {
            fn from(value: &'a mut $from) -> Self {
                $to::ref_cast_mut(value.as_inner_mut())
            }
        }
    };
}

impl_convert_handed!(LeftDeadEnd => RightDeadEnd);
impl_convert_handed!(RightDeadEnd => LeftDeadEnd);

/// This order is reversed compared to [`DeadEnd`] and [`RightDeadEnd`].
impl PartialOrd for LeftDeadEnd {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // NOTE: reversed order to [`DeadEnd`]
        other.0.partial_cmp(&self.0)
    }

    fn ge(&self, other: &Self) -> bool {
        // NOTE: reversed order to [`DeadEnd`]
        other.0 >= self.0
    }
}

impl PartialOrd for RightDeadEnd {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }

    fn ge(&self, other: &Self) -> bool {
        self.0 >= other.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn game_comparison() {
        let g = LeftDeadEnd::integer(2);
        let h = LeftDeadEnd::waiting(2);

        assert!(g > h);

        let g = RightDeadEnd::integer(2);
        let h = RightDeadEnd::waiting(2);

        assert!(h > g);
    }
}
