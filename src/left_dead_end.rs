use itertools::Itertools;
use ref_cast::RefCast;
use std::cmp::Ordering;
use std::fmt;
use std::ops::Add;

/// A dead end represented by its set of options.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone)]
pub struct DeadEnd {
    /// The options of the [`DeadEnd`].
    options: Vec<DeadEnd>,
}

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

impl Default for DeadEnd {
    fn default() -> Self {
        Self::ZERO
    }
}

/// Converts a non-negative rank into the corresponding integer [`DeadEnd`].
impl From<usize> for DeadEnd {
    fn from(value: usize) -> Self {
        Self::integer(value)
    }
}

impl DeadEnd {
    /// The zero game.
    pub const ZERO: Self = Self { options: vec![] };

    /// Constructs a new [`DeadEnd`] with no options; i.e. the zero game.
    ///
    /// This is equivalent to [`Self::ZERO`] and [`Self::default`].
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new [`DeadEnd`] from options, deduplicating isomorphic copies.
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::DeadEnd;
    /// let g = DeadEnd::with_options(0..2);
    ///
    /// assert_eq!(g.options(), vec![DeadEnd::ZERO, DeadEnd::integer(1)]);
    /// ```
    pub fn with_options(options: impl IntoIterator<Item = impl Into<Self>>) -> Self {
        Self::from_options(options.into_iter().map(Into::into).collect())
    }

    /// Creates a new [`DeadEnd`] from options without checking for isomorphic duplicates.
    ///
    /// This is faster than [`Self::with_options`] but assumes the caller already knows that there
    /// are no isomorphic duplicates.
    pub fn with_options_unchecked(options: impl IntoIterator<Item = impl Into<Self>>) -> Self {
        Self::from_options_unchecked(options.into_iter().map(Into::into).collect())
    }

    fn from_options(options: Vec<Self>) -> Self {
        let mut deduped = Vec::with_capacity(options.len());

        for option in options {
            if !deduped
                .iter()
                .any(|existing: &Self| existing.isomorphic(&option))
            {
                deduped.push(option);
            }
        }

        // NOTE: options are pairwise non-isomorphic by construction
        Self::from_options_unchecked(deduped)
    }

    fn from_options_unchecked(options: Vec<Self>) -> Self {
        Self { options }
    }

    /// Returns a borrowed slice of the options of the [`DeadEnd`].
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::DeadEnd;
    /// let g = DeadEnd::ZERO;
    /// assert!(g.options().is_empty());
    /// ```
    ///
    /// ```
    /// # use gemau::DeadEnd;
    /// let g = DeadEnd::integer(4);
    /// let h = DeadEnd::integer(3);
    /// assert_eq!(g.options(), vec![h]);
    /// ```
    #[must_use]
    pub fn options(&self) -> &[Self] {
        &self.options
    }

    /// Returns an iterator over the options of the [`DeadEnd`].
    pub fn options_iter(&self) -> std::slice::Iter<'_, Self> {
        self.options.iter()
    }

    /// Returns whether two [`DeadEnd`] objects are isomorphic.
    ///
    /// Two games are isomorphic when every option of each game is isomorphic to some option of the
    /// other game.
    #[must_use]
    pub fn isomorphic(&self, other: &Self) -> bool {
        self.options
            .iter()
            .all(|g| other.options.iter().any(|h| g.isomorphic(h)))
            && other
                .options
                .iter()
                .all(|h| self.options.iter().any(|g| g.isomorphic(h)))
    }

    /// Returns the strict form of an integer of a given rank.
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::DeadEnd;
    /// let g = DeadEnd::ZERO;
    /// let h = DeadEnd::integer(0);
    /// assert_eq!(g, h);
    /// ```
    ///
    /// ```
    /// # use gemau::DeadEnd;
    ///
    /// let g = DeadEnd::integer(1);
    /// let h = DeadEnd::integer(2);
    /// assert_eq!(&g + &g, h);
    /// ```
    #[must_use]
    pub fn integer(rank: usize) -> Self {
        if rank == 0 {
            return Self::ZERO;
        }
        Self::from_options_unchecked(vec![Self::integer(rank - 1)])
    }

    /// Returns the *waiting game* of the given rank. This has also been called the *perfect
    /// murder*.
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::DeadEnd;
    /// let g = DeadEnd::waiting(1);
    /// let h = DeadEnd::integer(1);
    /// assert_eq!(g, h);
    /// ```
    ///
    /// ```
    /// # use gemau::DeadEnd;
    /// let g = DeadEnd::ZERO;
    /// let h = DeadEnd::integer(1);
    /// let k = DeadEnd::waiting(2);
    /// assert_eq!(k.options(), vec![g, h]);
    /// ```
    #[must_use]
    pub fn waiting(rank: usize) -> Self {
        if rank == 0 {
            return Self::ZERO;
        }
        if rank == 1 {
            return Self::integer(1);
        }
        Self::from_options_unchecked(vec![Self::ZERO, Self::waiting(rank - 1)])
    }

    /// Returns `(rank, is_waiting)` for canonical-form recognition of waiting games.
    ///
    /// If `is_waiting` is `true`, then `rank` is the rank of the waiting game.
    #[must_use]
    pub fn is_waiting(&self) -> (usize, bool) {
        let (a, _, c) = self.is_integer();
        if c && a <= 1 {
            return (a, true);
        }

        if self.options.len() != 2 {
            return (0, false);
        }

        if self.options[0].options.is_empty() {
            let (a, b) = self.options[1].is_waiting();
            if b {
                return (a + 1, b);
            }
        }
        if self.options[1].options.is_empty() {
            let (a, b) = self.options[0].is_waiting();
            if b {
                return (a + 1, b);
            }
        }

        (0, false)
    }

    /// Returns integer decomposition as `(rank, counterpart, is_integer)`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::DeadEnd;
    /// let g = DeadEnd::integer(3);
    /// let h = DeadEnd::waiting(2);
    /// let k = (&g + &h).canonical();
    ///
    /// let (a, b, c) = k.is_integer();
    /// assert_eq!(a, 3);
    /// assert_eq!(b, &h);
    /// assert!(!c);
    /// ```
    ///
    /// ```
    /// # use gemau::DeadEnd;
    /// let g = DeadEnd::integer(7);
    ///
    /// let (a, b, c) = g.is_integer();
    /// assert_eq!(a, 7);
    /// assert_eq!(b, &DeadEnd::ZERO);
    /// assert!(c);
    /// ```
    #[must_use]
    pub fn is_integer(&self) -> (usize, &Self, bool) {
        if self.options.is_empty() {
            return (0, self, true);
        }
        if self.options.len() > 1 {
            return (0, self, false);
        }
        let (a, b, c) = self.options[0].is_integer();
        (a + 1, b, c)
    }

    /// Returns the flexibility of the [`DeadEnd`].
    #[must_use]
    pub fn flex(&self) -> usize {
        if self.is_integer().2 {
            return 0;
        }

        self.options
            .iter()
            .map(Self::flex)
            .max()
            .expect("flex called with non-empty options")
            + 1
    }

    /// Returns the birthday of the [`DeadEnd`].
    #[must_use]
    pub fn birth(&self) -> usize {
        if self.options.is_empty() {
            return 0;
        }

        self.options
            .iter()
            .map(Self::birth)
            .max()
            .expect("birth called with non-empty options")
            + 1
    }

    /// Returns the race of the [`DeadEnd`].
    #[must_use]
    pub fn race(&self) -> usize {
        if self.options.is_empty() {
            return 0;
        }

        self.options
            .iter()
            .map(Self::race)
            .min()
            .expect("race called with non-empty options")
            + 1
    }

    /// Returns the terminal lengths of the [`DeadEnd`].
    ///
    /// Values are deduplicated but not sorted. Note that `min(self.term_lengths()) == self.race()`
    /// and `max(self.term_lengths()) == self.birth()`.
    #[must_use]
    pub fn term_lengths(&self) -> Vec<usize> {
        if self.options.is_empty() {
            return vec![0];
        }

        let mut lengths = Vec::new();
        for option in &self.options {
            for length in option.term_lengths() {
                let next = length + 1;
                if !lengths.contains(&next) {
                    lengths.push(next);
                }
            }
        }

        lengths
    }

    /// Returns the novel factors of the [`DeadEnd`].
    ///
    /// If a sum `g = h + k` is *novel*, then, without loss of generality, `h` appears in every
    /// factorisation of `g`. But `k`, the counterpart to `h`, is not necessarily in the factors of
    /// the options of `g`, and so it must be checked separately.
    #[must_use]
    pub fn novel_factors(&self) -> Vec<Self> {
        if self.options.is_empty() {
            return vec![];
        }

        let mut options_factors = vec![];

        for r in &self.options {
            options_factors.push(r.factors());
        }

        let mut novels = vec![];

        'outer: for g in &options_factors[0] {
            for d in options_factors.iter().skip(1) {
                if !d.contains(g) {
                    continue 'outer;
                }
            }
            novels.push(g.clone());
        }

        let mut new_factors = vec![];

        for n in &novels {
            let mut counterparts = vec![];

            'outer: for (i, d) in options_factors.iter().enumerate() {
                for cand in d {
                    if n + cand == self.options[i] {
                        counterparts.push(cand.clone());
                        continue 'outer;
                    }
                }
            }

            let counter = Self::from_options(counterparts);

            if n + &counter == *self {
                if !new_factors.contains(n) {
                    new_factors.push(n.clone());
                }
                if !new_factors.contains(&counter) {
                    new_factors.push(counter);
                }
            }
        }

        new_factors
    }

    /// Returns the factors of the [`DeadEnd`].
    ///
    /// They are *not* guaranteed to be in canonical form.
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::DeadEnd;
    /// let g = DeadEnd::waiting(2) + 1;
    ///
    /// let factors = g.factors();
    ///
    /// assert_eq!(factors, vec![
    ///     DeadEnd::integer(1),
    ///     DeadEnd::waiting(2),
    ///     DeadEnd::waiting(2) + 1,
    ///     DeadEnd::ZERO
    /// ]);
    /// ```
    ///
    /// ```
    /// # use gemau::DeadEnd;
    /// let g = DeadEnd::integer(3);
    ///
    /// let factors = g.factors();
    ///
    /// assert_eq!(factors, vec![
    ///     DeadEnd::integer(1),
    ///     DeadEnd::integer(2),
    ///     DeadEnd::integer(3),
    ///     DeadEnd::ZERO
    /// ]);
    /// ```
    #[must_use]
    pub fn factors(&self) -> Vec<Self> {
        if self.options.is_empty() {
            return vec![Self::ZERO];
        }

        let mut candidates = vec![];

        for r in &self.options {
            let div = r.factors();
            for d in div {
                if !candidates.contains(&d) {
                    candidates.push(d.clone());
                }
            }
        }

        let mut factors = vec![];

        for i in 0..candidates.len() {
            for j in i..candidates.len() {
                if &candidates[i] + &candidates[j] == *self {
                    factors.push(candidates[i].clone());
                    if i != j {
                        factors.push(candidates[j].clone());
                    }
                }
            }
        }

        if !factors.contains(self) {
            factors.push(self.clone());
        }
        if !factors.contains(&Self::ZERO) {
            factors.push(Self::ZERO);
        }

        for n in &self.novel_factors() {
            if !factors.contains(n) {
                factors.push(n.clone());
            }
        }

        factors
    }

    /// Returns whether the [`DeadEnd`] is an atom; i.e. if it has precisely two factors.
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::DeadEnd;
    /// let g = DeadEnd::ZERO;
    /// assert!(!g.is_atom());
    ///
    /// let g = DeadEnd::integer(1);
    /// assert!(g.is_atom());
    /// ```
    #[must_use]
    pub fn is_atom(&self) -> bool {
        self.factors().len() == 2
    }

    /// Returns the canonical (simplest) form of the [`DeadEnd`].
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::DeadEnd;
    /// let g = DeadEnd::integer(1);
    /// let h = DeadEnd::waiting(2);
    /// let k = DeadEnd::with_options(vec![g.clone(), h.clone()]);
    ///
    /// assert_eq!(k.canonical().options(), vec![h]);
    /// ```
    #[must_use]
    pub fn canonical(&self) -> Self {
        let mut options = Vec::new();
        for x in &self.options {
            if !self.options.iter().any(|y| y > x) && !options.contains(x) {
                options.push(x.canonical());
            }
        }

        // NOTE: options are pairwise non-equivalent, so cannot be isomorphic
        Self::from_options_unchecked(options)
    }

    /// Returns the good options of the [`DeadEnd`].
    ///
    /// There are no isomorphic duplicates, but there may be equivalent good options.
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::DeadEnd;
    /// let g = DeadEnd::integer(1);
    /// let h = DeadEnd::waiting(2);
    /// let k = DeadEnd::with_options(vec![g.clone(), h.clone()]);
    ///
    /// assert_eq!(k.good_options(), vec![&h]);
    /// ```
    pub fn good_options(&self) -> Vec<&Self> {
        self.options
            .iter()
            .filter(|&h| !self.options.iter().any(|k| k > h))
            .collect()
    }

    /// Returns the unique good option (up to equivalence), if it exists.
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::DeadEnd;
    /// let g = DeadEnd::integer(1);
    /// let h = DeadEnd::waiting(2);
    /// let k = DeadEnd::with_options(vec![g.clone(), h.clone()]);
    ///
    /// assert_eq!(k.unique_good_option(), Some(&h));
    ///
    /// let g = DeadEnd::with_options(0..2);
    /// assert_eq!(g.unique_good_option(), None);
    /// ```
    pub fn unique_good_option(&self) -> Option<&Self> {
        let good_options = self.good_options();
        let first = *good_options.first()?;

        if good_options.iter().skip(1).all(|g| *g == first) {
            return Some(first);
        }

        None
    }

    /// Returns an upper bound on the length of factorisations.
    ///
    /// This uses several heuristics and is *not* always optimal.
    #[must_use]
    pub fn bound_length(&self) -> usize {
        if self.options.is_empty() {
            return 0;
        }

        if let Some(good_option) = self.unique_good_option() {
            return 1 + good_option.bound_length();
        }

        let mut min_bound = self.race();
        if min_bound == 1 {
            return 1;
        }

        let flex_bound = self.flex().div_ceil(2);
        if flex_bound == 1 {
            return 1;
        }
        min_bound = min_bound.min(flex_bound);

        let term_len_bound = self.term_lengths().len() - 1;
        if term_len_bound == 1 {
            return 1;
        }
        min_bound = min_bound.min(term_len_bound);

        let mut min_option_bound = usize::MAX;
        for option in self.good_options() {
            let bound = option.bound_length();
            // Implementation detail: in this branch, `race() > 1`, so no option can
            // have bound 0. Therefore if any option has bound 1, the recursive bound
            // is exactly 2.
            if bound == 1 {
                return 2;
            }
            min_option_bound = min_option_bound.min(bound);
        }
        let option_bound = min_option_bound + 1;

        min_bound.min(option_bound)
    }
}

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
            pub fn is_waiting(&self) -> (usize, bool) {
                self.0.is_waiting()
            }

            /// See [`DeadEnd::is_integer`].
            #[must_use]
            pub fn is_integer(&self) -> (usize, &Self, bool) {
                let (a, b, c) = self.0.is_integer();
                (a, Self::ref_cast(b), c)
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

/// Formats integers and canonical waiting games specially.
impl fmt::Display for DeadEnd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (a, b, c) = self.is_integer();
        if c {
            return write!(f, "{a}");
        }
        if a > 0 {
            return write!(f, "{a}+{b}");
        }

        let (a, b) = self.is_waiting();
        if b {
            return write!(f, "W_{a}");
        }

        let rep = self.options.iter().map(|g| format!("{g}")).join(",");
        write!(f, "{{{rep}}}")
    }
}

/// Formats integers specially.
impl fmt::Debug for DeadEnd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (a, b, c) = self.is_integer();
        if c {
            return write!(f, "{a}");
        }
        if a > 0 {
            return write!(f, "{a}+{b:?}");
        }

        let rep = self.options.iter().map(|g| format!("{g:?}")).join(",");
        write!(f, "{{{rep}}}")
    }
}

impl Eq for DeadEnd {}

impl PartialEq for DeadEnd {
    fn eq(&self, other: &Self) -> bool {
        self >= other && other >= self
    }
}

impl PartialOrd for DeadEnd {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self >= other, other >= self) {
            (true, true) => Some(Ordering::Equal),
            (true, _) => Some(Ordering::Greater),
            (_, true) => Some(Ordering::Less),
            _ => None,
        }
    }

    fn ge(&self, other: &Self) -> bool {
        if other.options.is_empty() {
            return self.options.is_empty();
        }

        other
            .options
            .iter()
            .all(|h| self.options.iter().any(|g| g >= h))
    }
}

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

impl Add for &DeadEnd {
    type Output = DeadEnd;

    fn add(self, other: Self) -> Self::Output {
        if self.options.is_empty() {
            return other.clone();
        }
        if other.options.is_empty() {
            return self.clone();
        }

        let mut options = Vec::with_capacity(self.options.len() + other.options.len());

        for g in &self.options {
            options.push(g + other);
        }
        for h in &other.options {
            options.push(self + h);
        }

        DeadEnd::from_options(options)
    }
}

impl<Rhs> Add<Rhs> for DeadEnd
where
    Rhs: Into<DeadEnd>,
{
    type Output = Self;

    fn add(self, rhs: Rhs) -> Self::Output {
        let other = rhs.into();
        &self + &other
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn birthday() {
        assert_eq!(DeadEnd::ZERO.birth(), 0);
        assert_eq!(DeadEnd::integer(3).birth(), 3);
        assert_eq!((DeadEnd::waiting(2) + DeadEnd::waiting(3)).birth(), 5);
    }

    #[test]
    fn race() {
        assert_eq!(DeadEnd::ZERO.race(), 0);
        assert_eq!(DeadEnd::integer(3).race(), 3);
        assert_eq!((DeadEnd::waiting(2) + DeadEnd::waiting(3)).race(), 2);
    }

    #[test]
    fn flex() {
        assert_eq!(DeadEnd::ZERO.flex(), 0);
        assert_eq!(DeadEnd::integer(3).flex(), 0);
        assert_eq!((DeadEnd::waiting(2) + DeadEnd::waiting(3)).flex(), 4);
    }

    #[test]
    fn bound_length() {
        let g = DeadEnd::with_options(0..2);
        assert_eq!(g.bound_length(), 1);

        let g =
            DeadEnd::with_options_unchecked([1, 2, 3]) + DeadEnd::with_options_unchecked([1, 2, 3]);
        assert_eq!(
            [g.race(), g.flex().div_ceil(2), g.term_lengths().len() - 1,],
            [4, 3, 4]
        );
    }

    #[test]
    fn add_integers_isomorphic() {
        let g = DeadEnd::integer(1) + DeadEnd::integer(2);

        assert!(g.isomorphic(&DeadEnd::integer(3)));
        assert_eq!(g.options(), vec![DeadEnd::integer(2)]);
    }

    #[test]
    fn isomorphic_options() {
        let g = DeadEnd::with_options([
            DeadEnd::integer(1),
            DeadEnd::waiting(1),
            DeadEnd::integer(1),
        ]);

        assert_eq!(g.options(), vec![DeadEnd::integer(1)]);
    }

    #[test]
    fn unchecked_isomorphic_options() {
        let g = DeadEnd::with_options_unchecked([DeadEnd::integer(1), DeadEnd::waiting(1)]);

        assert_eq!(g.options().len(), 2);
        assert!(g.options()[0].isomorphic(&g.options()[1]));
    }

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
