use std::cmp::Ordering;
use std::ops::Add;

/// A dead end represented by its set of options.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone)]
pub struct DeadEnd {
    /// The options of the [`DeadEnd`].
    options: Vec<DeadEnd>,
}

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
    /// assert!(g.is_zero());
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

    /// Returns whether `self` is the zero game.
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::DeadEnd;
    /// assert!(DeadEnd::ZERO.is_zero());
    /// assert!(!DeadEnd::integer(1).is_zero());
    /// ```
    #[must_use]
    pub fn is_zero(&self) -> bool {
        self.options.is_empty()
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

    /// Returns the rank of any waiting game equivalent to `self`.
    #[must_use]
    pub fn is_waiting(&self) -> Option<usize> {
        if let Some(a) = self.is_integer() {
            if a <= 1 {
                return Some(a);
            }
            return None;
        }

        let mut has_zero = false;
        let mut waiting_option: Option<(&Self, usize)> = None;

        for option in &self.options {
            if option.is_zero() {
                has_zero = true;
                continue;
            }

            if !self.is_dominant_option(option) {
                continue;
            }

            let Some(rank) = option.is_waiting() else {
                return None;
            };

            match waiting_option {
                None => waiting_option = Some((option, rank)),
                Some((existing, _)) if option == existing => {}
                Some(_) => return None,
            }
        }

        match (has_zero, waiting_option) {
            (true, Some((_, rank))) => Some(rank + 1),
            _ => None,
        }
    }

    /// Returns largest integer divisor as `(rank, counterpart)`.
    ///
    /// `rank` is the largest integer that fits into `self`, and `counterpart` is what remains
    /// after removing that integer part.
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::DeadEnd;
    /// let g = DeadEnd::with_options(0..2);
    ///
    /// assert_eq!(g.integer_part(), None);
    /// ```
    ///
    /// ```
    /// # use gemau::DeadEnd;
    /// let g = DeadEnd::integer(7);
    ///
    /// let (a, b) = g.integer_part().unwrap();
    /// assert_eq!(a, 7);
    /// assert_eq!(b, &DeadEnd::ZERO);
    /// ```
    #[must_use]
    pub fn integer_part(&self) -> Option<(usize, &Self)> {
        if self.is_zero() {
            return Some((0, self));
        }

        self.unique_good_option()?
            .integer_part()
            .map(|(a, b)| (a + 1, b))
    }

    /// Returns the rank if `self` is an integer.
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::DeadEnd;
    /// let g = DeadEnd::integer(4);
    /// assert_eq!(g.is_integer(), Some(4));
    /// ```
    ///
    /// ```
    /// # use gemau::DeadEnd;
    /// let g = DeadEnd::waiting(2);
    /// assert_eq!(g.is_integer(), None);
    /// ```
    #[must_use]
    pub fn is_integer(&self) -> Option<usize> {
        if self.is_zero() {
            return Some(0);
        }
        if self.options.len() > 1 {
            return None;
        }

        Some(self.options[0].is_integer()? + 1)
    }

    /// Returns the flexibility of the [`DeadEnd`].
    #[must_use]
    pub fn flex(&self) -> usize {
        if self.is_integer().is_some() {
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
        if self.is_zero() {
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
        if self.is_zero() {
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
        if self.is_zero() {
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
        if self.is_zero() {
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
        if self.is_zero() {
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
            if self.is_dominant_option(x) && !options.contains(x) {
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
            .filter(|&option| self.is_dominant_option(option))
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
        let mut first: Option<&Self> = None;

        for option in &self.options {
            if !self.is_dominant_option(option) {
                continue;
            }

            match first {
                None => first = Some(option),
                Some(existing) if option == existing => {}
                Some(_) => return None,
            }
        }

        first
    }

    fn is_dominant_option(&self, option: &Self) -> bool {
        !self.options.iter().any(|g| g > option)
    }

    /// Returns an upper bound on the length of factorisations.
    ///
    /// This uses several heuristics and is *not* always optimal.
    #[must_use]
    pub fn bound_length(&self) -> usize {
        if self.is_zero() {
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
        if other.is_zero() {
            return self.is_zero();
        }

        other
            .options()
            .iter()
            .all(|h| self.options().iter().any(|g| g >= h))
    }
}

impl Add for &DeadEnd {
    type Output = DeadEnd;

    fn add(self, other: Self) -> Self::Output {
        if self.is_zero() {
            return other.clone();
        }
        if other.is_zero() {
            return self.clone();
        }

        let mut options = Vec::with_capacity(self.options().len() + other.options().len());

        for g in self.options() {
            options.push(g + other);
        }
        for h in other.options() {
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
    fn add_integers_isomorphic() {
        let g = DeadEnd::integer(1) + DeadEnd::integer(2);

        assert!(g.isomorphic(&DeadEnd::integer(3)));
        assert_eq!(g.options(), vec![DeadEnd::integer(2)]);
    }

    #[test]
    fn integer_is_not_waiting_game() {
        assert_eq!(DeadEnd::integer(2).is_waiting(), None);
    }

    #[test]
    fn is_waiting_with_equal_good_options() {
        let g = DeadEnd::with_options_unchecked([
            DeadEnd::ZERO,
            DeadEnd::with_options(vec![DeadEnd::ZERO, DeadEnd::integer(1)]),
            DeadEnd::waiting(3),
        ]);
        assert_eq!(g.is_waiting(), Some(4));
    }

    #[test]
    fn waiting_game_needs_zero() {
        let g = DeadEnd::with_options_unchecked([DeadEnd::waiting(2), DeadEnd::waiting(3)]);
        assert_eq!(g.is_waiting(), None);
    }
}
