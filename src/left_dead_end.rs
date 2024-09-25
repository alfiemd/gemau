use itertools::FoldWhile::{Continue, Done};
use itertools::Itertools;
use std::cmp::Ordering;
use std::fmt;
use std::ops::Add;

/// Represents a Left dead end; it is identified with its set of options.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone)]
pub struct LeftDeadEnd {
    /// The (Right) options of the Left dead end.
    options: Vec<LeftDeadEnd>,
}

impl Default for LeftDeadEnd {
    fn default() -> Self {
        LeftDeadEnd::ZERO
    }
}

/// Convert a [`usize`] into what would be the canonical form of a (negative) integer in normal
/// play; i.e. an integer Left dead end.
impl From<usize> for LeftDeadEnd {
    fn from(value: usize) -> Self {
        LeftDeadEnd::integer(value)
    }
}

impl LeftDeadEnd {
    /// The zero game.
    pub const ZERO: LeftDeadEnd = LeftDeadEnd { options: vec![] };

    /// Constructs a new [`LeftDeadEnd`] with no options; i.e. the zero game. This is the same as
    /// [`LeftDeadEnd::default()`] and [`LeftDeadEnd::ZERO`].
    #[must_use]
    pub fn new() -> Self {
        LeftDeadEnd::default()
    }

    /// Creates a new [`LeftDeadEnd`] given an iterable of elements that can each be converted into
    /// a [`LeftDeadEnd`].
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::LeftDeadEnd;
    /// let g = LeftDeadEnd::with_options(0..2);
    ///
    /// assert_eq!(g.options(), vec![LeftDeadEnd::ZERO, LeftDeadEnd::integer(1)]);
    /// ```
    pub fn with_options(options: impl IntoIterator<Item = impl Into<LeftDeadEnd>>) -> Self {
        Self {
            options: options.into_iter().map(Into::into).collect(),
        }
    }

    /// Returns a borrowed slice to the options of the [`LeftDeadEnd`].
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::LeftDeadEnd;
    /// let g = LeftDeadEnd::ZERO;
    /// assert!(g.options().is_empty());
    /// ```
    ///
    /// ```
    /// # use gemau::LeftDeadEnd;
    /// let g = LeftDeadEnd::integer(4);
    /// let h = LeftDeadEnd::integer(3);
    /// assert_eq!(g.options(), vec!(h));
    /// ```
    #[must_use]
    pub fn options(&self) -> &[Self] {
        &self.options
    }

    /// Returns an iterator over the options of the [`LeftDeadEnd`].
    pub fn options_iter(&self) -> std::slice::Iter<'_, LeftDeadEnd> {
        self.options.iter()
    }

    /// Return the novel factors of a [`LeftDeadEnd`].
    ///
    /// If a sum `g = h + k` is *novel*, then, without loss of generality, `h` appears in every
    /// factorisation of `g`. But `k`, the counterpart to `h`, is not necessarily in the
    /// factors of the options of `g`, and so we have to build and check for it separately.
    ///
    /// As of writing, no Left dead ends are known *not* to be uniquely factorisable, and so
    /// the output will likely be identical to [`LeftDeadEnd::factors`].
    #[must_use]
    pub fn novel_factors(&self) -> Vec<LeftDeadEnd> {
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

            let counter = LeftDeadEnd {
                options: counterparts,
            };

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

    /// Return the factors of the [`LeftDeadEnd`]. They will *not* be in canonical form when
    /// returned.
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::LeftDeadEnd;
    /// let g = LeftDeadEnd::waiting(2) + 1;
    ///
    /// let factors = g.factors();
    ///
    /// assert_eq!(factors, vec![LeftDeadEnd::integer(1), LeftDeadEnd::waiting(2),
    /// LeftDeadEnd::waiting(2) + 1, LeftDeadEnd::ZERO]);
    /// ```
    ///
    /// ```
    /// # use gemau::LeftDeadEnd;
    /// let g = LeftDeadEnd::integer(3);
    ///
    /// let factors = g.factors();
    ///
    /// assert_eq!(factors, vec![LeftDeadEnd::integer(1), LeftDeadEnd::integer(2),
    /// LeftDeadEnd::integer(3), LeftDeadEnd::ZERO]);
    #[must_use]
    pub fn factors(&self) -> Vec<LeftDeadEnd> {
        if self.options.is_empty() {
            return vec![LeftDeadEnd::ZERO];
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
        if !factors.contains(&LeftDeadEnd::ZERO) {
            factors.push(LeftDeadEnd::ZERO);
        }

        for n in &self.novel_factors() {
            if !factors.contains(n) {
                factors.push(n.clone());
            }
        }

        factors
    }

    /// Returns whether or not the [`LeftDeadEnd`] is an atom; i.e. if it has precisely two
    /// factors.
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::LeftDeadEnd;
    /// let g = LeftDeadEnd::ZERO;
    /// assert!(!g.is_atom());
    ///
    /// let g = LeftDeadEnd::integer(1);
    /// assert!(g.is_atom());
    /// ```
    #[must_use]
    pub fn is_atom(&self) -> bool {
        self.factors().len() == 2
    }

    /// Returns the strict form of a negative integer of given rank.
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::LeftDeadEnd;
    /// let g = LeftDeadEnd::ZERO;
    /// let h = LeftDeadEnd::integer(0);
    /// assert_eq!(g, h);
    /// ```
    ///
    /// ```
    /// # use gemau::LeftDeadEnd;
    /// // The same form as the canonical form of -1 in normal play.
    /// let g = LeftDeadEnd::integer(1);
    /// // The same form as the canonical form of -2 in normal play.
    /// let h = LeftDeadEnd::integer(2);
    /// assert_eq!(&g + &g, h);
    /// ```
    #[must_use]
    pub fn integer(rank: usize) -> Self {
        if rank == 0 {
            return Self::ZERO;
        }
        Self {
            options: vec![Self::integer(rank - 1)],
        }
    }

    /// Returns the waiting game of given rank. Sometimes also called the perfect murder.
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::LeftDeadEnd;
    /// let g = LeftDeadEnd::waiting(1);
    /// let h = LeftDeadEnd::integer(1);
    /// assert_eq!(g, h);
    /// ```
    ///
    /// ```
    /// # use gemau::LeftDeadEnd;
    /// let g = LeftDeadEnd::ZERO;
    /// let h = LeftDeadEnd::integer(1);
    /// let k = LeftDeadEnd::waiting(2);
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
        LeftDeadEnd {
            options: vec![Self::ZERO, Self::waiting(rank - 1)],
        }
    }

    /// Returns the canonical (simplest) form of the [`LeftDeadEnd`].
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::LeftDeadEnd;
    /// let g = LeftDeadEnd::integer(1);
    /// let h = LeftDeadEnd::waiting(2);
    /// let k = LeftDeadEnd::with_options(vec![g.clone(), h.clone()]);
    ///
    /// // The waiting game dominates the integer here.
    /// assert_eq!(k.canonical().options(), vec![h]);
    /// ```
    #[must_use]
    pub fn canonical(&self) -> Self {
        LeftDeadEnd {
            options: self.options.iter().fold(Vec::new(), |mut acc, x| {
                if !self.options.iter().any(|y| y < x) && !acc.contains(x) {
                    acc.push(x.canonical());
                }
                acc
            }),
        }
    }

    /// Returns whether or not the [`LeftDeadEnd`] is a **canonical form** waiting game in the form
    /// of a tuple `(a, b)`, where `a` respresents the rank of the waiting game (if it is one), and
    /// `b` represents whether or not it is a waiting game.
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

    /// Returns a 3-tuple `(a, b, c)`, where `a` is the maximum rank of an integer that divides the
    /// [`LeftDeadEnd`], `b` is the counterpart to the integer in the factorisation of the game,
    /// and `c` represents whether the game is an integer.
    ///
    /// This function expects the [`LeftDeadEnd`] to be in **canonical form**!
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::LeftDeadEnd;
    /// let g = LeftDeadEnd::integer(3);
    /// let h = LeftDeadEnd::waiting(2);
    /// let k = (&g + &h).canonical();
    ///
    /// let (a, b, c) = k.is_integer();
    ///
    /// // The maximum rank of an integer dividing `k` is 3.
    /// assert_eq!(a, 3);
    ///
    /// // The counterpart to the integer in the factorisation of `k` is `h` (by construction).
    /// assert_eq!(b, &h);
    ///
    /// // The game `k` is not an integer.
    /// assert_eq!(c, false);
    /// ```
    ///
    /// ```
    /// # use gemau::LeftDeadEnd;
    /// let g = LeftDeadEnd::integer(7);
    ///
    /// let (a, b, c) = g.is_integer();
    ///
    /// // The maximum rank of an integer dividing `g` is 7.
    /// assert_eq!(a, 7);
    ///
    /// // The counterpart to the integer in the factorisation of `g` is 0 (by construction).
    /// assert_eq!(b, &LeftDeadEnd::ZERO);
    ///
    /// // The game `g` *is* an integer.
    /// assert_eq!(c, true);
    /// ```
    #[must_use]
    pub fn is_integer(&self) -> (usize, &LeftDeadEnd, bool) {
        if self.options.is_empty() {
            return (0, self, true);
        }
        if self.options.len() > 1 {
            return (0, self, false);
        }
        let (a, b, c) = self.options[0].is_integer();
        (a + 1, b, c)
    }

    /// Bounds the length of factorisations of the [`LeftDeadEnd`]. This bound is *not* always
    /// optimal.
    #[must_use]
    pub fn bound_length(&self) -> usize {
        if self.options.is_empty() {
            return 0;
        }

        if self.options.len() == 1 {
            return 1 + self.options[0].bound_length();
        }

        [
            self.race(),
            self.options
                .iter()
                .fold(usize::MAX, |min, x| min.min(x.bound_length()))
                + 1,
            self.term_lengths().len() - 1,
            (self.flex() + 1) / 2,
        ]
        .iter()
        .fold_while(usize::MAX, |min, &x| {
            if x == 1 {
                Done(min)
            } else {
                Continue(min.min(x))
            }
        })
        .into_inner()
    }

    /// Returns the flexibility of the [`LeftDeadEnd`].
    #[must_use]
    pub fn flex(&self) -> usize {
        if self.is_integer().2 {
            return 0;
        }

        self.options.iter().fold(0, |max, x| max.max(x.flex())) + 1
    }

    /// Returns the birthday of the [`LeftDeadEnd`]. Note that this is equivalent to its formal
    /// birthday, and hence there is no separate function for that. Also note that this is
    /// equivalent to `max(self.term_lengths())`.
    #[must_use]
    pub fn birth(&self) -> usize {
        if self.options.is_empty() {
            return 0;
        }

        self.options.iter().fold(0, |max, x| max.max(x.birth())) + 1
    }

    /// Returns the race of the [`LeftDeadEnd`]. Note that this is equivalent to
    /// `min(self.term_lengths())`.
    #[must_use]
    pub fn race(&self) -> usize {
        if self.options.is_empty() {
            return 0;
        }

        self.options
            .iter()
            .fold(usize::MAX, |min, x| min.min(x.race()))
            + 1
    }

    /// Returns the set of terminal lengths of the [`LeftDeadEnd`] as a vector; there are no
    /// repeated entries. Note that `min(self.term_lengths()) == self.race()` and
    /// `max(self.term_lengths()) == self.birth()`.
    #[must_use]
    pub fn term_lengths(&self) -> Vec<usize> {
        if self.options.is_empty() {
            return vec![0];
        }

        self.options
            .iter()
            .flat_map(|x| x.term_lengths().into_iter().map(|num| num + 1))
            .fold(vec![], |mut acc, x| {
                if !acc.contains(&x) {
                    acc.push(x);
                }
                acc
            })
    }
}

/// Will format canonical form integers and waiting games.
impl fmt::Display for LeftDeadEnd {
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

        let mut rep = String::new();
        for g in &self.options {
            rep.push_str(&format!("{g},"));
        }
        rep = rep[..rep.len() - 1].to_string();
        write!(f, "{{{rep}}}")
    }
}

/// Will only format canonical form integers.
impl fmt::Debug for LeftDeadEnd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (a, b, c) = self.is_integer();
        if c {
            return write!(f, "{a}");
        }
        if a > 0 {
            return write!(f, "{a}+{b:?}");
        }

        let mut rep = String::new();
        for g in &self.options {
            rep.push_str(&format!("{g:?},"));
        }
        rep = rep[..rep.len() - 1].to_string();
        write!(f, "{{{rep}}}")
    }
}

/// The hash is **not** well-defined up to equivalence.
///
/// # Example
///
/// ```
/// # use gemau::LeftDeadEnd;
/// # use std::hash::{Hash, DefaultHasher, Hasher};
/// let g = LeftDeadEnd::waiting(2) + 1;
/// let h = (LeftDeadEnd::waiting(2) + 1).canonical();
///
/// let mut hasher = DefaultHasher::new();
/// g.hash(&mut hasher);
/// let first_hash = hasher.finish();
///
/// let mut hasher = DefaultHasher::new();
/// h.hash(&mut hasher);
/// let second_hash = hasher.finish();
///
/// assert_ne!(first_hash, second_hash);
/// ```
impl std::hash::Hash for LeftDeadEnd {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        format!("{self:?}").hash(state);
    }
}

impl Eq for LeftDeadEnd {}

impl PartialEq for LeftDeadEnd {
    fn eq(&self, other: &LeftDeadEnd) -> bool {
        self >= other && other >= self
    }
}

impl PartialOrd for LeftDeadEnd {
    fn partial_cmp(&self, other: &LeftDeadEnd) -> Option<Ordering> {
        match (self >= other, other >= self) {
            (true, true) => Some(Ordering::Equal),
            (true, _) => Some(Ordering::Greater),
            (_, true) => Some(Ordering::Less),
            _ => None,
        }
    }

    fn ge(&self, other: &LeftDeadEnd) -> bool {
        if self.options.is_empty() {
            return other.options.is_empty();
        }

        self.options
            .iter()
            .all(|g| other.options.iter().any(|h| g >= h))
    }
}

impl Add for &LeftDeadEnd {
    type Output = LeftDeadEnd;

    fn add(self, other: Self) -> Self::Output {
        if self.options.is_empty() {
            return other.clone();
        }
        if other.options.is_empty() {
            return self.clone();
        }

        let mut options = vec![];

        for g in &self.options {
            options.push(g.clone() + other.clone());
        }
        for h in &other.options {
            options.push(self.clone() + h.clone());
        }

        LeftDeadEnd { options }
    }
}

impl<Rhs> Add<Rhs> for LeftDeadEnd
where
    Rhs: Into<LeftDeadEnd>,
{
    type Output = Self;

    fn add(self, rhs: Rhs) -> Self::Output {
        let other = rhs.into();

        if self.options.is_empty() {
            return other;
        }
        if other.options.is_empty() {
            return self;
        }

        let mut options = vec![];

        for g in &self.options {
            options.push(g.clone() + other.clone());
        }
        for h in other.options {
            options.push(self.clone() + h);
        }

        LeftDeadEnd { options }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn birthday() {
        assert_eq!(LeftDeadEnd::ZERO.birth(), 0);
        assert_eq!(LeftDeadEnd::integer(3).birth(), 3);
        assert_eq!(
            (LeftDeadEnd::waiting(2) + LeftDeadEnd::waiting(3)).birth(),
            5
        );
    }

    #[test]
    fn race() {
        assert_eq!(LeftDeadEnd::ZERO.race(), 0);
        assert_eq!(LeftDeadEnd::integer(3).race(), 3);
        assert_eq!(
            (LeftDeadEnd::waiting(2) + LeftDeadEnd::waiting(3)).race(),
            2
        );
    }

    #[test]
    fn flex() {
        assert_eq!(LeftDeadEnd::ZERO.flex(), 0);
        assert_eq!(LeftDeadEnd::integer(3).flex(), 0);
        assert_eq!(
            (LeftDeadEnd::waiting(2) + LeftDeadEnd::waiting(3)).flex(),
            4
        );
    }
}
