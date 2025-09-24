use std::cmp::Ordering;

#[derive(PartialEq, Clone, Debug)]
/// Represents the four outcome classes of short partizan games.
pub enum Outcome {
    /// 'Left' outcome: Left always wins.
    L,
    /// 'Next' outcome: the first (next) player wins.
    N,
    /// 'Previous' outcome: the second (previous) player wins.
    P,
    /// 'Right' outcome: Right always wins.
    R,
}

impl PartialOrd for Outcome {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self == other {
            return Some(Ordering::Equal);
        }
        if *self == Outcome::L || *other == Outcome::R {
            return Some(Ordering::Greater);
        }
        if *self == Outcome::R || *other == Outcome::L {
            return Some(Ordering::Less);
        }
        None
    }
}
