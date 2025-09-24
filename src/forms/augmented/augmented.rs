use std::fmt::Debug;
use std::ops::Add;
use std::ops::Neg;
use std::ops::Sub;

use serde::Deserialize;
use serde::Serialize;

use crate::Outcome;
use crate::ShortPartizan;
use crate::ShortRef;

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Augmented {
    left: Vec<Augmented>,
    right: Vec<Augmented>,
    tombstones: Tombstones,
}

impl Augmented {
    pub const ZERO: Self = Self {
        left: vec![],
        right: vec![],
        tombstones: Tombstones::new(),
    };

    #[must_use]
    pub fn new(left: &[Self], right: &[Self]) -> Self {
        Self {
            left: left.to_vec(),
            right: right.to_vec(),
            tombstones: Tombstones::new(),
        }
    }

    #[must_use]
    pub fn new_with_vec(left: Vec<Self>, right: Vec<Self>) -> Self {
        Self {
            left,
            right,
            tombstones: Tombstones::new(),
        }
    }

    #[must_use]
    pub fn new_with_tomb(left: &[Self], right: &[Self], tombstones: Tombstones) -> Self {
        Self {
            left: left.to_vec(),
            right: right.to_vec(),
            tombstones,
        }
    }

    #[must_use]
    pub fn n_truncate(&self, n: usize) -> Self {
        if n == 0 {
            return Self::ZERO;
        }

        Self {
            left: self.left().map(|g_l| g_l.n_truncate(n - 1)).collect(),
            right: self.right().map(|g_r| g_r.n_truncate(n - 1)).collect(),
            tombstones: self.tombstones.clone(),
        }
    }

    pub fn set_left_tomb(&mut self, tomb: bool) {
        self.tombstones.set_left(tomb);
    }

    pub fn set_right_tomb(&mut self, tomb: bool) {
        self.tombstones.set_right(tomb);
    }

    #[must_use]
    pub fn conjugate(&self) -> Self {
        Augmented {
            left: self.right().map(Self::conjugate).collect(),
            right: self.left().map(Self::conjugate).collect(),
            tombstones: self.tombstones.conjugate(),
        }
    }

    #[must_use]
    pub fn left_outcome(&self) -> Outcome {
        if self.left_endlike() || self.left().any(|x| x.right_outcome() == Outcome::L) {
            return Outcome::L;
        }

        Outcome::R
    }

    #[must_use]
    pub fn right_outcome(&self) -> Outcome {
        if self.right_endlike() || self.right().any(|x| x.left_outcome() == Outcome::R) {
            return Outcome::R;
        }

        Outcome::L
    }

    #[must_use]
    pub fn outcome(&self) -> Outcome {
        match (self.left_outcome(), self.right_outcome()) {
            (Outcome::L, Outcome::L) => Outcome::L,
            (Outcome::L, Outcome::R) => Outcome::N,
            (Outcome::R, Outcome::L) => Outcome::P,
            (Outcome::R, Outcome::R) => Outcome::R,
            _ => unreachable!(),
        }
    }

    #[must_use]
    pub fn p_free(&self) -> bool {
        self.outcome() != Outcome::P
            && self.left().all(Self::p_free)
            && self.right().all(Self::p_free)
    }

    #[must_use]
    pub fn left_endlike(&self) -> bool {
        self.left_end() || self.left_tombstone()
    }

    #[must_use]
    pub fn right_endlike(&self) -> bool {
        self.right_end() || self.right_tombstone()
    }

    #[must_use]
    pub fn left_tombstone(&self) -> bool {
        self.tombstones.left()
    }

    #[must_use]
    pub fn right_tombstone(&self) -> bool {
        self.tombstones.right()
    }

    #[must_use]
    pub fn integer(target: i64) -> Self {
        if target == 0 {
            return Self::ZERO;
        }

        if target > 0 {
            return Self::new(&[Self::integer(target - 1)], &[]);
        }

        Self::new(&[], &[Self::integer(target + 1)])
    }

    #[must_use]
    pub fn n_star(n: usize) -> Self {
        if n == 0 {
            return Self::ZERO;
        }

        let mut moves = vec![];
        for i in 0..n {
            moves.push(Self::n_star(i));
        }

        Self {
            left: moves.clone(),
            right: moves,
            tombstones: Tombstones::new(),
        }
    }

    #[must_use]
    pub fn n_waiting(target: i64) -> Self {
        match target {
            0 => Self::ZERO,
            1 => Self::integer(1),
            -1 => Self::integer(-1),
            i if i > 0 => Self::new(&[Self::ZERO, Self::n_waiting(i - 1)], &[]),
            i => Self::new(&[], &[Self::ZERO, Self::n_waiting(i + 1)]),
        }
    }

    fn remove_whitespace(s: &str) -> String {
        s.chars().filter(|c| !c.is_whitespace()).collect()
    }

    #[allow(clippy::too_many_lines)]
    #[must_use]
    pub fn from_string(s: &str) -> Self {
        let s = Self::remove_whitespace(s);

        let mut left_t = false;
        let mut right_t = false;

        if !s.contains('|') {
            let letters: Vec<char> = s.chars().collect();

            if letters[0] == '-' {
                let mut is_num = true;
                for i in letters.iter().skip(1) {
                    if !i.is_ascii_digit() {
                        is_num = false;
                        break;
                    }
                }
                if is_num {
                    let i = s.parse::<i64>().unwrap();
                    return Self::integer(i);
                }
            }

            let mut is_num = true;
            for i in &letters {
                if !i.is_ascii_digit() {
                    is_num = false;
                    break;
                }
            }
            if is_num {
                let i = s.parse::<i64>().unwrap();
                return Self::integer(i);
            }

            if letters[0] == '*' {
                if letters.len() == 1 {
                    return Self::n_star(1);
                }

                let mut is_num = true;
                for i in letters.iter().skip(1) {
                    if !i.is_ascii_digit() {
                        is_num = false;
                        break;
                    }
                }

                if is_num {
                    let i = s[1..].parse::<usize>().unwrap();
                    return Self::n_star(i);
                }
            }
        }

        // removing leading and trailing curly brackets
        let s = s[..s.len() - 1].to_string();
        let s = s[1..s.len()].to_string();

        let mut left = String::new();
        let mut right = String::new();

        let letters: Vec<char> = s.chars().collect();
        let mut diff = 0;
        for i in 0..letters.len() {
            if letters[i] == '{' {
                diff += 1;
            } else if letters[i] == '}' {
                diff -= 1;
            } else if letters[i] == '|' && diff == 0 {
                left = letters[0..i].iter().collect();
                right = letters[i + 1..].iter().collect();
            }
        }

        let mut terms: Vec<String> = vec![];
        let letters: Vec<char> = left.chars().collect();
        let mut diff = 0;
        let mut start = 0;
        for i in 0..letters.len() {
            if letters[i] == '{' {
                diff += 1;
            } else if letters[i] == '}' {
                diff -= 1;
            } else if letters[i] == ',' && diff == 0 {
                let term = letters[start..i].iter().collect();
                if term == "s" {
                    left_t = true;
                } else {
                    terms.push(term);
                }
                start = i + 1;
            }
            if i == letters.len() - 1 {
                let term = letters[start..].iter().collect();
                if term == "s" {
                    right_t = true;
                } else {
                    terms.push(term);
                }
            }
        }

        let left: Vec<Self> = terms.into_iter().map(|x| Self::from_string(&x)).collect();

        let mut terms: Vec<String> = vec![];
        let letters: Vec<char> = right.chars().collect();
        let mut diff = 0;
        let mut start = 0;
        for i in 0..letters.len() {
            if letters[i] == '{' {
                diff += 1;
            } else if letters[i] == '}' {
                diff -= 1;
            } else if letters[i] == ',' && diff == 0 {
                let term = letters[start..i].iter().collect();
                if term == "s" {
                    right_t = true;
                } else {
                    terms.push(term);
                }
                start = i + 1;
            }
            if i == letters.len() - 1 {
                let term = letters[start..].iter().collect();
                if term == "s" {
                    right_t = true;
                } else {
                    terms.push(term);
                }
            }
        }

        let right: Vec<Self> = terms.into_iter().map(|x| Self::from_string(&x)).collect();

        let tombstones = Tombstones::new_with(left_t, right_t);

        Self {
            left,
            right,
            tombstones,
        }
    }
}

impl Add<Augmented> for Augmented {
    type Output = Augmented;

    fn add(self, rhs: Augmented) -> Self::Output {
        &self + &rhs
    }
}

impl Add<Augmented> for &Augmented {
    type Output = Augmented;

    fn add(self, rhs: Augmented) -> Self::Output {
        self + &rhs
    }
}

impl Add<&Augmented> for Augmented {
    type Output = Augmented;

    fn add(self, rhs: &Augmented) -> Self::Output {
        &self + rhs
    }
}

impl Add<&Augmented> for &Augmented {
    type Output = Augmented;

    fn add(self, rhs: &Augmented) -> Self::Output {
        let left_t = self.left_endlike()
            && rhs.left_endlike()
            && (self.left_tombstone() || rhs.left_tombstone());
        let right_t = self.right_endlike()
            && rhs.right_endlike()
            && (self.right_tombstone() || rhs.right_tombstone());

        Augmented {
            left: self
                .left()
                .map(|g_l| g_l + rhs)
                .chain(rhs.left().map(|h_l| self + h_l))
                .collect(),
            right: self
                .right()
                .map(|g_r| g_r + rhs)
                .chain(rhs.right().map(|h_r| self + h_r))
                .collect(),
            tombstones: Tombstones::new_with(left_t, right_t),
        }
    }
}

impl Sub<Augmented> for Augmented {
    type Output = Augmented;

    fn sub(self, rhs: Augmented) -> Self::Output {
        &self - &rhs
    }
}

impl Sub<Augmented> for &Augmented {
    type Output = Augmented;

    fn sub(self, rhs: Augmented) -> Self::Output {
        self - &rhs
    }
}

impl Sub<&Augmented> for Augmented {
    type Output = Augmented;

    fn sub(self, rhs: &Augmented) -> Self::Output {
        &self - rhs
    }
}

impl Sub<&Augmented> for &Augmented {
    type Output = Augmented;

    fn sub(self, rhs: &Augmented) -> Self::Output {
        self + rhs.conjugate()
    }
}

impl Neg for Augmented {
    type Output = Augmented;

    fn neg(self) -> Self::Output {
        self.conjugate()
    }
}

impl Neg for &Augmented {
    type Output = Augmented;

    fn neg(self) -> Self::Output {
        self.conjugate()
    }
}

impl ShortRef for Augmented {
    fn options(&self) -> impl Iterator<Item = &Self> {
        self.left().chain(self.right())
    }
}

impl ShortPartizan for Augmented {
    fn left(&self) -> impl Iterator<Item = &Self> {
        self.left.iter()
    }

    fn right(&self) -> impl Iterator<Item = &Self> {
        self.right.iter()
    }
}

#[derive(Clone, Serialize, Deserialize, Debug, PartialEq)]
pub struct Tombstones(bool, bool);

impl Default for Tombstones {
    fn default() -> Self {
        Self::new()
    }
}

impl Tombstones {
    // TODO: when should this be const?
    #[must_use]
    pub const fn new() -> Self {
        Self(false, false)
    }

    #[must_use]
    pub fn new_with(left: bool, right: bool) -> Self {
        Self(left, right)
    }

    #[must_use]
    pub fn conjugate(&self) -> Self {
        Self(self.1, self.0)
    }

    #[must_use]
    pub fn left(&self) -> bool {
        self.0
    }

    #[must_use]
    pub fn right(&self) -> bool {
        self.1
    }

    pub fn set_left(&mut self, tomb: bool) {
        self.0 = tomb;
    }

    pub fn set_right(&mut self, tomb: bool) {
        self.1 = tomb;
    }
}
