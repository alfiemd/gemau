use std::cmp::Ordering;
use std::fmt;
use std::fmt::Debug;
use std::ops::Add;
use std::ops::Neg;
use std::ops::Sub;
use std::str::FromStr;

use serde::Deserialize;
use serde::Serialize;

use crate::core::traits::ShortPartizan;
use crate::core::traits::ShortRef;

#[derive(Clone, Serialize, Deserialize)]
pub struct Form {
    left: Vec<Form>,
    right: Vec<Form>,
}

impl Form {
    /// The zero (or empty) game: neither player has an option.
    ///
    /// This is an identity element for the disjunctive, sequential, selective, and some other
    /// compounds.
    pub const ZERO: Self = Self {
        left: vec![],
        right: vec![],
    };

    /// Creates a new game with given options for Left and Right
    #[must_use]
    pub fn new(left: Vec<Form>, right: Vec<Form>) -> Form {
        Form { left, right }
    }

    /// Adds two games using the disjunctive compound.
    ///
    /// In a disjunctive sum of games, a player must make a move in exactly one component.
    ///
    /// Equivalent to `+`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::form::Form;
    /// let g = Form::new(vec![Form::ZERO], vec![]);
    /// let h = Form::new(vec![], vec![Form::ZERO]);
    /// assert_eq!(g.add_disj(&h), g + h);
    /// ```
    #[must_use]
    pub fn add_disj(&self, other: &Form) -> Self {
        Form {
            left: self
                .left()
                .map(|g_l| g_l + other)
                .chain(other.left().map(|h_l| self + h_l))
                .collect(),
            right: self
                .right()
                .map(|g_r| g_r + other)
                .chain(other.right().map(|h_r| self + h_r))
                .collect(),
        }
    }

    /// Adds two games using the diminished disjunctive compound.
    ///
    /// In a diminished disjunctive sum of games, a player must make a move in exactly one
    /// component, but play stops immediately when one of the components terminates.
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::form::Form;
    /// # use crate::gemau::ShortPartizan;
    /// let g = Form::new(vec![Form::ZERO], vec![]);
    /// let h = Form::ZERO;
    /// let sum = g.add_dim_disj(&h);
    ///
    /// // Compare with `g.add_disj(&h)`, which would return a `Form` isomorphic to `g`
    /// assert!(sum.left_end() && sum.right_end());
    /// ```
    #[must_use]
    pub fn add_dim_disj(&self, other: &Form) -> Form {
        if self.left.is_empty() && self.right.is_empty() {
            return Form::ZERO;
        }
        if other.left.is_empty() && other.right.is_empty() {
            return Form::ZERO;
        }

        let mut left = vec![];
        for l in &self.left {
            left.push(l.add_dim_disj(other));
        }
        for l in &other.left {
            left.push(l.add_dim_disj(self));
        }

        let mut right = vec![];
        for r in &self.right {
            right.push(r.add_dim_disj(other));
        }
        for r in &other.right {
            right.push(r.add_dim_disj(self));
        }

        Form { left, right }
    }

    /// Adds two games using the conjunctive compound.
    ///
    /// In a conjunctive sum of games, a player must make a move in every component.
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::form::Form;
    /// # use crate::gemau::ShortPartizan;
    /// let g = Form::new(vec![Form::ZERO], vec![]);
    /// let h = Form::new(vec![], vec![Form::ZERO]);
    /// let sum = g.add_conj(&h);
    /// assert!(sum.left_end() && sum.right_end());
    /// ```
    #[must_use]
    pub fn add_conj(&self, other: &Form) -> Form {
        let mut left = vec![];
        for l1 in self.left() {
            for l2 in other.left() {
                left.push(l1.add_conj(l2));
            }
        }

        let mut right = vec![];
        for r1 in self.right() {
            for r2 in other.right() {
                right.push(r1.add_conj(r2));
            }
        }

        Form { left, right }
    }

    /// Adds two games using the continued conjunctive compound.
    ///
    /// In a continued conjunctive sum of games, a player must make a move in every component that
    /// is not terminal.
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::form::Form;
    /// # use crate::gemau::ShortPartizan;
    /// let g = Form::new(vec![Form::ZERO], vec![]);
    /// let h = Form::new(vec![], vec![Form::ZERO]);
    /// let sum = g.add_cont_conj(&h);
    ///
    /// // Compare with `g.add_conj(&h)`, which would return `Form::ZERO`
    /// assert!(!sum.left_end() || !sum.right_end());
    /// ```
    #[must_use]
    pub fn add_cont_conj(&self, other: &Form) -> Form {
        let mut left = vec![];
        if self.left.is_empty() {
            left.clone_from(&other.left);
        } else if other.left.is_empty() {
            left.clone_from(&self.left);
        } else {
            for l1 in &self.left {
                for l2 in &other.left {
                    left.push(l1.add_conj(l2));
                }
            }
        }

        let mut right = vec![];
        if self.right.is_empty() {
            right.clone_from(&other.right);
        } else if other.right.is_empty() {
            right.clone_from(&self.right);
        } else {
            for r1 in &self.right {
                for r2 in &other.right {
                    right.push(r1.add_conj(r2));
                }
            }
        }

        Form { left, right }
    }

    /// Adds two games using the sequential compound.
    ///
    /// In a sequential compound of games, play is restricted to the first component until it
    /// terminates, at which point play may continue in the second component.
    ///
    /// # Warning
    ///
    /// This binary operation is *non-commutative*!
    ///
    /// # Examples
    ///
    /// ```
    /// # use gemau::form::Form;
    /// # use crate::gemau::ShortPartizan;
    /// let g = Form::new(vec![Form::ZERO], vec![]);
    /// let h = Form::new(vec![], vec![Form::ZERO]);
    /// let sum = g.add_cont_conj(&h);
    ///
    /// // Compare with `g.add_conj(&h)`, which would return `Form::ZERO`.
    /// assert!(!sum.left_end() || !sum.right_end());
    /// ```
    #[must_use]
    pub fn add_seq(&self, other: &Form) -> Form {
        if self.left.is_empty() && self.right.is_empty() {
            return other.clone();
        }

        let mut left = vec![];
        for l in &self.left {
            left.push(l.add_seq(other));
        }

        let mut right = vec![];
        for r in &self.right {
            right.push(r.add_seq(other));
        }

        Form { left, right }
    }

    /// Adds two games using the selective compound.
    ///
    /// In a selective compound of games, a player can choose a non-empty subset of components to
    /// move in (i.e. they must move in _at least_ one component).
    #[must_use]
    pub fn add_sel(&self, other: &Form) -> Form {
        let mut left = vec![];
        for l1 in &self.left {
            left.push(l1.add_sel(other));
            for l2 in &other.left {
                left.push(l1.add_sel(l2));
            }
        }
        for l in &other.left {
            left.push(other.add_sel(l));
        }

        let mut right = vec![];
        for r1 in &self.right {
            right.push(r1.add_sel(other));
            for r2 in &other.left {
                right.push(r1.add_sel(r2));
            }
        }
        for r in &other.right {
            right.push(other.add_sel(r));
        }

        Form { left, right }
    }

    /// Adds two games using the shortened selective compound.
    ///
    /// In a shortened selective sum of games, a player can choose a non-empty subset of components
    /// to move in (i.e. they must move in _at leat_ one component), but play terminates
    /// immediately if one of the components terminates.
    #[must_use]
    pub fn add_short_sel(&self, other: &Form) -> Form {
        if self.left.is_empty() && self.right.is_empty() {
            return Form::ZERO;
        }
        if other.left.is_empty() && other.right.is_empty() {
            return Form::ZERO;
        }

        let mut left = vec![];
        for l1 in &self.left {
            left.push(l1.add_short_sel(other));
            for l2 in &other.left {
                left.push(l1.add_short_sel(l2));
            }
        }
        for l in &other.left {
            left.push(other.add_short_sel(l));
        }

        let mut right = vec![];
        for r1 in &self.right {
            right.push(r1.add_short_sel(other));
            for r2 in &other.left {
                right.push(r1.add_short_sel(r2));
            }
        }
        for r in &other.right {
            right.push(other.add_short_sel(r));
        }

        Form { left, right }
    }

    /// Adds two games using the ordinal compound.
    ///
    /// In an ordinal sum of games, a player must move in exactly one component, but moving on the
    /// first component will terminate (annihilate) the second component.
    ///
    /// # Warning
    ///
    /// This operation is *non-commutative*!
    #[must_use]
    pub fn add_ord(&self, other: &Form) -> Form {
        let mut left = self.left.clone();
        for l in &other.left {
            left.push(self.add_ord(l));
        }

        let mut right = self.right.clone();
        for r in &other.right {
            right.push(self.add_ord(r));
        }

        Form { left, right }
    }

    /// Adds two games using the side compound.
    ///
    /// In a side sum of games, a player must move in exactly one component, but Left making a move
    /// on the second component will terminate (annihilate) the first, and Right making a move on
    /// the first component will terminate (annihilate) the second.
    ///
    /// # Warning
    ///
    /// This operation is *non-commutative*!
    #[must_use]
    pub fn add_side(&self, other: &Form) -> Form {
        let mut left = other.left.clone();
        for l in &self.left {
            left.push(l.add_side(other));
        }

        let mut right = self.right.clone();
        for r in &other.right {
            right.push(self.add_side(r));
        }

        Form { left, right }
    }

    #[must_use]
    pub fn conjugate(&self) -> Form {
        Form {
            left: self.right().map(Self::conjugate).collect(),
            right: self.left().map(Self::conjugate).collect(),
        }
    }

    #[must_use]
    pub fn adjoint(&self) -> Form {
        if self.is_empty() {
            return Form::n_star(1);
        }

        if self.left_end() {
            return Form {
                left: self.right().map(Self::adjoint).collect(),
                right: vec![Form::ZERO],
            };
        }

        if self.right_end() {
            return Form {
                left: vec![Form::ZERO],
                right: self.left().map(Self::adjoint).collect(),
            };
        }

        Form {
            left: self.right().map(Self::adjoint).collect(),
            right: self.left().map(Self::adjoint).collect(),
        }
    }

    #[must_use]
    pub fn integer(target: i32) -> Self {
        if target == 0 {
            return Self::ZERO;
        }

        if target > 0 {
            return Self {
                left: vec![Self::integer(target - 1)],
                right: vec![],
            };
        }

        Self {
            left: vec![],
            right: vec![Self::integer(target + 1)],
        }
    }

    #[must_use]
    pub fn n_star(n: u32) -> Form {
        if n == 0 {
            return Form::ZERO;
        }

        let mut moves = vec![];
        for i in 0..n {
            moves.push(Form::n_star(i));
        }

        Form {
            left: moves.clone(),
            right: moves,
        }
    }

    fn remove_whitespace(s: &str) -> String {
        s.chars().filter(|c| !c.is_whitespace()).collect()
    }

    #[allow(clippy::too_many_lines)]
    #[must_use]
    pub fn from_string(s: &str) -> Form {
        let s = Form::remove_whitespace(s);

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
                    let i = s.parse::<i32>().unwrap();
                    return Form::integer(i);
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
                let i = s.parse::<i32>().unwrap();
                return Form::integer(i);
            }

            if letters[0] == '*' {
                if letters.len() == 1 {
                    return Form::n_star(1);
                }

                let mut is_num = true;
                for i in letters.iter().skip(1) {
                    if !i.is_ascii_digit() {
                        is_num = false;
                        break;
                    }
                }

                if is_num {
                    let i = s[1..].parse::<u32>().unwrap();
                    return Form::n_star(i);
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
                terms.push(letters[start..i].iter().collect());
                start = i + 1;
            }
            if i == letters.len() - 1 {
                terms.push(letters[start..].iter().collect());
            }
        }

        let left: Vec<Form> = terms.into_iter().map(|x| Form::from_string(&x)).collect();

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
                terms.push(letters[start..i].iter().collect());
                start = i + 1;
            }
            if i == letters.len() - 1 {
                terms.push(letters[start..].iter().collect());
            }
        }

        let right: Vec<Form> = terms.into_iter().map(|x| Form::from_string(&x)).collect();

        Form { left, right }
    }

    #[must_use]
    pub fn is_integer(&self) -> Option<i32> {
        let mut b = self.birthday() as i32;

        if self.left_end() {
            b *= -1;
        }

        if *self == Self::integer(b) {
            return Some(b);
        }

        None
    }

    #[must_use]
    pub fn is_star(&self) -> Option<u32> {
        let b = self.birthday() as u32;

        if *self == Self::n_star(b) {
            return Some(b);
        }

        None
    }
}

impl ShortRef for Form {
    fn options(&self) -> impl Iterator<Item = &Self> {
        self.left().chain(self.right())
    }
}

impl ShortPartizan for Form {
    fn left(&self) -> impl Iterator<Item = &Self> {
        self.left.iter()
    }

    fn right(&self) -> impl Iterator<Item = &Self> {
        self.right.iter()
    }
}

impl PartialEq for Form {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other) == Some(Ordering::Equal)
    }
}

impl PartialOrd for Form {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self >= other, other >= self) {
            (true, true) => Some(Ordering::Equal),
            (true, false) => Some(Ordering::Greater),
            (false, true) => Some(Ordering::Less),
            (false, false) => None,
        }
    }

    fn ge(&self, other: &Self) -> bool {
        other.left().all(|h_l| self.left().any(|g_l| g_l >= h_l))
            && self.right().all(|g_r| other.right().any(|h_r| g_r >= h_r))
    }
}

impl Add<Form> for Form {
    type Output = Form;

    fn add(self, rhs: Form) -> Self::Output {
        &self + &rhs
    }
}

impl Add<Form> for &Form {
    type Output = Form;

    fn add(self, rhs: Form) -> Self::Output {
        self + &rhs
    }
}

impl Add<&Form> for Form {
    type Output = Form;

    fn add(self, rhs: &Form) -> Self::Output {
        &self + rhs
    }
}

impl Add<&Form> for &Form {
    type Output = Form;

    fn add(self, rhs: &Form) -> Self::Output {
        self.add_disj(rhs)
    }
}

impl Sub<Form> for Form {
    type Output = Form;

    fn sub(self, rhs: Form) -> Self::Output {
        &self - &rhs
    }
}

impl Sub<Form> for &Form {
    type Output = Form;

    fn sub(self, rhs: Form) -> Self::Output {
        self - &rhs
    }
}

impl Sub<&Form> for Form {
    type Output = Form;

    fn sub(self, rhs: &Form) -> Self::Output {
        &self - rhs
    }
}

impl Sub<&Form> for &Form {
    type Output = Form;

    fn sub(self, rhs: &Form) -> Self::Output {
        self.add_disj(&rhs.conjugate())
    }
}

impl Neg for Form {
    type Output = Form;

    fn neg(self) -> Self::Output {
        self.conjugate()
    }
}

impl Neg for &Form {
    type Output = Form;

    fn neg(self) -> Self::Output {
        self.conjugate()
    }
}

impl Debug for Form {
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

#[derive(Debug, PartialEq, Eq)]
pub struct ParseFormError;

impl FromStr for Form {
    type Err = ParseFormError;

    #[allow(clippy::too_many_lines)]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = Form::remove_whitespace(s);

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
                    if let Ok(i) = s.parse::<i32>() {
                        return Ok(Form::integer(i));
                    }
                    return Err(ParseFormError);
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
                if let Ok(i) = s.parse::<i32>() {
                    return Ok(Form::integer(i));
                }
                return Err(ParseFormError);
            }

            if letters[0] == '*' {
                if letters.len() == 1 {
                    return Ok(Form::n_star(1));
                }

                let mut is_num = true;
                for i in letters.iter().skip(1) {
                    if !i.is_ascii_digit() {
                        is_num = false;
                        break;
                    }
                }

                if is_num {
                    let i = s[1..].parse::<u32>().unwrap();
                    return Ok(Form::n_star(i));
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
                terms.push(letters[start..i].iter().collect());
                start = i + 1;
            }
            if i == letters.len() - 1 {
                terms.push(letters[start..].iter().collect());
            }
        }

        let left: Vec<Form> = terms
            .into_iter()
            .map(|x| Form::from_str(&x))
            .collect::<Result<Vec<Form>, _>>()?;

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
                terms.push(letters[start..i].iter().collect());
                start = i + 1;
            }
            if i == letters.len() - 1 {
                terms.push(letters[start..].iter().collect());
            }
        }

        let right: Vec<Form> = terms
            .into_iter()
            .map(|x| Form::from_str(&x))
            .collect::<Result<Vec<Form>, _>>()?;

        Ok(Form { left, right })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_zero_game() {
        let zero = Form::ZERO;

        assert!(zero.is_empty());
    }

    #[test]
    fn test_integer_construction() {
        let one = Form::integer(1);

        let left_options: Vec<_> = one.left().collect();
        assert_eq!(left_options.len(), 1);
        assert_eq!(*left_options[0], Form::ZERO);

        assert!(one.right_end());
    }

    #[test]
    fn test_n_star_construction() {
        let star = Form::n_star(1);
        let star2 = Form::n_star(2);

        let left: Vec<_> = star.left().collect();
        let right: Vec<_> = star.right().collect();
        assert_eq!(left.len(), 1);
        assert_eq!(right.len(), 1);

        let left2: Vec<_> = star2.left().collect();
        let right2: Vec<_> = star2.right().collect();
        assert_eq!(left2.len(), 2);
        assert_eq!(right2.len(), 2);
    }

    // String parsing tests
    #[test]
    fn test_string_parsing() {
        let game_strs = [
            ("0", Form::ZERO),
            ("1", Form::integer(1)),
            ("*", Form::n_star(1)),
            ("*2", Form::n_star(2)),
            ("{1|0}", Form::new(vec![Form::integer(1)], vec![Form::ZERO])),
        ];

        for (s, expected) in game_strs {
            let parsed = Form::from_string(s);
            assert_eq!(parsed, expected);
        }
    }

    #[test]
    fn test_complex_string_parsing() {
        let complex = "{1,{2|1}|0,{1|0}}";
        let parsed = Form::from_string(complex);

        let left: Vec<_> = parsed.left().collect();
        let right: Vec<_> = parsed.right().collect();
        assert_eq!(left.len(), 2);
        assert_eq!(right.len(), 2);
    }

    #[test]
    fn test_conjugate() {
        let g = Form::integer(1) + Form::integer(-1);
        let neg_g = -&g;

        assert_eq!(-neg_g, g);
    }

    #[cfg(test)]
    mod property_tests {
        use proptest::prelude::*;

        use super::*;

        prop_compose! {
            fn arb_simple_form()(n in -5..5) -> Form {
                Form::integer(n)
            }
        }

        proptest! {
            #[test]
            fn prop_zero_plus_identity(g in arb_simple_form()) {
                assert_eq!(&g + Form::ZERO, g);
            }

            #[test]
            fn prop_neg_neg_cancels(g in arb_simple_form()) {
                assert_eq!(-(-g.clone()), g);
            }
        }
    }

    #[test]
    fn test_string_handling() {
        let _g = Form::from_string("{|}");
        let _h = Form::integer(3);
    }

    #[test]
    fn test_birthday_calculation() {
        let g = Form::integer(2);
        assert_eq!(g.birthday(), 2);

        let h = Form::n_star(2);
        assert_eq!(h.birthday(), 2);
    }
}
