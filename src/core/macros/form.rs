// TODO: refactor
use std::cmp::Ordering;

#[derive(Debug, PartialEq, Eq)]
pub enum UsizeOrInfinity {
    Finite(usize),
    Infinity,
}

impl PartialOrd for UsizeOrInfinity {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for UsizeOrInfinity {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (UsizeOrInfinity::Infinity, UsizeOrInfinity::Infinity) => Ordering::Equal,
            (UsizeOrInfinity::Infinity, _) => Ordering::Greater,
            (_, UsizeOrInfinity::Infinity) => Ordering::Less,
            (UsizeOrInfinity::Finite(a), UsizeOrInfinity::Finite(b)) => a.cmp(b),
        }
    }
}

#[macro_export]
macro_rules! impl_form_wrapper {
    ($wrapper:ident) => {
        impl ShortRef for $wrapper {
            fn options(&self) -> impl Iterator<Item = &Self> {
                self.0.options().map(Self::ref_cast)
            }
        }

        impl ShortPartizan for $wrapper {
            fn left(&self) -> impl Iterator<Item = &Self> {
                self.0.left().map(Self::ref_cast)
            }

            fn right(&self) -> impl Iterator<Item = &Self> {
                self.0.right().map(Self::ref_cast)
            }
        }

        impl PartialEq for $wrapper {
            fn eq(&self, other: &Self) -> bool {
                self.partial_cmp(other) == Some(Ordering::Equal)
            }
        }

        impl Add<$wrapper> for $wrapper {
            type Output = $wrapper;

            fn add(self, rhs: $wrapper) -> Self::Output {
                &self + &rhs
            }
        }

        impl Add<$wrapper> for &$wrapper {
            type Output = $wrapper;

            fn add(self, rhs: $wrapper) -> Self::Output {
                self + &rhs
            }
        }

        impl Add<&$wrapper> for $wrapper {
            type Output = $wrapper;

            fn add(self, rhs: &$wrapper) -> Self::Output {
                &self + rhs
            }
        }

        impl Add<&$wrapper> for &$wrapper {
            type Output = $wrapper;

            fn add(self, rhs: &$wrapper) -> Self::Output {
                self.add_disj(rhs)
            }
        }

        impl Sub<$wrapper> for $wrapper {
            type Output = $wrapper;

            fn sub(self, rhs: $wrapper) -> Self::Output {
                &self - &rhs
            }
        }

        impl Sub<$wrapper> for &$wrapper {
            type Output = $wrapper;

            fn sub(self, rhs: $wrapper) -> Self::Output {
                self - &rhs
            }
        }

        impl Sub<&$wrapper> for $wrapper {
            type Output = $wrapper;

            fn sub(self, rhs: &$wrapper) -> Self::Output {
                &self - rhs
            }
        }

        impl Sub<&$wrapper> for &$wrapper {
            type Output = $wrapper;

            fn sub(self, rhs: &$wrapper) -> Self::Output {
                self.add_disj(&rhs.conjugate())
            }
        }

        impl Neg for $wrapper {
            type Output = $wrapper;

            fn neg(self) -> Self::Output {
                self.conjugate()
            }
        }

        impl Neg for &$wrapper {
            type Output = $wrapper;

            fn neg(self) -> Self::Output {
                self.conjugate()
            }
        }

        impl $wrapper {
            #[must_use]
            pub fn new(form: Form) -> Self {
                Self(form)
            }

            #[must_use]
            pub fn inner(self) -> Form {
                self.0
            }

            #[must_use]
            pub fn add_disj(&self, other: &Self) -> Self {
                Self(&self.0 + &other.0)
            }

            #[must_use]
            pub fn add_dim_disj(&self, other: &Self) -> Self {
                Self(self.0.add_dim_disj(&other.0))
            }

            #[must_use]
            pub fn add_conj(&self, other: &Self) -> Self {
                Self(self.0.add_conj(&other.0))
            }

            #[must_use]
            pub fn add_cont_conj(&self, other: &Self) -> Self {
                Self(self.0.add_cont_conj(&other.0))
            }

            #[must_use]
            pub fn add_seq(&self, other: &Self) -> Self {
                Self(self.0.add_seq(&other.0))
            }

            #[must_use]
            pub fn add_sel(&self, other: &Self) -> Self {
                Self(self.0.add_sel(&other.0))
            }

            #[must_use]
            pub fn add_short_sel(&self, other: &Self) -> Self {
                Self(self.0.add_short_sel(&other.0))
            }

            #[must_use]
            pub fn add_ord(&self, other: &Self) -> Self {
                Self(self.0.add_ord(&other.0))
            }

            #[must_use]
            pub fn add_side(&self, other: &Self) -> Self {
                Self(self.0.add_side(&other.0))
            }

            #[must_use]
            pub fn conjugate(&self) -> Self {
                Self(self.0.conjugate())
            }

            #[must_use]
            pub fn integer(target: i32) -> Self {
                Self(Form::integer(target))
            }

            #[must_use]
            pub fn n_star(n: u32) -> Self {
                Self(Form::n_star(n))
            }

            #[must_use]
            pub fn adjoint(&self) -> Self {
                Self(self.0.adjoint())
            }

            #[must_use]
            pub fn from_string(s: &str) -> Self {
                Self(Form::from_string(s))
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
            pub fn left_maint(&self, other: &Self) -> bool {
                other.left().all(|h_l| {
                    self.left().any(|g_l| g_l.ge(h_l)) || h_l.right().any(|h_lr| self.ge(h_lr))
                })
            }

            #[must_use]
            pub fn right_maint(&self, other: &Self) -> bool {
                self.right().all(|g_r| {
                    other.right().any(|h_r| g_r.ge(h_r)) || g_r.left().any(|g_rl| g_rl.ge(other))
                })
            }

            #[must_use]
            pub fn jane(&self) -> Self {
                Self(Form::new(
                    self.left()
                        .filter(|g_l| g_l.outcome() <= Outcome::N)
                        .map(|g_l| g_l.jane().0)
                        .collect(),
                    self.right()
                        .filter(|g_r| g_r.outcome() >= Outcome::N)
                        .map(|g_r| g_r.jane().0)
                        .collect(),
                ))
            }

            #[must_use]
            pub fn left_preference(&self, other: &Self) -> bool {
                other
                    .left()
                    .all(|h| self.left().any(|g| g + other >= self + h))
            }

            #[must_use]
            pub fn left_remoteness(&self) -> usize {
                Self::find_remoteness(self.left(), Self::right_remoteness)
            }

            #[must_use]
            pub fn right_remoteness(&self) -> usize {
                Self::find_remoteness(self.right(), Self::left_remoteness)
            }

            #[must_use]
            pub fn isomorphic(&self, other: &Self) -> bool {
                other
                    .left()
                    .all(|h_l| self.left().any(|g_l| g_l.isomorphic(h_l)))
                    && self
                        .left()
                        .all(|g_l| other.left().any(|h_l| g_l.isomorphic(h_l)))
                    && other
                        .right()
                        .all(|h_r| self.right().any(|g_r| g_r.isomorphic(h_r)))
                    && self
                        .right()
                        .all(|g_r| other.right().any(|h_r| g_r.isomorphic(h_r)))
            }

            // TODO: can this just be in the traits?
            #[must_use]
            pub fn left_end_distance(&self) -> usize {
                self.left()
                    .map(|g_l| g_l.left_end_distance() + 1)
                    .min()
                    .unwrap_or(0)
            }

            #[must_use]
            pub fn right_end_distance(&self) -> usize {
                self.right()
                    .map(|g_r| g_r.right_end_distance() + 1)
                    .min()
                    .unwrap_or(0)
            }

            #[must_use]
            pub fn lazy(&self) -> bool {
                self.left().all(|g_l| {
                    g_l.lazy() && g_l.right_end_distance() <= self.right_end_distance() + 1
                }) && self.right().all(|g_r| {
                    g_r.lazy() && g_r.left_end_distance() <= self.left_end_distance() + 1
                })
            }

            #[must_use]
            pub fn n_lazy(&self, n: usize) -> bool {
                self.left().all(|g_l| {
                    g_l.n_lazy(n)
                        && (self.right_end_distance() > n
                            || g_l.right_end_distance() <= self.right_end_distance() + 1)
                }) && self.right().all(|g_r| {
                    g_r.n_lazy(n)
                        && (self.left_end_distance() > n
                            || g_r.left_end_distance() <= self.left_end_distance() + 1)
                })
            }

            #[must_use]
            pub fn laziness(&self) -> Option<$crate::core::macros::form::UsizeOrInfinity> {
                if !self.blocking() {
                    return None;
                }

                if self.lazy() {
                    return Some($crate::core::macros::form::UsizeOrInfinity::Infinity);
                }

                let end_max = self.left_end_distance().max(self.right_end_distance());

                for i in 1..end_max {
                    if !self.n_lazy(i) {
                        return Some($crate::core::macros::form::UsizeOrInfinity::Finite(i - 1));
                    }
                }

                Some($crate::core::macros::form::UsizeOrInfinity::Finite(
                    end_max - 1,
                ))
            }

            #[must_use]
            pub fn option_closed(&self) -> bool {
                self.left().all(|g_l| {
                    g_l.option_closed()
                        && g_l
                            .left()
                            .all(|g_ll| self.left().any(|g_l2| g_ll.isomorphic(g_l2)))
                }) && self.right().all(|g_r| {
                    g_r.option_closed()
                        && g_r
                            .right()
                            .all(|g_rr| self.right().any(|g_r2| g_rr.isomorphic(g_r2)))
                })
            }

            #[must_use]
            pub fn outcome_free(&self, outcomes: &[Outcome]) -> bool {
                !outcomes.contains(&self.outcome())
                    && self.left().all(|g_l| g_l.outcome_free(outcomes))
                    && self.right().all(|g_r| g_r.outcome_free(outcomes))
            }

            #[must_use]
            pub fn l_free(&self) -> bool {
                self.outcome() != Outcome::L
                    && self.left().all(Self::l_free)
                    && self.right().all(Self::l_free)
            }

            #[must_use]
            pub fn n_free(&self) -> bool {
                self.outcome() != Outcome::N
                    && self.left().all(Self::n_free)
                    && self.right().all(Self::n_free)
            }

            #[must_use]
            pub fn p_free(&self) -> bool {
                self.outcome() != Outcome::P
                    && self.left().all(Self::p_free)
                    && self.right().all(Self::p_free)
            }

            #[must_use]
            pub fn r_free(&self) -> bool {
                self.outcome() != Outcome::R
                    && self.left().all(Self::r_free)
                    && self.right().all(Self::r_free)
            }

            fn left_revers(&self, option: &Self) -> Option<Vec<Self>> {
                for r in option.right() {
                    if self >= r {
                        return Some(r.left().cloned().collect());
                    }
                }
                None
            }

            fn right_revers(&self, option: &Self) -> Option<Vec<Self>> {
                for l in option.left() {
                    if l >= self {
                        return Some(l.right().cloned().collect());
                    }
                }
                None
            }

            fn left_canonical(&self) -> Vec<Self> {
                let mut reversed = vec![];
                let mut check_revers: Vec<_> = self.left().cloned().collect();
                while !check_revers.is_empty() {
                    if let Some(i) = self.left_revers(&check_revers[0].clone()) {
                        check_revers.append(&mut i.clone());
                    } else {
                        reversed.push(check_revers[0].clone());
                    }
                    check_revers.swap_remove(0);
                }

                let mut left = vec![];
                for g in &reversed {
                    let mut best = true;
                    for h in &reversed {
                        if h > g {
                            best = false;
                            break;
                        }
                    }
                    if best && !left.contains(g) {
                        left.push(g.clone());
                    }
                }

                left
            }

            fn right_canonical(&self) -> Vec<Self> {
                Self(self.0.conjugate())
                    .left_canonical()
                    .iter()
                    .map(|g| Self(g.0.conjugate()))
                    .collect()
            }

            #[must_use]
            pub fn canonical(&self) -> Self {
                let mut left = vec![];
                for l in self.left_canonical() {
                    left.push(l.canonical().0);
                }

                let mut right = vec![];
                for r in Self(
                    (Form::new(left.clone(), self.right().map(|g| g.0.clone()).collect()))
                        .conjugate(),
                )
                .left_canonical()
                {
                    right.push(Self(r.0.conjugate()).canonical());
                }

                let g = Form::new(left, right.iter().map(|g| g.0.clone()).collect());

                Self(g)
            }

            #[must_use]
            pub fn is_integer(&self) -> Option<i32> {
                let g = self.canonical();

                let mut b = g.birthday() as i32;

                if g.left_end() {
                    b *= -1;
                }

                if g == Self::integer(b) {
                    return Some(b);
                }

                None
            }

            #[must_use]
            pub fn is_star(&self) -> Option<u32> {
                let g = self.canonical();

                let b = g.birthday() as u32;

                if g == Self::n_star(b) {
                    return Some(b);
                }

                None
            }
        }
    };
}
