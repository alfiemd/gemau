#[macro_export]
macro_rules! impl_augmented_wrapper {
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

        impl From<Augmented> for $wrapper {
            fn from(value: Augmented) -> Self {
                Self::new(value)
            }
        }

        impl From<$wrapper> for Augmented {
            fn from(value: $wrapper) -> Self {
                value.augmented()
            }
        }

        impl PartialEq for $wrapper {
            fn eq(&self, other: &Self) -> bool {
                self.partial_cmp(other) == Some(Ordering::Equal)
            }
        }

        impl PartialOrd for $wrapper {
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
                $wrapper(self.inner() + rhs.inner())
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

        #[allow(clippy::suspicious_arithmetic_impl)]
        impl Sub<&$wrapper> for &$wrapper {
            type Output = $wrapper;

            fn sub(self, rhs: &$wrapper) -> Self::Output {
                self + rhs.conjugate()
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

        use itertools::Itertools;

        impl $wrapper {
            pub const ZERO: Self = Self(Augmented::ZERO);

            #[must_use]
            pub fn new(form: Augmented) -> Self {
                Self(form)
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
            pub fn left_endlike(&self) -> bool {
                self.0.left_endlike()
            }

            #[must_use]
            pub fn right_endlike(&self) -> bool {
                self.0.right_endlike()
            }

            #[must_use]
            pub fn left_tombstone(&self) -> bool {
                self.0.left_tombstone()
            }

            #[must_use]
            pub fn right_tombstone(&self) -> bool {
                self.0.left_tombstone()
            }

            #[must_use]
            pub fn left_proviso(&self, other: &Self) -> bool {
                self.left_strong() || !other.left_endlike()
            }

            #[must_use]
            pub fn right_proviso(&self, other: &Self) -> bool {
                !self.right_endlike() || other.right_strong()
            }

            #[must_use]
            pub fn left_outcome(&self) -> Outcome {
                self.0.left_outcome()
            }

            #[must_use]
            pub fn right_outcome(&self) -> Outcome {
                self.0.right_outcome()
            }

            #[must_use]
            pub fn outcome(&self) -> Outcome {
                self.0.outcome()
            }

            #[must_use]
            pub fn conjugate(&self) -> Self {
                Self(self.0.conjugate())
            }

            #[must_use]
            pub fn inner(&self) -> &Augmented {
                &self.0
            }

            #[must_use]
            pub fn augmented(self) -> Augmented {
                self.0
            }

            #[must_use]
            pub fn integer(target: i64) -> Self {
                Self(Augmented::integer(target))
            }

            #[must_use]
            pub fn n_star(n: usize) -> Self {
                Self(Augmented::n_star(n))
            }

            #[must_use]
            pub fn n_waiting(target: i64) -> Self {
                Self(Augmented::n_waiting(target))
            }

            #[must_use]
            pub fn from_string(s: &str) -> Self {
                Self(Augmented::from_string(s))
            }

            #[must_use]
            pub fn p_free(&self) -> bool {
                self.outcome() != Outcome::P
                    && self.left().all(Self::p_free)
                    && self.right().all(Self::p_free)
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

            fn left_canonical(&self) -> (Vec<Self>, bool) {
                let mut tomb = false;
                let mut reversed = vec![];
                let mut check_revers: Vec<_> = self.left().cloned().collect();
                while !check_revers.is_empty() {
                    if let Some(i) = self.left_revers(&check_revers[0].clone()) {
                        if !tomb && i.is_empty() {
                            tomb = true;
                        }
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

                (left, tomb)
            }

            fn right_canonical(&self) -> (Vec<Self>, bool) {
                let temp = self.conjugate().left_canonical();
                (temp.0.iter().map(Self::conjugate).collect(), temp.1)
            }

            #[must_use]
            pub fn canonical(&self) -> Self {
                let mut left = vec![];
                let (left_canonical, left_t) = self.left_canonical();
                for l in &left_canonical {
                    left.push(l.canonical().0);
                }

                let mut right = vec![];
                let (right_canonical, right_t) = self.right_canonical();
                for r in &right_canonical {
                    right.push(r.canonical().0);
                }

                let mut g = Augmented::new_with_tomb(
                    &left,
                    &right,
                    Tombstones::new_with(
                        left_t || self.0.left_tombstone(),
                        right_t || self.0.right_tombstone(),
                    ),
                );

                // TODO: shouldn't clone g here
                if g.left_tombstone() {
                    g.set_left_tomb(false);
                    if !$wrapper(g.clone()).left_strong() {
                        g.set_left_tomb(true);
                    }
                }

                // TODO: shouldn't clone g here
                if g.right_tombstone() {
                    g.set_right_tomb(false);
                    if !$wrapper(g.clone()).right_strong() {
                        g.set_right_tomb(true);
                    }
                }

                Self(g)
            }

            #[must_use]
            pub fn is_integer(&self) -> Option<i64> {
                let g = self.canonical();

                #[allow(clippy::cast_possible_wrap)]
                let mut b = g.birthday() as i64;

                if g.left_end() {
                    b *= -1;
                }

                if g == Self(Augmented::integer(b)) {
                    return Some(b);
                }

                None
            }

            #[must_use]
            pub fn is_star(&self) -> Option<usize> {
                let g = self.canonical();

                let b = g.birthday();

                if g == Self(Augmented::n_star(b)) {
                    return Some(b);
                }

                None
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

                if self.0.left_tombstone() {
                    rep.push('\u{220E}');
                    if !self.left_end() {
                        rep.push(',');
                    }
                }

                rep.push_str(
                    &self
                        .left()
                        .map(|g_l| format!("{},", g_l.sensible_fmt()))
                        .sorted_unstable()
                        .collect::<String>(),
                );

                if !self.left_end() {
                    rep = rep[..rep.len() - 1].to_string();
                }

                rep.push('|');

                rep.push_str(
                    &self
                        .right()
                        .map(|g_r| format!("{},", g_r.sensible_fmt()))
                        .sorted_unstable()
                        .collect::<String>(),
                );

                if self.0.right_tombstone() {
                    rep.push('\u{220E}');
                } else if !self.right_end() {
                    rep = rep[..rep.len() - 1].to_string();
                }

                format!("{{{rep}}}")
            }
        }

        impl Debug for $wrapper {
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

                if self.0.left_tombstone() {
                    rep.push('\u{220E}');
                    if !self.left_end() {
                        rep.push(',');
                    }
                }

                for g in self.left() {
                    rep.push_str(&format!("{g:?},"));
                }
                if !self.left_end() {
                    rep = rep[..rep.len() - 1].to_string();
                }

                rep.push('|');

                for g in self.right() {
                    rep.push_str(&format!("{g:?},"));
                }

                if self.0.right_tombstone() {
                    rep.push('\u{220E}');
                } else if !self.right_end() {
                    rep = rep[..rep.len() - 1].to_string();
                }

                write!(f, "{{{rep}}}")
            }
        }
    };
}
