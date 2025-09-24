#[macro_export]
macro_rules! impl_short {
    ($trait_name:ident, $sub_name:ident, $item_type:ty, $iter_trait:ident, $($sized:tt)?) => {
        pub trait $trait_name $(: $sized)? {
            fn options(&self) -> impl $iter_trait<Item = $item_type>;

            fn birthday(&self) -> usize {
                self.options().into_iter().map(|b| b.birthday() + 1).max().unwrap_or(0)
            }

            fn race(&self) -> usize {
                self.options().into_iter().map(|r| r.race() + 1).min().unwrap_or(0)
            }

            fn terminal_lengths(&self) -> Vec<usize> {
                let mut lengths: Vec<usize> = self
                    .options()
                    .into_iter()
                    .flat_map(|x| x.terminal_lengths())
                    .map(|len| len + 1)
                    .fold(Vec::new(), |mut acc, len| {
                        if !acc.contains(&len) {
                            acc.push(len);
                        }
                        acc
                    });

                if lengths.is_empty() {
                    lengths.push(0);
                }

                lengths
            }
        }

        pub trait $sub_name: $trait_name {
            fn left(&self) -> impl $iter_trait<Item = $item_type>;
            fn right(&self) -> impl $iter_trait<Item = $item_type>;


            fn dicotic(&self) -> bool {
                ((self.left_end() && self.right_end())
                || (!self.left_end() && !self.right_end()))
                && self.left().into_iter().all(|x| x.dicotic())
                && self.right().into_iter().all(|x| x.dicotic())
            }

            fn left_end(&self) -> bool {
                self.left().into_iter().next().is_none()
            }

            fn right_end(&self) -> bool {
                self.right().into_iter().next().is_none()
            }

            fn is_empty(&self) -> bool {
                self.left_end() && self.right_end()
            }

            fn left_deadend(&self) -> bool {
                self.left_end() && self.right().into_iter().all(|x| x.left_deadend())
            }

            fn right_deadend(&self) -> bool {
                self.right_end() && self.left().into_iter().all(|x| x.right_deadend())
            }

            fn deadending(&self) -> bool {
                (!self.left_end() || self.left_deadend())
                    && (!self.right_end() || self.right_deadend())
                    && self.options().into_iter().all(|x| x.deadending())
            }

            fn left_blocked_end(&self) -> bool {
                self.left_end()
                    && self
                        .right().into_iter()
                        .all(|g_r| g_r.left_blocked_end() || g_r.left().into_iter().any(|x| x.left_blocked_end()))
            }

            fn right_blocked_end(&self) -> bool {
                self.right_end()
                    && self
                        .left().into_iter()
                        .all(|g_l| g_l.right_blocked_end() || g_l.right().into_iter().any(|x| x.right_blocked_end()))
            }

            fn blocking(&self) -> bool {
                (!self.left_end() || self.left_blocked_end())
                    && (!self.right_end() || self.right_blocked_end())
                    && self.options().into_iter().all(|x| x.blocking())
            }
        }
    };
}
