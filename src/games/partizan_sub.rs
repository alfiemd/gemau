use crate::core::ShortOwned;
use crate::core::ShortPartizanOwned;

struct PartizanSub {
    heap: usize,
    left: Vec<usize>,
    right: Vec<usize>,
}

impl ShortOwned for PartizanSub {
    fn options(&self) -> impl IntoIterator<Item = Self> {
        self.left()
            .into_iter()
            .chain(self.right())
            .collect::<Vec<_>>()
    }
}

impl ShortPartizanOwned for PartizanSub {
    fn left(&self) -> impl IntoIterator<Item = Self> {
        if self.heap == 0 {
            return vec![];
        }

        let mut moves = vec![];
        for l in &self.left {
            if self.heap >= *l {
                moves.push(PartizanSub {
                    heap: self.heap - l,
                    left: self.left.clone(),
                    right: self.right.clone(),
                });
            }
        }

        moves
    }

    fn right(&self) -> impl IntoIterator<Item = Self> {
        if self.heap == 0 {
            return vec![];
        }

        let mut moves = vec![];
        for r in &self.right {
            if self.heap >= *r {
                moves.push(PartizanSub {
                    heap: self.heap - r,
                    left: self.left.clone(),
                    right: self.right.clone(),
                });
            }
        }

        moves
    }
}
