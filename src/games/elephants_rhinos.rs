use crate::ShortOwned;
use crate::ShortPartizanOwned;

pub struct EleRhi {
    elephants: usize,
    elephant_moves: usize,
    middle_spaces: usize,
    rhino_moves: usize,
    rhinos: usize,
}

impl EleRhi {
    #[must_use]
    pub fn new(
        elephants: usize,
        elephant_moves: usize,
        middle_spaces: usize,
        rhino_moves: usize,
        rhinos: usize,
    ) -> Self {
        Self {
            elephants,
            elephant_moves,
            middle_spaces,
            rhino_moves,
            rhinos,
        }
    }
}

impl ShortOwned for EleRhi {
    fn options(&self) -> impl IntoIterator<Item = Self> {
        self.left()
            .into_iter()
            .chain(self.right())
            .collect::<Vec<_>>()
    }
}

impl ShortPartizanOwned for EleRhi {
    fn left(&self) -> impl IntoIterator<Item = Self> {
        let mut options = vec![];

        if self.middle_spaces > 0 {
            options.push(Self::new(
                self.elephants,
                self.elephant_moves + self.elephants - 1,
                self.middle_spaces - 1,
                self.rhino_moves,
                self.rhinos,
            ));
        }

        if self.elephant_moves > 0 {
            options.push(Self::new(
                self.elephants,
                self.elephant_moves - 1,
                self.middle_spaces,
                self.rhino_moves,
                self.rhinos,
            ));
        }

        options
    }

    fn right(&self) -> impl IntoIterator<Item = Self> {
        let mut options = vec![];

        if self.middle_spaces > 0 {
            options.push(Self::new(
                self.elephants,
                self.elephant_moves,
                self.middle_spaces - 1,
                self.rhino_moves + self.rhinos - 1,
                self.rhinos,
            ));
        }

        if self.rhino_moves > 0 {
            options.push(Self::new(
                self.elephants,
                self.elephant_moves,
                self.middle_spaces,
                self.rhino_moves - 1,
                self.rhinos,
            ));
        }

        options
    }
}
