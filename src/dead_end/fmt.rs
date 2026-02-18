use itertools::Itertools;
use std::fmt;

use super::dead_end::DeadEnd;

const SUBSCRIPT_DIGITS: [char; 10] = ['₀', '₁', '₂', '₃', '₄', '₅', '₆', '₇', '₈', '₉'];

fn usize_to_subscript(mut n: usize) -> String {
    if n == 0 {
        return SUBSCRIPT_DIGITS[0].to_string();
    }

    let mut digits = Vec::new();
    while n > 0 {
        digits.push(SUBSCRIPT_DIGITS[n % 10]);
        n /= 10;
    }

    digits.iter().rev().collect()
}

/// Formats integers and canonical waiting games specially.
impl fmt::Display for DeadEnd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some((a, b)) = self.integer_part() {
            if b.options().is_empty() {
                return write!(f, "{a}");
            }
            if a > 0 {
                return write!(f, "{a}+{b}");
            }
        }

        if let Some(a) = self.is_waiting() {
            return write!(f, "W{}", usize_to_subscript(a));
        }

        let rep = self.options().iter().map(|g| format!("{g}")).join(",");
        write!(f, "{{{rep}}}")
    }
}

/// Formats integers specially.
impl fmt::Debug for DeadEnd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(a) = self.is_integer() {
            return write!(f, "{a}");
        }

        let rep = self.options().iter().map(|g| format!("{g:?}")).join(",");
        write!(f, "{{{rep}}}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display_waiting_uses_subscripts() {
        assert_eq!(DeadEnd::waiting(10).to_string(), "W₁₀");
    }
}
