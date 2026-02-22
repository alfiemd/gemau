use chumsky::prelude::*;
use std::fmt;
use std::str::FromStr;

use super::form::DeadEnd;

const SUBSCRIPT_DIGITS: [char; 10] = ['₀', '₁', '₂', '₃', '₄', '₅', '₆', '₇', '₈', '₉'];

fn subscript_digit_to_usize(ch: char) -> Option<usize> {
    SUBSCRIPT_DIGITS.iter().position(|digit| *digit == ch)
}

fn subscript_to_usize(s: &str) -> Option<usize> {
    if s.is_empty() {
        return None;
    }

    let mut value = 0_usize;
    for ch in s.chars() {
        let digit = subscript_digit_to_usize(ch)?;
        value = value.checked_mul(10)?.checked_add(digit)?;
    }

    Some(value)
}

fn dead_end_parser<'src>() -> impl Parser<'src, &'src str, DeadEnd, extra::Err<Rich<'src, char>>> {
    recursive(|expr| {
        let integer = any()
            .filter(|ch: &char| ch.is_ascii_digit())
            .repeated()
            .at_least(1)
            .collect::<String>()
            .try_map(|digits, span| {
                digits
                    .parse::<usize>()
                    .map(DeadEnd::integer)
                    .map_err(|_| Rich::custom(span, "invalid integer rank"))
            });

        let subscript_digit = one_of(SUBSCRIPT_DIGITS);
        let waiting = just('W')
            .ignore_then(subscript_digit.repeated().at_least(1).collect::<String>())
            .try_map(|subscript_digits, span| {
                subscript_to_usize(&subscript_digits)
                    .map(DeadEnd::waiting)
                    .ok_or_else(|| {
                        Rich::custom(span, "waiting games use subscript digits after 'W'")
                    })
            });

        let braced = expr
            .separated_by(just(',').padded())
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just('{').padded(), just('}').padded())
            .map(DeadEnd::with_options);

        let term = choice((braced, waiting, integer)).padded();

        term.clone().foldl(
            just('+').padded().ignore_then(term).repeated(),
            |left, right| left + right,
        )
    })
    .then_ignore(end())
}

/// Error returned when parsing a [`DeadEnd`] from text fails.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseDeadEndError {
    message: String,
}

impl ParseDeadEndError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl fmt::Display for ParseDeadEndError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for ParseDeadEndError {}

impl FromStr for DeadEnd {
    type Err = ParseDeadEndError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        dead_end_parser().parse(s).into_result().map_err(|errs| {
            // Chumsky can emit multiple errors; use the first as the primary diagnostic.
            let message = errs
                .first()
                .map(|err| err.to_string())
                .unwrap_or_else(|| "invalid dead end syntax".to_string());
            ParseDeadEndError::new(message)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_integer() {
        assert_eq!(DeadEnd::from_str("97").unwrap(), DeadEnd::integer(97));
    }

    #[test]
    fn parse_waiting_game() {
        assert_eq!(DeadEnd::from_str("W₁₀").unwrap(), DeadEnd::waiting(10));
    }

    #[test]
    fn parse_braced_recursive() {
        let parsed = DeadEnd::from_str("{0,W₂,{1,W₃}}").unwrap();
        let expected = DeadEnd::with_options([
            DeadEnd::ZERO,
            DeadEnd::waiting(2),
            DeadEnd::with_options([DeadEnd::integer(1), DeadEnd::waiting(3)]),
        ]);

        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_display_round_trip() {
        let g = DeadEnd::integer(2) + DeadEnd::waiting(3);
        assert_eq!(g.to_string().parse::<DeadEnd>().unwrap(), g);
    }

    #[test]
    fn parse_general_sum() {
        let parsed = DeadEnd::from_str("W₂+{0,W₁}+3").unwrap();
        let expected =
            DeadEnd::waiting(2) + DeadEnd::with_options([DeadEnd::ZERO, DeadEnd::waiting(1)]) + 3;

        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_sum_inside_braces() {
        let parsed = DeadEnd::from_str("{0+W₂,1+{W₂,0}}").unwrap();
        let expected = DeadEnd::with_options([
            DeadEnd::ZERO + DeadEnd::waiting(2),
            DeadEnd::integer(1) + DeadEnd::with_options([DeadEnd::waiting(2), DeadEnd::ZERO]),
        ]);

        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_rejects_non_subscript_waiting_rank() {
        assert!(DeadEnd::from_str("W10").is_err());
    }

    #[test]
    fn parse_rejects_empty_input() {
        assert!(DeadEnd::from_str("").is_err());
    }

    #[test]
    fn parse_rejects_leading_option_comma() {
        assert!(DeadEnd::from_str("{,0}").is_err());
    }

    #[test]
    fn parse_accepts_trailing_option_comma() {
        let parsed = DeadEnd::from_str("{0,}").unwrap();
        let expected = DeadEnd::integer(1);

        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_rejects_unclosed_brace() {
        assert!(DeadEnd::from_str("{0,W₂").is_err());
    }

    #[test]
    fn parse_rejects_dangling_sum_operator() {
        assert!(DeadEnd::from_str("W₂+").is_err());
    }

    #[test]
    fn parse_rejects_leading_sum_operator() {
        assert!(DeadEnd::from_str("+W₂").is_err());
    }

    #[test]
    fn parse_reports_integer_rank_overflow() {
        let err = DeadEnd::from_str(&"9".repeat(128)).unwrap_err().to_string();

        assert!(
            err.contains("invalid integer rank"),
            "unexpected parse error: {err}"
        );
    }

    #[test]
    fn parse_reports_waiting_rank_overflow() {
        let input = format!("W{}", "₉".repeat(128));
        let err = DeadEnd::from_str(&input).unwrap_err().to_string();

        assert!(
            err.contains("waiting games use subscript digits after 'W'"),
            "unexpected parse error: {err}"
        );
    }
}
