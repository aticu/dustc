//! This module defines all the errors the compiler can output.

use problem_reporting::ProblemSummary;
use problem_reporting::ProblemType::*;
use std::sync::Arc;

/// The problem summary number for an unknown token.
pub const UNKNOWN_TOKEN: usize = 1;

/// The problem summary number for an unmatched string.
pub const UNMATCHED_STRING: usize = 2;

/// The problem summary number for an unknown escape sequence.
pub const UNKNOWN_ESCAPE_SEQUENCE: usize = 3;

/// The problem summary number for an unexpected token.
pub const UNEXPECTED_TOKEN: usize = 4;

lazy_static! {
    /// This defines the problems that can occur within the language.
    pub static ref PROBLEMS: Vec<Arc<ProblemSummary>> = vec![
        Arc::new(ProblemSummary::new(
            "Dummy Problem",
            "I am a dummy problem to make sure the problem numbers start at 1.",
            Warning)),

        Arc::new(ProblemSummary::new(
            "Unrecognizable token",
            "The character pointed to is not a valid character of a token.",
            Error)),

        Arc::new(ProblemSummary::new(
            "Unmatched string start",
            concat!("The start of the string token at the specified position",
            "is not properly matched with an ending \"."),
            Error)),

        Arc::new(ProblemSummary::new(
            "Unknown escape sequence",
            "The escape sequence used is not valid.",
            Error)),

        Arc::new(ProblemSummary::new(
            "Unexpected token: expected",
            concat!("None of the rules of the language allowed for the given token ",
                    "to be used in that position."),
            Error))
    ];
}
