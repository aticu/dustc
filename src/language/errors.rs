//! This module defines all the errors the compiler can output.

use ::problem_reporting::ProblemDescription;
use ::problem_reporting::ProblemType::*;

lazy_static! {
    /// This defines the problems that can occur within the language.
    pub static ref PROBLEMS: Vec<ProblemDescription> = vec![
        ProblemDescription::new("unrecognizable token", "The character pointed to is not a valid character of a token.", Error),
    ];
}
