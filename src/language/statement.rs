//! This module defines the syntactic category of statements in the target
//! language.

use super::expression::Expression;
use problem_reporting::{InputPosition, Locatable};

/// This enum represents a Statement in the AST.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    /// Represents an expression.
    Expression(Expression),
    /// Represents the empty statement.
    Empty(InputPosition)
}

impl Statement {
    /// Returns the contained expression of the statement or the statement
    /// itself.
    pub fn extract_expression(self) -> Result<Expression, Statement> {
        if let Statement::Expression(expression) = self {
            Ok(expression)
        } else {
            Err(self)
        }
    }
}

impl Locatable for Statement {
    fn get_input_position(&self) -> &InputPosition {
        match self {
            &Statement::Expression(ref expr) => expr.get_input_position(),
            &Statement::Empty(ref position) => position
        }
    }
}
