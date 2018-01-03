//! This module defines the syntactic category of statements in the target
//! language.

use super::expression::Expression;

/// This enum represents a Statement in the AST.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    /// Represents an expression.
    Expression(Expression),
    /// Represents the empty statement.
    Empty
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
