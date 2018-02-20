//! This module defines the syntactic category of expressions in the target
//! language.

use super::statement::Statement;
use problem_reporting::{InputPosition, Locatable};

/// This enum represents an expression in the AST.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    /// Represents an integer literal.
    Integer(String, InputPosition),
    /// Represents an identifier.
    Identifier(String, InputPosition),
    /// Represents the addition of two expressions.
    Addition(Box<Expression>, Box<Expression>, InputPosition),
    /// Represents the subtraction of two expressions.
    Subtraction(Box<Expression>, Box<Expression>, InputPosition),
    /// Represents the multiplication of two expressions.
    Multiplication(Box<Expression>, Box<Expression>, InputPosition),
    /// Represents the division of two expressions.
    Division(Box<Expression>, Box<Expression>, InputPosition),
    /// Represents the negation of an expression.
    Negation(Box<Expression>, InputPosition),
    /// Represents a list of statements possibly followed by an expression.
    Block(Vec<Statement>, Option<Box<Expression>>, InputPosition)
}

impl Locatable for Expression {
    fn get_input_position(&self) -> &InputPosition {
        match self {
            &Expression::Integer(_, ref position) => position,
            &Expression::Identifier(_, ref position) => position,
            &Expression::Addition(_, _, ref position) => position,
            &Expression::Subtraction(_, _, ref position) => position,
            &Expression::Multiplication(_, _, ref position) => position,
            &Expression::Division(_, _, ref position) => position,
            &Expression::Negation(_, ref position) => position,
            &Expression::Block(_, _, ref position) => position
        }
    }
}
