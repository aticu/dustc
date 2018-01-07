//! This module defines the syntactic category of expressions in the target
//! language.

use super::statement::Statement;
use problem_reporting::InputPosition;

/// This enum represents an expression in the AST.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    /// Represents an integer literal.
    Integer(String),
    /// Represents an identifier.
    Identifier(String),
    /// Represents the addition of two expressions.
    Addition(Box<Expression>, Box<Expression>),
    /// Represents the subtraction of two expressions.
    Subtraction(Box<Expression>, Box<Expression>),
    /// Represents the multiplication of two expressions.
    Multiplication(Box<Expression>, Box<Expression>),
    /// Represents the division of two expressions.
    Division(Box<Expression>, Box<Expression>),
    /// Represents the negation of an expression.
    Negation(Box<Expression>),
    /// Represents a list of statements possibly followed by an expression.
    Block(Vec<Statement>, Option<Box<Expression>>)
}
