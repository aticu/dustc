//! This module is supposed to define an Abstract Syntax Tree (or AST).

use super::expression::Expression;
use super::statement::Statement;
use super::token::Token;
use super::types::Type;
use problem_reporting::{InputPosition, Locatable};

/// This enum defines an abstract syntax tree.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AST {
    /// Represents a default AST. Not a meaningful value.
    Default,
    /// Represents a module.
    Module(Vec<AST>, InputPosition),
    /// Represents a type.
    Type(Type),
    /// Represents a function.
    Function(String, Expression, InputPosition),
    /// Represents a statement.
    Statement(Statement),
    /// Represents an expression.
    Expression(Expression),
    /// Represents a single token of the lexer output.
    Token(Token, InputPosition)
}

impl Default for AST {
    fn default() -> AST {
        AST::Default
    }
}

impl From<(Token, InputPosition)> for AST {
    fn from(input: (Token, InputPosition)) -> AST {
        let (token, position) = input;
        AST::Token(token, position)
    }
}

impl AST {
    /// Returns the contained token of the tree, if it exists.
    ///
    /// # Panics
    /// This function panics if the AST is not a token AST.
    pub fn extract_token(self) -> (Token, InputPosition) {
        match self {
            AST::Token(token, position) => (token, position),
            _ => panic!("Trying to extract a token from a non token AST. This is a bug."),
        }
    }

    /// Returns the contained expression of the tree, if it exists.
    ///
    /// # Panics
    /// This function panics if the AST is not an expression AST.
    pub fn extract_expression(self) -> Expression {
        match self {
            AST::Expression(expression) => expression,
            _ => {
                panic!(concat!("Trying to extract an expression from a non expression AST.",
                               "This is a bug."))
            },
        }
    }

    /// Returns the contained statement of the tree, if it exists.
    ///
    /// # Panics
    /// This function panics if the AST is not an statement AST.
    pub fn extract_statement(self) -> Statement {
        match self {
            AST::Statement(statement) => statement,
            _ => panic!("Trying to extract a statement from a non statement AST. This is a bug."),
        }
    }
}

impl Locatable for AST {
    fn get_input_position(&self) -> &InputPosition {
        match self {
            &AST::Default => unreachable!("The value of a default AST should never exist."),
            &AST::Module(_, ref position) => position,
            &AST::Type(ref types) => types.get_input_position(),
            &AST::Function(_, _, ref position) => position,
            &AST::Statement(ref statement) => statement.get_input_position(),
            &AST::Expression(ref expression) => expression.get_input_position(),
            &AST::Token(_, ref position) => position
        }
    }
}
