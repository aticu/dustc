//! This module is supposed to define an Abstract Syntax Tree (or AST).

use super::expression::Expression;
use super::statement::Statement;
use super::token::Token;
use super::types::Type;

/// This enum defines an abstract syntax tree.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AST {
    /// Represents a module.
    Module(Vec<AST>),
    /// Represents a type.
    Type(Type),
    /// Represents a function.
    Function(String, Expression),
    /// Represents a statement.
    Statement(Statement),
    /// Represents an expression.
    Expression(Expression),
    /// Represents a single token of the lexer output.
    Token(Token)
}

impl Default for AST {
    fn default() -> AST {
        AST::Module(vec![])
    }
}

impl From<Token> for AST {
    fn from(token: Token) -> AST {
        AST::Token(token)
    }
}

impl AST {
    /// Returns the contained token of the tree, if it exists.
    ///
    /// # Panics
    /// This function panics if the AST is not a token AST.
    pub fn extract_token(self) -> Token {
        match self {
            AST::Token(token) => token,
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
