//! This module is supposed to define an Abstract Syntax Tree (or AST).

use super::token::Token;

/// This enum defines an abstract syntax tree.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AST {
    /// The value representing a nonexistant AST.
    None,
    /// Represents an integer literal.
    Integer(String),
    /// Represents the addition of two expressions.
    Addition(Box<AST>, Box<AST>),
    /// Represents the subtraction of two expressions.
    Subtraction(Box<AST>, Box<AST>),
    /// Represents the multiplication of two expressions.
    Multiplication(Box<AST>, Box<AST>),
    /// Represents the division of two expressions.
    Division(Box<AST>, Box<AST>),
    /// Represents the negation of an expression.
    Negation(Box<AST>),
    /// Represents a single token of the lexer output.
    Token(Token),
    TEST(Box<AST>)
}

impl Default for AST {
    fn default() -> AST {
        AST::None
    }
}

impl From<Token> for AST {
    fn from(token: Token) -> AST {
        AST::Token(token)
    }
}

impl AST {
    /// Returns the contained token of the tree, if it exists.
    pub fn token(self) -> Token {
        match self {
            AST::Token(token) => token,
            _ => panic!("Trying to get a token from a non-token AST."),
        }
    }
}
