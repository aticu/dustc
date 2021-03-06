//! This module is supposed to define a type for modelling tokens.

use std::fmt::{Display, Formatter, Result};

/// The possible types of parentheses.
#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub enum ParenthesesType {
    /// Opening parentheses.
    Opening,
    /// Closing parentheses.
    Closing
}

/// This enumeration specifies all the token types in the target language.
#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub enum Token {
    /// Represents an integer.
    Integer(String),
    /// Represents a keyword in the language.
    Keyword(String),
    /// Represents an identifier in the language.
    Identifier(String),
    /// Represents a string in the language.
    String(String),
    /// Represents an operator in the language.
    Operator(String),
    /// Represents a statement separator in the language.
    StatementSeparator,
    /// Represents parenthesis (´(´ and ´)´).
    Parentheses(ParenthesesType),
    /// Represents brackets (´[´ and ´]´).
    Brackets(ParenthesesType),
    /// Represents braces (´{´ and ´}´).
    Braces(ParenthesesType)
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "`")?;
        match self {
            &Token::Integer(ref int) => write!(f, "{}", int),
            &Token::Keyword(ref keyword) => write!(f, "{}", keyword),
            &Token::Identifier(ref ident) => write!(f, "{}", ident),
            &Token::String(ref string) => write!(f, "\"{}\"", string),
            &Token::Operator(ref operator) => write!(f, "{}", operator),
            &Token::StatementSeparator => write!(f, ";"),
            &Token::Parentheses(ref kind) => {
                write!(f,
                       "{}",
                       if *kind == ParenthesesType::Opening {
                           "("
                       } else {
                           ")"
                       })
            },
            &Token::Brackets(ref kind) => {
                write!(f,
                       "{}",
                       if *kind == ParenthesesType::Opening {
                           "["
                       } else {
                           "]"
                       })
            },
            &Token::Braces(ref kind) => {
                write!(f,
                       "{}",
                       if *kind == ParenthesesType::Opening {
                           "{"
                       } else {
                           "}"
                       })
            },
        }?;
        write!(f, "`")
    }
}
