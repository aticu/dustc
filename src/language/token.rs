//! This module is supposed to define a type for modelling tokens.

/// The possible types of parentheses.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum ParenthesesType {
    /// Opening parentheses.
    Opening,
    /// Closing parentheses.
    Closing
}

/// This enumeration specifies all the token types in the target language.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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
