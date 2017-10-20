//! This module is supposed to parse the given token string.
//!
//! The input token string should be converted into an abstract syntax tree
//! (AST) which can later be used for further processing.

#[macro_use]
pub mod grammer;

/// Corresponds to an action a parser can take upon reading a symbol.
enum ParserAction {
    /// The shift action just pushes the next input symbol onto the stack.
    Shift,
    /// The go action pushes a nonterminal onto the stack.
    Go,
    /// The reduce action reduces the top of the stack using a production.
    Reduce,
    /// The accept action signals the successful completion of parsing.
    Accept,
}
