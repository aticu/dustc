//! This module is supposed to define non-terminal symbols for the language grammer.

use language::token::Token;
use parser::grammer::symbol::{Symbol, IntoSymbol};

/// This enum defines all the possible non-terminal symbols in the language grammer.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Nonterminal {
    /// The symbol represents the whole program.
    Program,
    /// The symbol represents an expression.
    Expression,
}

impl IntoSymbol<Nonterminal, Token> for Nonterminal {
    fn into_symbol(self) -> Symbol<Nonterminal, Token> {
        Symbol::Nonterminal(self)
    }
}
