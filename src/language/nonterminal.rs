//! This module is supposed to define non-terminal symbols for the language
//! grammar.

use language::token::Token;
use parser::grammar::symbol::{IntoSymbol, Symbol};

/// This enum defines all the possible non-terminal symbols in the language
/// grammar.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Nonterminal {
    /// The symbol represents the whole program.
    Program,
    /// The symbol represents an expression.
    Expression
}

impl IntoSymbol<Nonterminal, Token> for Nonterminal {
    fn into_symbol(self) -> Symbol<Nonterminal, Token> {
        Symbol::Nonterminal(self)
    }
}
