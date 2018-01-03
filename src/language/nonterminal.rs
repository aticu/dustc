//! This module is supposed to define non-terminal symbols for the language
//! grammar.

use language::token::Token;
use parser::grammar::symbol::{IntoSymbol, Symbol};
use std::fmt::{Display, Formatter, Result};

/// This enum defines all the possible non-terminal symbols in the language
/// grammar.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Nonterminal {
    /// The symbol represents the whole program.
    Module,
    /// The symbol represents an expression.
    Expression,
    /// The symbol represents a statement.
    Statement,
    /// The symbol represents multiple statements.
    Statements,
    /// The symbol represents a block of statements with an optional expression
    /// at the end.
    Block,
    /// The symbol represents a type.
    Type,
    /// The symbol represents a function.
    Function,
    /// The symbols represents anything that can be at the top level of a
    /// module.
    ModuleContent
}

impl IntoSymbol<Nonterminal, Token> for Nonterminal {
    fn into_symbol(self) -> Symbol<Nonterminal, Token> {
        Symbol::Nonterminal(self)
    }
}

impl Display for Nonterminal {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            &Nonterminal::Module => write!(f, "a module"),
            &Nonterminal::Expression => write!(f, "an expression"),
            &Nonterminal::Statement => write!(f, "a statement"),
            &Nonterminal::Statements => write!(f, "multiple statements"),
            &Nonterminal::Block => write!(f, "a block"),
            &Nonterminal::Type => write!(f, "a type"),
            &Nonterminal::Function => write!(f, "a function"),
            &Nonterminal::ModuleContent => write!(f, ""),
        }
    }
}
