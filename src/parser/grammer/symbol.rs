//! This module is supposed to define a symbol used within a grammer.

use std::fmt::Debug;
use std::hash::Hash;

/// Represents a terminal symbol in the grammer.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TerminalMatcher<Terminal>(fn(Terminal) -> bool)
    where Terminal: Clone + Debug + Eq + Hash;

impl<Terminal> TerminalMatcher<Terminal>
    where Terminal: Clone + Debug + Eq + Hash
{
    /// Creates a new terminal matcher.
    pub fn new(match_fn: fn(Terminal) -> bool) -> TerminalMatcher<Terminal> {
        TerminalMatcher(match_fn)
    }

    /// Checks if the given terminal matches.
    fn matches(&self, terminal: &Terminal) -> bool {
        self.0(terminal.clone())
    }
}

impl<Nonterminal, Terminal> IntoSymbol<Nonterminal, Terminal> for TerminalMatcher<Terminal>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash
{
    fn into_symbol(self) -> Symbol<Nonterminal, Terminal> {
        Symbol::Terminal(self)
    }
}

/// Creates a new ´TerminalMatcher´.
macro_rules! terminal_matcher {
    ($($pattern: pat => $result: expr),*) => {{
        use $crate::parser::grammer::symbol::TerminalMatcher;
        TerminalMatcher::new(|terminal| {
            match terminal {
                $(
                    $pattern => $result,
                )*
                _ => false
            }
        })
    }};
    ($($pattern: pat),*) => {{
        use $crate::parser::grammer::symbol::TerminalMatcher;
        TerminalMatcher::new(|terminal| {
            match terminal {
                $(
                    $pattern => true,
                )*
                _ => false
            }
        })
    }}
}

/// Creates a symbol out of the implementing type.
pub trait IntoSymbol<Nonterminal, Terminal>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash
{
    /// This method creates the symbol.
    fn into_symbol(self) -> Symbol<Nonterminal, Terminal>;
}

/// Represents a symbol in the grammer.
#[derive(Clone, Debug, Hash)]
pub enum Symbol<Nonterminal, Terminal>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash
{
    /// Represents a non-terminal symbol.
    Nonterminal(Nonterminal),
    /// Represents a terminal symbol.
    Terminal(TerminalMatcher<Terminal>),
    /// Represents a non-terminal symbol for internal use.
    InternalNonterminal(u64),
    /// Represents an input symbol during parsing.
    InputSymbol(Terminal),
}

impl<Nonterminal, Terminal> PartialEq for Symbol<Nonterminal, Terminal>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash
{
    fn eq(&self, other: &Symbol<Nonterminal, Terminal>) -> bool {
        use self::Symbol::*;
        match self {
            &Nonterminal(ref nonterminal) => {
                match other {
                    &Nonterminal(ref other_nonterminal) => nonterminal == other_nonterminal,
                    _ => false,
                }
            }
            &InternalNonterminal(ref nonterminal) => {
                match other {
                    &InternalNonterminal(ref other_nonterminal) => nonterminal == other_nonterminal,
                    _ => false,
                }
            }
            &Terminal(ref terminal_matcher) => {
                match other {
                    &InputSymbol(ref terminal) => terminal_matcher.matches(terminal),
                    &Terminal(ref other_terminal_matcher) => {
                        other_terminal_matcher == terminal_matcher
                    }
                    _ => false,
                }
            }
            &InputSymbol(ref terminal) => {
                match other {
                    &Terminal(ref terminal_matcher) => terminal_matcher.matches(terminal),
                    &InputSymbol(_) => panic!("Trying to directly compare two input symbols"),
                    _ => false,
                }
            }
        }
    }
}

impl<Nonterminal, Terminal> Eq for Symbol<Nonterminal, Terminal>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash
{
}

impl<Nonterminal, Terminal> Symbol<Nonterminal, Terminal>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash
{
    /// Returns true if this symbol is a symbol matcher definition.
    pub fn is_matcher(&self) -> bool {
        match self {
            &Symbol::Nonterminal(_) => true,
            &Symbol::Terminal(_) => true,
            _ => false,
        }
    }

    /// Returns true if this symbol is a nonterminal symbol.
    pub fn is_nonterminal(&self) -> bool {
        match self {
            &Symbol::Nonterminal(_) => true,
            &Symbol::InternalNonterminal(_) => true,
            _ => false,
        }
    }
}
