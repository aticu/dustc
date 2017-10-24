//! This module is supposed to define a symbol used within a grammar.

use std::fmt::{Debug, Formatter, Result};
use std::hash::Hash;

/// Represents a terminal symbol in the grammar.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TerminalMatcher<Terminal>(&'static str, fn(Terminal) -> bool)
    where Terminal: Clone + Debug + Eq + Hash;

impl<Terminal> TerminalMatcher<Terminal>
    where Terminal: Clone + Debug + Eq + Hash
{
    /// Creates a new terminal matcher.
    pub fn new(match_fn: fn(Terminal) -> bool, name: &'static str) -> TerminalMatcher<Terminal> {
        TerminalMatcher(name, match_fn)
    }

    /// Checks if the given terminal matches.
    fn matches(&self, terminal: &Terminal) -> bool {
        self.1(terminal.clone())
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

impl<Terminal> Debug for TerminalMatcher<Terminal>
    where Terminal: Clone + Debug + Eq + Hash
{
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{:?}", self.0)
    }
}

/// Creates a new ´TerminalMatcher´.
macro_rules! terminal_matcher {
    ($name: expr, $($pattern: pat => $result: expr),*) => {{
        use $crate::parser::grammar::symbol::TerminalMatcher;
        TerminalMatcher::new(|terminal| {
            match terminal {
                $(
                    $pattern => $result,
                )*
                _ => false
            }
        }, $name)
    }};
    ($name: expr, $($pattern: pat),*) => {{
        use $crate::parser::grammar::symbol::TerminalMatcher;
        TerminalMatcher::new(|terminal| {
            match terminal {
                $(
                    $pattern => true,
                )*
                _ => false
            }
        }, $name)
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

/// Represents a symbol in the grammar.
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
    /// Represents the end of the input stream.
    EndOfInput
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
            },
            &InternalNonterminal(ref nonterminal) => {
                match other {
                    &InternalNonterminal(ref other_nonterminal) => nonterminal == other_nonterminal,
                    _ => false,
                }
            },
            &Terminal(ref terminal_matcher) => {
                match other {
                    &InputSymbol(ref terminal) => terminal_matcher.matches(terminal),
                    &Terminal(ref other_terminal_matcher) => {
                        other_terminal_matcher == terminal_matcher
                    },
                    _ => false,
                }
            },
            &InputSymbol(ref terminal) => {
                match other {
                    &Terminal(ref terminal_matcher) => terminal_matcher.matches(terminal),
                    &InputSymbol(_) => panic!("Trying to directly compare two input symbols"),
                    _ => false,
                }
            },
            &EndOfInput => {
                match other {
                    &EndOfInput => true,
                    _ => false,
                }
            },
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

    pub fn get_input_symbol(&self) -> Terminal {
        match self {
            &Symbol::InputSymbol(ref symbol) => symbol.clone(),
            _ => {
                panic!(concat!("Parse bug: Trying to retrieve the input symbol from ",
                               "something that isn't an input symbol."))
            },
        }
    }
}
