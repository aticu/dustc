//! This module is supposed to define the finite automata used in the parser.

pub mod nfa;
pub mod dfa;

use std::fmt;
use std::sync::Mutex;

/// Represents a state in an NFA.
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct State(u64);

lazy_static! {
    /// A counter to steadily increase the numbers of states.
    static ref CURRENT_STATE_NUM: Mutex<u64> = Mutex::new(0);
}

impl State {
    /// Creates a new state with a unique number.
    pub fn new() -> State {
        let mut num = CURRENT_STATE_NUM
            .lock()
            .expect("The NFA state creation mutex is corrupted");

        *num += 1;

        assert!(*num < u64::max_value());

        State(*num)
    }
}

/// Represents a transition in an automaton.
#[derive(PartialEq, Eq, Hash, Clone)]
pub enum Transition<Symbol: Eq + Clone> {
    /// Corresponds to a direct transition on the given symbol.
    Direct(State, Symbol, State),
    /// Corresponds to an epsilon transition.
    Epsilon(State, State),
    /// Corresponds to an indirect transition on every symbol but the given one.
    Indirect(State, Vec<Symbol>, State),
}

impl<Symbol: Eq + Clone + fmt::Debug> fmt::Debug for Transition<Symbol> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Transition::Direct(from, ref symbol, to) => {
                write!(f, "{:?} -{:?}-> {:?}", from, symbol, to)
            }
            &Transition::Epsilon(from, to) => write!(f, "{:?} -> {:?}", from, to),
            &Transition::Indirect(from, ref symbols, to) => {
                write!(f, "{:?} -!{:?}-> {:?}", from, symbols, to)
            }
        }
    }
}

impl<Symbol: Eq + Clone> Transition<Symbol> {
    /// Creates a new transition.
    pub fn new(from: State, symbol: Option<Symbol>, to: State) -> Transition<Symbol> {
        match symbol {
            Some(symbol) => Transition::Direct(from, symbol, to),
            None => Transition::Epsilon(from, to),
        }
    }

    /// Creates a new indirect transition on all symbols but the given ones.
    pub fn new_indirect(from: State, symbols: Vec<Symbol>, to: State) -> Transition<Symbol> {
        Transition::Indirect(from, symbols, to)
    }

    /// Returns the state this transition is from.
    pub fn from(&self) -> State {
        match self {
            &Transition::Direct(ref from, _, _) => *from,
            &Transition::Epsilon(ref from, _) => *from,
            &Transition::Indirect(ref from, _, _) => *from,
        }
    }

    /// Returns the state this transition is to.
    pub fn to(&self) -> State {
        match self {
            &Transition::Direct(_, _, ref to) => *to,
            &Transition::Epsilon(_, ref to) => *to,
            &Transition::Indirect(_, _, ref to) => *to,
        }
    }

    /// Checks if the transition matches the given symbol.
    pub fn matches(&self, symbol: &Symbol) -> bool {
        match self {
            &Transition::Direct(_, ref transition_symbol, _) => transition_symbol == symbol,
            &Transition::Epsilon(_, _) => false,
            &Transition::Indirect(_, ref symbol_list, _) => !symbol_list.contains(symbol),
        }
    }

    /// Returns the symbols relevant to this transition.
    pub fn relevant_symbols(&self) -> Vec<Symbol> {
        match self {
            &Transition::Direct(_, ref transition_symbol, _) => vec![transition_symbol.clone()],
            &Transition::Epsilon(_, _) => Vec::new(),
            &Transition::Indirect(_, ref symbol_list, _) => symbol_list.clone(),
        }
    }

    /// Returns true only if the transition is an epsilon transition.
    pub fn is_epsilon(&self) -> bool {
        match self {
            &Transition::Direct(_, _, _) => false,
            &Transition::Epsilon(_, _) => true,
            &Transition::Indirect(_, _, _) => false,
        }
    }

    /// Returns true if the transition matches any implicit symbol.
    pub fn matches_rest(&self) -> bool {
        match self {
            &Transition::Direct(_, _, _) => false,
            &Transition::Epsilon(_, _) => false,
            &Transition::Indirect(_, _, _) => true,
        }
    }
}
