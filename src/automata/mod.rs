//! This module is supposed to define the finite automata used in the parser.

pub mod nfa;
pub mod dfa;

use std::sync::Mutex;

/// Represents a state in an NFA.
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct State(u64);

lazy_static! {
    static ref CURRENT_STATE_NUM: Mutex<u64> = Mutex::new(0);
}

impl State {
    /// Creates a new state with a unique number.
    pub fn new() -> State {
        let mut num = CURRENT_STATE_NUM.lock().expect("The NFA state creation mutex is corrupted");

        *num += 1;

        assert!(*num < u64::max_value());

        State(*num)
    }
}

/// Represents a transition in an automaton.
pub struct Transition<T: Eq> {
    /// The state the transition starts at.
    from: State,
    /// The symbol the transition is used at.
    symbol: T,
    /// The state that is being transitioned to.
    to: State
}
