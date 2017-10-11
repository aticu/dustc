//! This module is supposed to implement deterministic finite automata (DFAs).

use std::collections::HashMap;
use super::State;

/// This struct represents a deterministic finite automaton (DFA).
#[derive(Debug, Clone)]
pub struct DFA<T: Clone> {
    /// Represents the transition function.
    ///
    /// A map from the current state and the next character to the next state.
    transition_fn: HashMap<(State, char), State>,
    /// The starting state of the DFA.
    starting_state: State,
    /// The set of accepting states.
    accepting_states: HashMap<State, T>
}

impl<T: Clone> DFA<T> {
    /// Creates a new DFA.
    pub fn new(transition_fn: HashMap<(State, char), State>, starting_state: State, accepting_states: HashMap<State, T>) -> DFA<T> {
        DFA {
            transition_fn,
            starting_state,
            accepting_states
        }
    }

    /// Calculates the transition function for the given state and symbol.
    pub fn transition(&self, state: State, character: char) -> Option<State> {
        self.transition_fn.get(&(state, character)).map(|&state| state)
    }

    /// Returns the the given state maps to if it is accepting.
    ///
    /// If the state is not accepting, ´None´ is returned.
    pub fn get_accepting_value(&self, state: State) -> Option<T> {
        self.accepting_states.get(&state).map(|value| value.clone())
    }

    /// Returns true if the given state is an accepting state.
    pub fn is_accepting(&self, state: State) -> bool {
        self.accepting_states.contains_key(&state)
    }

    /// Starts this DFA.
    ///
    /// This just returns the starting state.
    pub fn start(&self) -> State {
        self.starting_state
    }
}
