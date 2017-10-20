//! This module is supposed to implement deterministic finite automata (DFAs).

use automata::Transition;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use super::State;

/// This struct represents a deterministic finite automaton (DFA).
#[derive(Clone)]
pub struct DFA<AcceptingValue: Clone, Symbol: Eq + Hash + Clone> {
    /// Represents the transition function.
    ///
    /// A map from the current state and the next character to the next state.
    transition_fn: Vec<Transition<Symbol>>,
    /// The starting state of the DFA.
    starting_state: State,
    /// The set of accepting states.
    accepting_states: HashMap<State, AcceptingValue>,
}

impl<AcceptingValue: Clone, Symbol: Eq + Hash + Clone> DFA<AcceptingValue, Symbol> {
    /// Creates a new DFA.
    pub fn new(transition_fn: Vec<Transition<Symbol>>,
               starting_state: State,
               accepting_states: HashMap<State, AcceptingValue>)
               -> DFA<AcceptingValue, Symbol> {
        for transition in &transition_fn {
            assert!(!transition.is_epsilon());
        }
        DFA {
            transition_fn,
            starting_state,
            accepting_states,
        }
    }

    /// Calculates the transition function for the given state and symbol.
    pub fn transition(&self, state: State, symbol: Symbol) -> Option<State> {
        self.transition_fn
            .iter()
            .find(|transition| transition.from() == state && transition.matches(&symbol))
            .map(|transition| transition.to())
    }

    /// Returns the the given state maps to if it is accepting.
    ///
    /// If the state is not accepting, ´None´ is returned.
    pub fn get_accepting_value(&self, state: State) -> Option<AcceptingValue> {
        self.accepting_states
            .get(&state)
            .map(|value| value.clone())
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

    /// Returns the transitions from the given state.
    pub fn transitions_from(&self, state: State) -> Vec<&Transition<Symbol>> {
        self.transition_fn
            .iter()
            .filter(|transition| transition.from() == state)
            .collect()
    }
}

impl<AcceptingValue: Clone, Symbol: Eq + Hash + Clone + fmt::Debug> fmt::Debug
    for DFA<AcceptingValue, Symbol> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Starting state: {:?}", self.starting_state)?;

        writeln!(f, "Transitions:")?;
        for transition in &self.transition_fn {
            writeln!(f, "    {:?}", transition)?;
        }

        writeln!(f, "Accepting states:")?;
        write!(f, "{{")?;
        for (count, state) in self.accepting_states.keys().enumerate() {
            if count != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{:?}", state)?;
        }
        write!(f, "}}")
    }
}
