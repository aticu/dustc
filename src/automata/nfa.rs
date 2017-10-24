//! This module is supposed to define non-deterministic finite automata (NFAs).

use super::{State, Transition};
use super::dfa::DFA;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

/// Represents a non-deterministic finite automaton.
#[derive(Debug)]
pub struct NFA<AcceptingValue: Clone, Symbol: Eq + Hash + Clone> {
    /// Represents the transition relation.
    ///
    /// A map from the current state and the next character to the next state.
    /// It is however not unique, so it is just represented as a list of
    /// triples.
    /// A transition on a ´None´ is an epsilon transition.
    transitions: Vec<Transition<Symbol>>,
    /// The starting state of the NFA.
    starting_state: State,
    /// The set of accepting states.
    ///
    /// Each state has an associated type to it with a priority. Lower numbers
    /// mean higher priority.
    accepting_states: HashMap<State, (u32, AcceptingValue)>
}

impl<AcceptingValue: Clone, Symbol: Eq + Hash + Clone> NFA<AcceptingValue, Symbol> {
    /// Creates a new NFA.
    pub fn new(transitions: Vec<Transition<Symbol>>,
               starting_state: State,
               accepting_states: HashMap<State, (u32, AcceptingValue)>)
               -> NFA<AcceptingValue, Symbol> {
        NFA {
            transitions,
            starting_state,
            accepting_states
        }
    }

    /// Calculates the epsilon closure of the given set of states.
    fn epsilon_closure(&self, states: &HashSet<State>) -> HashSet<State> {
        let mut unmarked_states: Vec<State> = Vec::new();

        unmarked_states.extend(states);

        let mut result = HashSet::new();

        while unmarked_states.len() > 0 {
            let next_state = unmarked_states.pop().unwrap();
            result.insert(next_state);

            let transition_result = self.transition(next_state, &None);

            for state in transition_result {
                if !states.contains(&state) {
                    if !result.contains(&state) {
                        if !unmarked_states.contains(&state) {
                            unmarked_states.push(state);
                        }
                    }
                }
            }
        }

        result
    }

    /// Calculates the transition relation from one state.
    ///
    /// The returned set is the set of reachable states using the given symbol.
    fn transition(&self, state: State, symbol: &Option<Symbol>) -> HashSet<State> {
        let mut result = HashSet::new();

        if let &Some(ref symbol) = symbol {
            for ref transition in self.transitions
                    .iter()
                    .filter(|transition| {
                                transition.from() == state && transition.matches(&symbol)
                            }) {
                result.insert(transition.to());
            }
        } else {
            for ref transition in self.transitions
                    .iter()
                    .filter(|transition| {
                                transition.from() == state && transition.is_epsilon()
                            }) {
                result.insert(transition.to());
            }
        }

        result
    }

    /// Calculates the transition relation from a set of states.
    ///
    /// The returned set is the set of reachable states using the given symbol.
    fn transition_of_set(&self,
                         states: &HashSet<State>,
                         symbol: &Option<Symbol>)
                         -> HashSet<State> {
        let mut result = HashSet::new();

        for state in states {
            result.extend(&self.epsilon_closure(&self.transition(*state, symbol)));
        }

        result
    }

    /// Calculates the transition relation from one state on symbols implicitly
    /// mentioned.
    ///
    /// The returned set is the set of reachable states using implicitly
    /// mentioned symbols.
    fn transition_rest(&self, state: State) -> HashSet<State> {
        let mut result = HashSet::new();

        for ref transition in
            self.transitions
                .iter()
                .filter(|transition| transition.from() == state && transition.matches_rest()) {
            result.insert(transition.to());
        }

        result
    }

    /// Calculates the transition relation from a set of states on symbols
    /// implicitly mentioned.
    ///
    /// The returned set is the set of reachable states using implicitly
    /// mentioned symbols.
    fn transition_of_set_rest(&self, states: &HashSet<State>) -> HashSet<State> {
        let mut result = HashSet::new();

        for state in states {
            result.extend(&self.epsilon_closure(&self.transition_rest(*state)));
        }

        result
    }

    /// Converts this NFA to a DFA.
    pub fn to_dfa_with_map(self) -> (DFA<AcceptingValue, Symbol>, Vec<(HashSet<State>, State)>) {
        let mut starting_state_set = HashSet::new();
        starting_state_set.insert(self.starting_state);

        let starting_state_set = self.epsilon_closure(&starting_state_set);

        let mut unmarked_states = Vec::new();
        unmarked_states.push(starting_state_set.clone());

        // Calculates the set of characters used in transitions.
        let mut relevant_symbols = HashSet::new();

        for ref transition in (&self.transitions).iter() {
            relevant_symbols.extend(transition.relevant_symbols());
        }

        // The transition map holds the transitions from the temporary state sets
        // to the temporary state sets.
        // Once all transitions and new states have been calculated they will be
        // transformed to new simpler states.
        let mut transition_map = Vec::new();
        let mut rest_transitions = Vec::new();
        let mut marked_states = Vec::new();

        while unmarked_states.len() > 0 {
            let current_state = unmarked_states.pop().unwrap();
            marked_states.push(current_state.clone());

            for symbol in &relevant_symbols {
                let target_state = self.transition_of_set(&current_state, &Some(symbol.clone()));
                if target_state.len() > 0 {
                    transition_map.push((current_state.clone(),
                                         Some(symbol),
                                         target_state.clone()));

                    if !marked_states.contains(&target_state) {
                        if !unmarked_states.contains(&target_state) {
                            unmarked_states.push(target_state);
                        }
                    }
                }
            }

            // Also try transitioning on the "rest", meaning all implicit symbols (not in
            // the
            // relevant_symbols list).
            let target_state = self.transition_of_set_rest(&current_state);
            if target_state.len() > 0 {
                rest_transitions.push((current_state.clone(), target_state.clone()));

                if !marked_states.contains(&target_state) {
                    if !unmarked_states.contains(&target_state) {
                        unmarked_states.push(target_state);
                    }
                }
            }
        }

        // ´marked_state´ now contains all states of the new DFA.
        // The ´transition_map´ contains all transitions.
        // The states are now transformed to the ´State´s.
        let mut state_map = Vec::new();

        for state in marked_states {
            state_map.push((state, State::new()));
        }

        let mut final_transition_map = Vec::new();

        for (old_state_set, symbol, new_state_set) in transition_map {
            let mut old_state = None;
            let mut new_state = None;

            for &(ref state_set, state) in &state_map {
                if &old_state_set == state_set {
                    old_state = Some(state);
                    if new_state.is_some() {
                        break;
                    }
                }
                if &new_state_set == state_set {
                    new_state = Some(state);
                    if old_state.is_some() {
                        break;
                    }
                }
            }

            let old_state = old_state.expect("The state conversion in the DFA creation failed.");
            let new_state = new_state.expect("The state conversion in the DFA creation failed.");

            final_transition_map.push(Transition::<Symbol>::new(old_state,
                                                                symbol.cloned(),
                                                                new_state));
        }

        for (old_state_set, new_state_set) in rest_transitions {
            let mut old_state = None;
            let mut new_state = None;

            for &(ref state_set, state) in &state_map {
                if &old_state_set == state_set {
                    old_state = Some(state);
                    if new_state.is_some() {
                        break;
                    }
                }
                if &new_state_set == state_set {
                    new_state = Some(state);
                    if old_state.is_some() {
                        break;
                    }
                }
            }

            let old_state = old_state.expect("The state conversion in the DFA creation failed.");
            let new_state = new_state.expect("The state conversion in the DFA creation failed.");

            final_transition_map
                .push(Transition::<Symbol>::new_indirect(old_state,
                                                         relevant_symbols
                                                             .iter()
                                                             .map(|symbol| symbol.clone())
                                                             .collect(),
                                                         new_state));
        }

        // The transition map is now calculated.
        // Now the starting state is transformed.
        let mut starting_state = None;

        for &(ref state_set, state) in &state_map {
            if &starting_state_set == state_set {
                starting_state = Some(state);
                break;
            }
        }

        let starting_state =
            starting_state.expect("The state conversion in the DFA creation failed.");

        // Last the accepting states are calculated.
        let mut accepting_states = HashMap::new();

        for &(ref state_set, state) in &state_map {
            let mut current_priority: u32 = <u32>::max_value();
            let mut action = None;

            for sub_state in state_set {
                // If the state is accepting
                if let Some(&(new_priority, ref new_action)) =
                    self.accepting_states.get(sub_state) {
                    if action.is_none() {
                        current_priority = new_priority;
                        action = Some(new_action.clone());
                    } else if new_priority < current_priority {
                        current_priority = new_priority;
                        action = Some(new_action.clone());
                    }
                }
            }

            if action.is_some() {
                accepting_states.insert(state, action.unwrap().clone());
            }
        }

        (DFA::new(final_transition_map, starting_state, accepting_states), state_map)
    }

    pub fn to_dfa(self) -> DFA<AcceptingValue, Symbol> {
        self.to_dfa_with_map().0
    }

    /// Combines several NFAs into one.
    pub fn combine(nfas: Vec<NFA<AcceptingValue, Symbol>>) -> NFA<AcceptingValue, Symbol> {
        let start_state = State::new();

        let mut transitions = Vec::new();

        let mut accepting_states = HashMap::new();

        for nfa in nfas {
            transitions.push(Transition::new(start_state, None, nfa.starting_state));

            transitions.extend(nfa.transitions);

            accepting_states.extend(nfa.accepting_states);
        }

        NFA::new(transitions, start_state, accepting_states)
    }
}
