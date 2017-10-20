//! This module is supposed to define context-free grammers and their components.

#[macro_use]
pub mod symbol;

use automata::{State, Transition};
use automata::dfa::DFA;
use automata::nfa::NFA;
use std::collections::HashMap;
use self::symbol::Symbol;
use std::fmt::Debug;
use std::hash::Hash;

/// Represents a production in the grammer.
#[derive(Debug, Clone)]
pub struct Production<Nonterminal, Terminal>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash
{
    /// The non-terminal that uses this production.
    from: Symbol<Nonterminal, Terminal>,
    /// The symbols that the non-terminal gets converted to.
    to: Vec<Symbol<Nonterminal, Terminal>>,
}

/// This macro creates a production.
macro_rules! production {
    ($nonterminal: expr => $($symbol: expr),*) => {{
        use $crate::parser::grammer::Production;
        use $crate::parser::grammer::symbol::IntoSymbol;
        Production::new(
            $nonterminal.into_symbol(),
            vec![$($symbol.clone().into_symbol()),*]
        )
    }}
}

impl<Nonterminal, Terminal> Production<Nonterminal, Terminal>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash
{
    /// Creates a new production.
    pub fn new(from: Symbol<Nonterminal, Terminal>,
               to: Vec<Symbol<Nonterminal, Terminal>>)
               -> Production<Nonterminal, Terminal> {
        assert!(from.is_nonterminal());
        Production { from, to }
    }

    /// Creates an NFA from this production.
    fn to_nfa_fragment(&self) -> NFAFragment<Nonterminal, Terminal> {
        let starting_state = State::new();

        let mut last_state = starting_state;
        let mut transitions = Vec::new();

        for symbol in &self.to {
            let new_state = State::new();

            transitions.push(Transition::new(last_state, Some(symbol.clone()), new_state));

            last_state = new_state;
        }

        NFAFragment {
            starting_state,
            transitions,
            accepting_state: last_state,
        }
    }
}

/// Represents an NFA fragment that is created from a production.
struct NFAFragment<Nonterminal, Terminal>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash
{
    /// The starting state of the NFA fragment.
    starting_state: State,
    /// The transitions of the NFA fragment.
    transitions: Vec<Transition<Symbol<Nonterminal, Terminal>>>,
    /// The accepting state of the NFA fragment.
    accepting_state: State,
}

/// Represents a context-free grammer.
#[derive(Debug)]
pub struct Grammer<Nonterminal, Terminal>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash
{
    /// The start symbol for the grammer.
    start_symbol: Symbol<Nonterminal, Terminal>,
    /// The productions for the grammer.
    productions: Vec<Production<Nonterminal, Terminal>>,
}

impl<Nonterminal, Terminal> Grammer<Nonterminal, Terminal>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash
{
    /// Creates a new grammer with the given start symbol and productions.
    pub fn new(start_symbol: Nonterminal,
               productions: Vec<Production<Nonterminal, Terminal>>)
               -> Grammer<Nonterminal, Terminal> {
        // Make sure no internal symbols are used.
        for production in &productions {
            for symbol in &production.to {
                assert!(symbol.is_matcher());
            }
        }

        Grammer {
            start_symbol: Symbol::Nonterminal(start_symbol),
            productions,
        }
    }

    /// Creates a DFA from this grammer.
    pub fn to_dfa(&self) -> DFA<Production<Nonterminal, Terminal>, Symbol<Nonterminal, Terminal>> {
        // Add a new starting production.
        let start_symbol: Symbol<Nonterminal, Terminal> = Symbol::InternalNonterminal(0);
        let start_production =
            Production::<Nonterminal, Terminal>::new(start_symbol, vec![self.start_symbol.clone()]);

        // Calculate NFA fragments from the productions.
        let mut nfa_fragments = vec![(start_production.clone(),
                                      start_production.to_nfa_fragment())];
        for production in &self.productions {
            nfa_fragments.push((production.clone(), production.to_nfa_fragment()));
        }

        // Combine the transitions.
        let mut transitions = Vec::new();
        for &(_, ref fragment) in &nfa_fragments {
            transitions.extend(fragment.transitions.clone());

            for transition in &fragment.transitions {
                // This is safe, because we know how the transitions are constructed.
                let transition_symbol = &transition.relevant_symbols()[0];

                // If the transition is on a nonterminal, add transitions to that
                // nonterminals productions from the starting state.
                if transition_symbol.is_nonterminal() {
                    let starting_state = transition.from();

                    for &(ref production, ref other_fragment) in &nfa_fragments {
                        if &production.from == transition_symbol {
                            transitions.push(Transition::new(starting_state,
                                                             None,
                                                             other_fragment.starting_state));
                        }
                    }
                }
            }
        }

        // The starting state is the state of the added production.
        let starting_state = nfa_fragments[0].1.starting_state;

        // Build the accepting states containing the productions they used.
        let mut accepting_states = HashMap::new();
        for (production, fragment) in nfa_fragments {
            accepting_states.insert(fragment.accepting_state, (0, production));
        }

        NFA::new(transitions, starting_state, accepting_states).to_dfa()

    }
}
