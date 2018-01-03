//! This module is supposed to define context-free grammars and their
//! components.

#[macro_use]
pub mod symbol;

use self::symbol::Symbol;
use super::{Associativity, Operator, Parser, ParserAction};
use automata::{State, Transition};
use automata::dfa::DFA;
use automata::nfa::NFA;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;

/// Represents a production in the grammar.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Production<Nonterminal, Terminal, AST>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash,
          AST: Clone + Debug + Eq + Default + From<Terminal>
{
    /// The non-terminal that uses this production.
    from: Symbol<Nonterminal, Terminal>,
    /// The symbols that the non-terminal gets converted to.
    to: Vec<Symbol<Nonterminal, Terminal>>,
    reduce_action: fn(Vec<AST>) -> AST
}

/// This macro creates a production.
macro_rules! production {
    ([$nonterminal: expr => $($symbol: expr),*], $action: expr) => {{
        use $crate::parser::grammar::Production;
        use $crate::parser::grammar::symbol::IntoSymbol;
        Production::new(
            $nonterminal.into_symbol(),
            vec![$($symbol.clone().into_symbol()),*],
            $action
        )
    }}
}

impl<Nonterminal, Terminal, AST> Production<Nonterminal, Terminal, AST>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash,
          AST: Clone + Debug + Eq + Default + From<Terminal>
{
    /// Creates a new production.
    pub fn new(from: Symbol<Nonterminal, Terminal>,
               to: Vec<Symbol<Nonterminal, Terminal>>,
               reduce_action: fn(Vec<AST>) -> AST)
               -> Production<Nonterminal, Terminal, AST> {
        assert!(from.is_nonterminal());
        Production {
            from,
            to,
            reduce_action
        }
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
            accepting_state: last_state
        }
    }

    /// Returns the last operator in the right hand side of the production.
    fn last_operator<'a>(&'a self,
                         operators: &'a Vec<Operator<Terminal>>)
                         -> Option<&'a Operator<Terminal>> {
        let mut last_operator = None;

        for symbol in &self.to {
            match symbol {
                &Symbol::Terminal(ref terminal) => {
                    for operator in operators {
                        if &operator.terminal == terminal {
                            last_operator = Some(operator);
                            break;
                        }
                    }
                },
                _ => (),
            }
        }

        last_operator
    }

    /// Reduces using this production, returning the result of the production.
    pub fn reduce(&self, stack: &mut Vec<(State, AST)>) -> (Symbol<Nonterminal, Terminal>, AST) {
        let nonterminal = self.from.clone();

        let mut parameters = Vec::new();

        for _ in 0..self.to.len() {
            if let Some((_, next_paramter)) = stack.pop() {
                parameters.insert(0, next_paramter);
            } else {
                panic!("Unexpected end of parse stack.");
            }
        }

        (nonterminal, (self.reduce_action)(parameters))
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
    accepting_state: State
}

/// Represents a context-free grammar.
#[derive(Debug)]
pub struct Grammar<Nonterminal, Terminal, AST>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash,
          AST: Clone + Debug + Eq + Default + From<Terminal>
{
    /// The start symbol for the grammar.
    start_symbol: Symbol<Nonterminal, Terminal>,
    /// The productions for the grammar.
    productions: Vec<Production<Nonterminal, Terminal, AST>>
}

impl<Nonterminal, Terminal, AST> Grammar<Nonterminal, Terminal, AST>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash,
          AST: Clone + Debug + Eq + Default + From<Terminal>
{
    /// Creates a new grammar with the given start symbol and productions.
    pub fn new(start_symbol: Nonterminal,
               productions: Vec<Production<Nonterminal, Terminal, AST>>)
               -> Grammar<Nonterminal, Terminal, AST> {
        // Make sure no internal symbols are used.
        for production in &productions {
            for symbol in &production.to {
                assert!(symbol.is_matcher());
            }
        }

        Grammar {
            start_symbol: Symbol::Nonterminal(start_symbol),
            productions
        }
    }

    /// Returns the set of nullable nonterminal symbols.
    fn nullable(&self) -> HashSet<&Symbol<Nonterminal, Terminal>> {
        let mut result = HashSet::new();

        // Map right hand sides and nonterminals to their "nullable-ness".
        let mut right_hand_sides = HashMap::new();
        let mut nonterminals = HashMap::new();

        // Start with everything being non-nullable.
        for production in &self.productions {
            right_hand_sides.insert(&production.to, false);
            nonterminals.insert(&production.from, false);

            for symbol in &production.to {
                if symbol.is_nonterminal() {
                    nonterminals.insert(&symbol, false);
                }
            }
        }

        // Iterate until no changes occur.
        let mut changed = true;
        while changed {
            changed = false;

            for (right_hand_side, value) in right_hand_sides.iter_mut() {
                if !*value {
                    let mut nullable = true;
                    for symbol in *right_hand_side {
                        if symbol.is_nonterminal() {
                            if !nonterminals.get(symbol).unwrap() {
                                nullable = false;
                                break;
                            }
                        } else {
                            nullable = false;
                            break;
                        }
                    }
                    if nullable {
                        *value = true;
                        changed = true;
                    }
                }
            }

            for (nonterminal, value) in nonterminals.iter_mut() {
                if !*value {
                    for production in &self.productions {
                        if &production.from == *nonterminal {
                            if *right_hand_sides.get(&production.to).unwrap() {
                                *value = true;
                                changed = true;
                                break;
                            }
                        }
                    }
                }
            }
        }

        // Collect all nullable nonterminals.
        for (nonterminal, value) in nonterminals.iter() {
            if *value {
                result.insert(*nonterminal);
            }
        }

        result
    }

    /// Calculates the first set for each nonterminal.
    ///
    /// The first set contains all terminals that could be the first character
    /// of a derivation with the given nonterminal.
    fn first(&self)
             -> HashMap<&Symbol<Nonterminal, Terminal>, Vec<&Symbol<Nonterminal, Terminal>>> {
        let nullable = self.nullable();

        // Map right hand sides and nonterminals to their first sets.
        let mut right_hand_sides = HashMap::new();
        let mut nonterminals = HashMap::new();

        // Start with empty sets.
        for production in &self.productions {
            right_hand_sides.insert(&production.to, Vec::new());
            nonterminals.insert(&production.from, Vec::new());

            for symbol in &production.to {
                if symbol.is_nonterminal() {
                    nonterminals.insert(&symbol, Vec::new());
                }
            }
        }

        // Iterate until no changes occur.
        let mut changed = true;
        while changed {
            changed = false;

            for (right_hand_side, value) in right_hand_sides.iter_mut() {
                for symbol in right_hand_side.iter() {
                    // Iterate until we hit a non-nullable nonterminal or a terminal.
                    if symbol.is_nonterminal() {
                        for other_symbol in nonterminals.get(symbol).unwrap() {
                            if !value.contains(other_symbol) {
                                changed = true;
                                value.push(*other_symbol);
                            }
                        }
                        if !nullable.contains(&symbol) {
                            break;
                        }
                    } else {
                        if !value.contains(&symbol) {
                            changed = true;
                            value.push(symbol);
                        }
                        break;
                    }
                }
            }

            for (nonterminal, value) in nonterminals.iter_mut() {
                for production in &self.productions {
                    if &production.from == *nonterminal {
                        let symbols = right_hand_sides.get(&production.to).unwrap();
                        for symbol in symbols {
                            if !value.contains(symbol) {
                                changed = true;
                                value.push(symbol);
                            }
                        }
                    }
                }
            }
        }

        nonterminals
    }

    /// Calculates the follow set for each nonterminal.
    ///
    /// The follow set contains all terminals that could follow an occurance of
    /// the nonterminal.
    pub fn follow
        (&self)
         -> HashMap<&Symbol<Nonterminal, Terminal>, Vec<Symbol<Nonterminal, Terminal>>> {
        let nullable = self.nullable();
        let first = self.first();

        // Map nonterminals to their follow sets.
        let mut nonterminals = HashMap::new();
        let mut constraints = HashSet::new();

        // Start with empty follow sets.
        for production in &self.productions {
            nonterminals.insert(&production.from, Vec::new());

            for symbol in &production.to {
                if symbol.is_nonterminal() {
                    nonterminals.insert(&symbol, Vec::new());
                }
            }
        }

        // The start symbol may be followed by the end of input.
        nonterminals
            .get_mut(&self.start_symbol)
            .unwrap()
            .push(Symbol::EndOfInput);

        for (nonterminal, value) in nonterminals.iter_mut() {
            for production in &self.productions {
                let last_symbol = production.to.len();
                for i in 0..last_symbol {
                    if &production.to[i] == *nonterminal {
                        let symbols = &production.to[i + 1..last_symbol];
                        let first_symbols =
                            Grammar::<Nonterminal, Terminal, AST>::first_sequence(&nullable,
                                                                                  &first,
                                                                                  symbols);
                        for symbol in first_symbols {
                            if !value.contains(symbol) {
                                value.push(symbol.clone());
                            }
                        }
                        if Grammar::<Nonterminal, Terminal, AST>::nullable_sequence(&nullable,
                                                                                    symbols) {
                            if &production.from != *nonterminal {
                                constraints.insert((&production.from, nonterminal.clone()));
                            }
                        }
                    }
                }
            }
        }

        let mut changed = true;
        while changed {
            changed = false;

            for &(subset, superset) in &constraints {
                nonterminals.get(subset).cloned();

                for symbol in nonterminals.get(subset).unwrap().clone() {
                    if !nonterminals.get(superset).unwrap().contains(&symbol) {
                        changed = true;
                        nonterminals
                            .get_mut(superset)
                            .unwrap()
                            .push(symbol.clone());
                    }
                }
            }
        }

        nonterminals
    }

    /// Returns true if the given sequence is nullable.
    fn nullable_sequence(nullable: &HashSet<&Symbol<Nonterminal, Terminal>>,
                         sequence: &[Symbol<Nonterminal, Terminal>])
                         -> bool {
        for symbol in sequence {
            if symbol.is_nonterminal() {
                if !nullable.contains(symbol) {
                    return false;
                }
            } else {
                return false;
            }
        }

        true
    }

    /// Calculates the first set for a given sequence.
    fn first_sequence<'a>(nullable: &'a HashSet<&Symbol<Nonterminal, Terminal>>,
                          first: &'a HashMap<&Symbol<Nonterminal, Terminal>,
                                             Vec<&Symbol<Nonterminal, Terminal>>>,
                          sequence: &'a [Symbol<Nonterminal, Terminal>])
                          -> HashSet<&'a Symbol<Nonterminal, Terminal>> {
        let mut result = HashSet::new();

        for symbol in sequence {
            if symbol.is_nonterminal() {
                result.extend(first.get(symbol).unwrap());
                if !nullable.contains(symbol) {
                    break;
                }
            } else {
                result.insert(symbol);
                break;
            }
        }

        result
    }

    /// Returns the corresponding operator to the terminal symbol, if it exists.
    fn find_operator<'a>(symbol: &'a Symbol<Nonterminal, Terminal>,
                         operators: &'a Vec<Operator<Terminal>>)
                         -> Option<&'a Operator<Terminal>> {
        match symbol {
            &Symbol::Terminal(ref terminal) => {
                for operator in operators {
                    if &operator.terminal == terminal {
                        return Some(operator);
                    }
                }
                None
            },
            _ => None,
        }
    }

    /// Creates a DFA from this grammar.
    fn to_dfa(&self)
              -> ((DFA<Production<Nonterminal, Terminal, AST>, Symbol<Nonterminal, Terminal>>,
                   Vec<(HashSet<State>, State)>),
                  HashMap<State, (u32, Production<Nonterminal, Terminal, AST>)>) {
        // Add a new starting production.
        let start_symbol: Symbol<Nonterminal, Terminal> = Symbol::InternalNonterminal(0);
        let start_production =
            Production::<Nonterminal, Terminal, AST>::new(start_symbol,
                                                          vec![self.start_symbol.clone()],
                                                          |_| AST::default());

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

        (NFA::new(transitions, starting_state, accepting_states.clone()).to_dfa_with_map(),
         accepting_states)
    }

    /// Creates a parser from this grammar.
    pub fn to_parser(&self,
                     operators: Vec<Operator<Terminal>>)
                     -> Parser<Nonterminal, Terminal, AST> {
        let follow = self.follow();
        let ((dfa, nfa_state_map), accepting_states) = self.to_dfa();

        let mut table: HashMap<(State, Symbol<Nonterminal, Terminal>),
                               ParserAction<Nonterminal, Terminal, AST>> = HashMap::new();

        for &(_, state) in &nfa_state_map {
            for transition in dfa.transitions_from(state) {
                let symbol = transition.relevant_symbols()[0].clone();

                if symbol.is_nonterminal() {
                    table.insert((state, symbol.clone()),
                                 ParserAction::Go(dfa.transition(state, symbol).unwrap()));
                } else {
                    table.insert((state, symbol.clone()),
                                 ParserAction::Shift(dfa.transition(state, symbol).unwrap()));
                }
            }
        }

        macro_rules! report_conflict {
            ($new_action: expr, $location: expr, $old_action: expr) => {
                panic!(concat!("Parser generation error: ",
                               "Trying to add {:?} to the SLR table at {:?},",
                               "but {:?} was already there."),
                               $new_action,
                               $location,
                               $old_action);
            }
        }

        for &(ref nfa_states, state) in &nfa_state_map {
            for nfa_state in nfa_states {
                if let Some(production) = accepting_states.get(&nfa_state) {
                    if production.1.from == Symbol::InternalNonterminal(0) {
                        if let Some(conflicting_entry) =
                            table.insert((state, Symbol::EndOfInput), ParserAction::Accept) {
                            let accept: ParserAction<Nonterminal,
                                                     Terminal,
                                                     AST> = ParserAction::Accept;
                            let end_of_input: Symbol<Nonterminal,
                                                     Terminal> = Symbol::EndOfInput;

                            report_conflict!(accept, (state, end_of_input), conflicting_entry);
                        }
                    } else {
                        for symbol in follow.get(&production.1.from).unwrap() {
                            // If the table already contains the key.
                            if table.contains_key(&(state, symbol.clone())) {
                                // Try to resolve the conflict using precendence.
                                let new_operator = production.1.last_operator(&operators);
                                let old_operator =
                                    Grammar::<Nonterminal,
                                              Terminal,
                                              AST>::find_operator(symbol, &operators);

                                // Only resolve shift-reduce conflicts.
                                if let Some(&ParserAction::Shift(_)) =
                                    table.get(&(state, symbol.clone())) {
                                    // Only resolve conflicts if both symbols involved are
                                    // operators.
                                    if let (Some(new_operator), Some(old_operator)) =
                                        (new_operator, old_operator) {
                                        // Pick the operator with the higher precedence.
                                        if new_operator.precedence < old_operator.precedence {
                                            table.insert((state, symbol.clone()),
                                                         ParserAction::Reduce(production
                                                                                  .1
                                                                                  .clone()));
                                        } else if old_operator.precedence ==
                                                  new_operator.precedence {
                                            // For equal precendence decide based on associativity.
                                            assert_eq!(old_operator.associativity,
                                                       new_operator.associativity);
                                            match new_operator.associativity {
                                                Associativity::Left =>
                                                    table.insert((state, symbol.clone()),
                                                    ParserAction::Reduce(
                                                        production.1.clone())),
                                                Associativity::Right => None,
                                                Associativity::None => {
                                                    table.remove(&(state, symbol.clone()))
                                                },
                                            };
                                        }
                                    } else {
                                        let conflicting_entry = table.get(&(state, symbol.clone()));

                                        report_conflict!(ParserAction::Reduce(production
                                                                                  .1
                                                                                  .clone()),
                                                         (state, symbol.clone()),
                                                         conflicting_entry);
                                    }
                                } else {
                                    let conflicting_entry = table.get(&(state, symbol.clone()));

                                    report_conflict!(ParserAction::Reduce(production.1.clone()),
                                                     (state, symbol.clone()),
                                                     conflicting_entry);
                                }
                            } else {
                                // If there is no conflict, just add the action.
                                table.insert((state, symbol.clone()),
                                             ParserAction::Reduce(production.1.clone()));
                            }
                        }
                    }
                }
            }
        }

        Parser::new(table, dfa.start())
    }
}
