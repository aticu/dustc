//! This module is supposed to define the regular expressions that are used for lexical analysis.

use automata::State;
use automata::nfa::NFA;
use char_iter;
use std::collections::HashMap;
use std::ops::{BitOr, BitAnd};
use super::LexerAction;

macro_rules! reg_exp {
    ([a-z]) => {{
        use lexer::regexp::RegularExpression::Range;
        Box::new(Range('a', 'z'))
    }};
    ([A-Z]) => {{
        use lexer::regexp::RegularExpression::Range;
        Box::new(Range('A', 'Z'))
    }};
    ([0-9]) => {{
        use lexer::regexp::RegularExpression::Range;
        Box::new(Range('0', '9'))
    }};
    ([$x: expr]) => {{
        use lexer::regexp::RegularExpression::{BigUnion, Single, Epsilon};
        match $x.len() {
            0 => Box::new(Epsilon),
            1 => Box::new(Single($x.chars().next().unwrap())),
            _ => Box::new(BigUnion($x))
        }
    }};
    ($x: expr) => {{
        use lexer::regexp::RegularExpression::{Concatenation, Single, Epsilon};
        let mut iterator = $x.chars();
        if let Some(character) = iterator.next() {
            let mut result = Box::new(Single(character));

            for character in iterator {
                result = Box::new(Concatenation(result, Box::new(Single(character))))
            }

            result
        } else {
            Box::new(Epsilon)
        }
    }};
}

macro_rules! zero_or_more {
    ($x: expr) => {{
        use lexer::regexp::RegularExpression::ZeroOrMore;
        Box::new(ZeroOrMore($x))
    }};
}

/// This is the type used within regular expressions to allow recursion.
type IndirectRegularExpression = Box<RegularExpression>;

#[derive(Debug)]
/// Represents a regular expression.
pub enum RegularExpression {
    /// Represents an empty string.
    Epsilon,
    /// Represents the string with only the one given symbol.
    Single(char),
    /// Represents the union of the first and the second set of producable strings.
    Union(IndirectRegularExpression, IndirectRegularExpression),
    /// Represents many unions of many single symbols within the range.
    Range(char, char),
    /// Represents the union of all symbols contained within the string.
    BigUnion(&'static str),
    /// Represents the concatenation of both regular expressions.
    Concatenation(IndirectRegularExpression, IndirectRegularExpression),
    /// Represents zero or more occurences of the given regular expression.
    ZeroOrMore(IndirectRegularExpression),
    /// Represents one or more occurences of the given regular expression.
    OneOrMore(IndirectRegularExpression),
    /// Represents zero or one occurences of the given regular expression.
    ZeroOrOne(IndirectRegularExpression)
}

impl BitOr for Box<RegularExpression> {
    type Output = Box<RegularExpression>;

    fn bitor(self, rhs: Self) -> Box<RegularExpression> {
        Box::new(RegularExpression::Union(self, rhs))
    }
}

impl BitAnd for Box<RegularExpression> {
    type Output = Box<RegularExpression>;

    fn bitand(self, rhs: Self) -> Box<RegularExpression> {
        Box::new(RegularExpression::Concatenation(self, rhs))
    }
}

impl RegularExpression {
    /// Creates a new regular expression that has zero or more occurances of ´exp´.
    pub fn zero_or_more(exp: Box<RegularExpression>) -> Box<RegularExpression> {
        Box::new(RegularExpression::ZeroOrMore(exp))
    }

    /// Creates a new regular expression that has one or more occurances of ´exp´.
    pub fn one_or_more(exp: Box<RegularExpression>) -> Box<RegularExpression> {
        Box::new(RegularExpression::OneOrMore(exp))
    }

    /// Creates a new regular expression that has zero or one occurances of ´exp´.
    pub fn zero_or_one(exp: Box<RegularExpression>) -> Box<RegularExpression> {
        Box::new(RegularExpression::ZeroOrOne(exp))
    }

    /// Converts this regular expression to an NFA.
    pub fn to_nfa(&self, action: LexerAction, priority: u32) -> NFA<LexerAction> {
        self.to_fragment().to_nfa(action, priority)
    }

    /// Converts this regular expression to an NFA fragment.
    fn to_fragment(&self) -> NFAFragment {
        match *self {
            RegularExpression::Epsilon => {
                let start_state = State::new();

                NFAFragment {
                    start_state,
                    exit_transition: (start_state, None),
                    transitions: Vec::new()
                }
            }
            RegularExpression::Single(character) => {
                let start_state = State::new();

                NFAFragment {
                    start_state,
                    exit_transition: (start_state, Some(character)),
                    transitions: Vec::new()
                }
            }
            RegularExpression::Union(ref first, ref second) => {
                let first_fragment = first.to_fragment();
                let second_fragment = second.to_fragment();
                let (first_end, first_char) = first_fragment.exit_transition;
                let (second_end, second_char) = second_fragment.exit_transition;
                let start_state = State::new();
                let end_state = State::new();

                let mut transitions = vec![(start_state, None, first_fragment.start_state), 
                                           (start_state, None, second_fragment.start_state),
                                           (first_end, first_char, end_state),
                                           (second_end, second_char, end_state)];

                transitions.extend(first_fragment.transitions);
                transitions.extend(second_fragment.transitions);

                NFAFragment {
                    start_state,
                    exit_transition: (end_state, None),
                    transitions
                }
            }
            RegularExpression::BigUnion(ref string) => {
                let start_state = State::new();
                let end_state = State::new();

                let mut transitions = Vec::new();

                for character in string.chars() {
                    transitions.push((start_state, Some(character), end_state));
                }

                NFAFragment {
                    start_state,
                    exit_transition: (end_state, None),
                    transitions
                }
            }
            RegularExpression::Range(start_char, end_char) => {
                assert!(start_char < end_char);
                
                let start_state = State::new();
                let end_state = State::new();

                let mut transitions = Vec::new();

                for character in char_iter::new(start_char, end_char) {
                    transitions.push((start_state, Some(character), end_state));
                }

                NFAFragment {
                    start_state,
                    exit_transition: (end_state, None),
                    transitions
                }
            }
            RegularExpression::Concatenation(ref first, ref second) => {
                let first_fragment = first.to_fragment();
                let second_fragment = second.to_fragment();
                let (first_end, first_char) = first_fragment.exit_transition;

                let mut transitions = vec![(first_end, first_char, second_fragment.start_state)];

                transitions.extend(first_fragment.transitions);
                transitions.extend(second_fragment.transitions);

                NFAFragment {
                    start_state: first_fragment.start_state,
                    exit_transition: second_fragment.exit_transition,
                    transitions
                }
            }
            RegularExpression::ZeroOrMore(ref exp) => {
                let fragment = exp.to_fragment();
                let (fragment_end, fragment_char) = fragment.exit_transition;
                let start_state = State::new();

                let mut transitions = vec![(start_state, None, fragment.start_state),
                                           (fragment_end, fragment_char, start_state)];

                transitions.extend(fragment.transitions);

                NFAFragment {
                    start_state,
                    exit_transition: (start_state, None),
                    transitions
                }
            }
            RegularExpression::OneOrMore(ref exp) => {
                let fragment = exp.to_fragment();
                let (fragment_end, fragment_char) = fragment.exit_transition;
                let start_state = State::new();
                let end_state = State::new();

                let mut transitions = vec![(start_state, None, fragment.start_state),
                                           (end_state, None, start_state),
                                           (fragment_end, fragment_char, end_state)];

                transitions.extend(fragment.transitions);

                NFAFragment {
                    start_state,
                    exit_transition: (end_state, None),
                    transitions
                }
            }
            RegularExpression::ZeroOrOne(ref exp) => {
                let fragment = exp.to_fragment();
                let (fragment_end, fragment_char) = fragment.exit_transition;
                let start_state = State::new();
                let end_state = State::new();

                let mut transitions = vec![(start_state, None, fragment.start_state),
                                           (start_state, None, end_state),
                                           (fragment_end, fragment_char, end_state)];

                transitions.extend(fragment.transitions);

                NFAFragment {
                    start_state,
                    exit_transition: (end_state, None),
                    transitions
                }
            }
        }
    }
}

/// An internally used NFA fragment that is later combined to a full NFA.
struct NFAFragment {
    /// The start state of the fragment.
    start_state: State,
    /// The half-transition that is exiting this fragment.
    exit_transition: (State, Option<char>),
    /// The transitions of this fragment.
    transitions: Vec<(State, Option<char>, State)>
}

impl NFAFragment {
    /// Converts the NFA fragment to an NFA.
    fn to_nfa(self, action: LexerAction, priority: u32) -> NFA<LexerAction> {
        let start_state = self.start_state;
        let mut transitions = self.transitions;
        let (last_state, last_character) = self.exit_transition;

        let mut accepting_states = HashMap::new();

        if last_character.is_some() {
            let end_state = State::new();
            transitions.push((last_state, last_character, end_state));

            accepting_states.insert(end_state, (priority, action));
        } else {
            accepting_states.insert(last_state, (priority, action));
        }

        NFA::new(transitions, start_state, accepting_states)
    }
}
