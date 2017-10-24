//! This module is supposed to define the regular expressions that are used for
//! lexical analysis.

use super::LexerAction;
use automata::{State, Transition};
use automata::nfa::NFA;
use char_iter;
use std::collections::HashMap;
use std::ops::{BitAnd, BitOr};

/// This macro creates a regular expression.
#[macro_export]
macro_rules! reg_exp {
    ([a-z]) => {{
        use $crate::lexer::regexp::RegularExpression::Range;
        Box::new(Range('a', 'z'))
    }};
    ([A-Z]) => {{
        use $crate::lexer::regexp::RegularExpression::Range;
        Box::new(Range('A', 'Z'))
    }};
    ([a-zA-Z]) => {{
        reg_exp!([a-z]) | reg_exp!([A-Z])
    }};
    ([0-9]) => {{
        use $crate::lexer::regexp::RegularExpression::Range;
        Box::new(Range('0', '9'))
    }};
    ([a-zA-Z0-9]) => {{
        reg_exp!([a-z]) | reg_exp!([A-Z]) | reg_exp!([0-9])
    }};
    ([$x: expr]) => {{
        use $crate::lexer::regexp::RegularExpression::{BigUnion, Single, Epsilon};
        match $x.len() {
            0 => Box::new(Epsilon),
            1 => Box::new(Single($x.chars().next().unwrap())),
            _ => Box::new(BigUnion($x))
        }
    }};
    (![$x: expr]) => {{
        use $crate::lexer::regexp::RegularExpression::EverythingBut;
        let vec: Vec<char> = $x.chars().collect();
        Box::new(EverythingBut(vec))
    }};
    ($x: expr) => {{
        use $crate::lexer::regexp::RegularExpression::{Concatenation, Single, Epsilon};
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

/// This macro creates a regular expression.
macro_rules! zero_or_more {
    ($x: expr) => {{
        use $crate::lexer::regexp::RegularExpression::ZeroOrMore;
        Box::new(ZeroOrMore($x))
    }};
}

/// This macro creates a regular expression.
macro_rules! one_or_more {
    ($x: expr) => {{
        use $crate::lexer::regexp::RegularExpression::OneOrMore;
        Box::new(OneOrMore($x))
    }};
}

/// This macro creates a regular expression.
macro_rules! zero_or_one {
    ($x: expr) => {{
        use $crate::lexer::regexp::RegularExpression::ZeroOrOne;
        Box::new(ZeroOrOne($x))
    }};
}

/// This is the type used within regular expressions to allow recursion.
type IndirectRegularExpression = Box<RegularExpression>;

/// Represents a regular expression.
#[derive(Debug, PartialEq, Eq)]
pub enum RegularExpression {
    /// Represents an empty string.
    Epsilon,
    /// Represents the string with only the one given symbol.
    Single(char),
    /// Represents the union of the first and the second set of producable
    /// strings.
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
    ZeroOrOne(IndirectRegularExpression),
    /// Represents every unicode symbol except the given ones.
    EverythingBut(Vec<char>)
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
    /// Creates a new regular expression that has zero or more occurances of
    /// ´exp´.
    pub fn zero_or_more(exp: Box<RegularExpression>) -> Box<RegularExpression> {
        Box::new(RegularExpression::ZeroOrMore(exp))
    }

    /// Creates a new regular expression that has one or more occurances of
    /// ´exp´.
    pub fn one_or_more(exp: Box<RegularExpression>) -> Box<RegularExpression> {
        Box::new(RegularExpression::OneOrMore(exp))
    }

    /// Creates a new regular expression that has zero or one occurances of
    /// ´exp´.
    pub fn zero_or_one(exp: Box<RegularExpression>) -> Box<RegularExpression> {
        Box::new(RegularExpression::ZeroOrOne(exp))
    }

    /// Converts this regular expression to an NFA.
    pub fn to_nfa(&self, action: LexerAction, priority: u32) -> NFA<LexerAction, char> {
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
            },
            RegularExpression::Single(character) => {
                let start_state = State::new();

                NFAFragment {
                    start_state,
                    exit_transition: (start_state, Some(character)),
                    transitions: Vec::new()
                }
            },
            RegularExpression::Union(ref first, ref second) => {
                let first_fragment = first.to_fragment();
                let second_fragment = second.to_fragment();
                let (first_end, first_char) = first_fragment.exit_transition;
                let (second_end, second_char) = second_fragment.exit_transition;
                let start_state = State::new();
                let end_state = State::new();

                let mut transitions =
                    vec![Transition::new(start_state, None, first_fragment.start_state),
                         Transition::new(start_state, None, second_fragment.start_state),
                         Transition::new(first_end, first_char, end_state),
                         Transition::new(second_end, second_char, end_state)];

                transitions.extend(first_fragment.transitions);
                transitions.extend(second_fragment.transitions);

                NFAFragment {
                    start_state,
                    exit_transition: (end_state, None),
                    transitions
                }
            },
            RegularExpression::BigUnion(ref string) => {
                let start_state = State::new();
                let end_state = State::new();

                let mut transitions = Vec::new();

                for character in string.chars() {
                    transitions.push(Transition::new(start_state, Some(character), end_state));
                }

                NFAFragment {
                    start_state,
                    exit_transition: (end_state, None),
                    transitions
                }
            },
            RegularExpression::Range(start_char, end_char) => {
                assert!(start_char < end_char);

                let start_state = State::new();
                let end_state = State::new();

                let mut transitions = Vec::new();

                for character in char_iter::new(start_char, end_char) {
                    transitions.push(Transition::new(start_state, Some(character), end_state));
                }

                NFAFragment {
                    start_state,
                    exit_transition: (end_state, None),
                    transitions
                }
            },
            RegularExpression::Concatenation(ref first, ref second) => {
                let first_fragment = first.to_fragment();
                let second_fragment = second.to_fragment();
                let (first_end, first_char) = first_fragment.exit_transition;

                let mut transitions =
                    vec![Transition::new(first_end, first_char, second_fragment.start_state)];

                transitions.extend(first_fragment.transitions);
                transitions.extend(second_fragment.transitions);

                NFAFragment {
                    start_state: first_fragment.start_state,
                    exit_transition: second_fragment.exit_transition,
                    transitions
                }
            },
            RegularExpression::ZeroOrMore(ref exp) => {
                let fragment = exp.to_fragment();
                let (fragment_end, fragment_char) = fragment.exit_transition;
                let start_state = State::new();

                let mut transitions =
                    vec![Transition::new(start_state, None, fragment.start_state),
                         Transition::new(fragment_end, fragment_char, start_state)];

                transitions.extend(fragment.transitions);

                NFAFragment {
                    start_state,
                    exit_transition: (start_state, None),
                    transitions
                }
            },
            RegularExpression::OneOrMore(ref exp) => {
                let fragment = exp.to_fragment();
                let (fragment_end, fragment_char) = fragment.exit_transition;
                let start_state = State::new();
                let end_state = State::new();

                let mut transitions =
                    vec![Transition::new(start_state, None, fragment.start_state),
                         Transition::new(end_state, None, start_state),
                         Transition::new(fragment_end, fragment_char, end_state)];

                transitions.extend(fragment.transitions);

                NFAFragment {
                    start_state,
                    exit_transition: (end_state, None),
                    transitions
                }
            },
            RegularExpression::ZeroOrOne(ref exp) => {
                let fragment = exp.to_fragment();
                let (fragment_end, fragment_char) = fragment.exit_transition;
                let start_state = State::new();
                let end_state = State::new();

                let mut transitions =
                    vec![Transition::new(start_state, None, fragment.start_state),
                         Transition::new(start_state, None, end_state),
                         Transition::new(fragment_end, fragment_char, end_state)];

                transitions.extend(fragment.transitions);

                NFAFragment {
                    start_state,
                    exit_transition: (end_state, None),
                    transitions
                }
            },
            RegularExpression::EverythingBut(ref symbols) => {
                let start_state = State::new();
                let end_state = State::new();

                let transitions =
                    vec![Transition::new_indirect(start_state, symbols.clone(), end_state)];

                NFAFragment {
                    start_state,
                    exit_transition: (end_state, None),
                    transitions
                }
            },
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
    transitions: Vec<Transition<char>>
}

impl NFAFragment {
    /// Converts the NFA fragment to an NFA.
    fn to_nfa(self, action: LexerAction, priority: u32) -> NFA<LexerAction, char> {
        let start_state = self.start_state;
        let mut transitions = self.transitions;
        let (last_state, last_character) = self.exit_transition;

        let mut accepting_states = HashMap::new();

        if last_character.is_some() {
            let end_state = State::new();
            transitions.push(Transition::new(last_state, last_character, end_state));

            accepting_states.insert(end_state, (priority, action));
        } else {
            accepting_states.insert(last_state, (priority, action));
        }

        NFA::new(transitions, start_state, accepting_states)
    }
}

#[cfg(test)]
mod tests {
    use super::RegularExpression::*;

    #[test]
    fn ranges() {
        assert_eq!(reg_exp!([0 - 9]), Box::new(Range('0', '9')));

        assert_eq!(reg_exp!([a - z]), Box::new(Range('a', 'z')));

        assert_eq!(reg_exp!([A - Z]), Box::new(Range('A', 'Z')));
    }

    #[test]
    fn simple_concatenation() {
        assert_eq!(reg_exp!("a"), Box::new(Single('a')));

        assert_eq!(reg_exp!("foo"),
                   Box::new(Concatenation(Box::new(Concatenation(Box::new(Single('f')),
                                                                 Box::new(Single('o')))),
                                          Box::new(Single('o')))));
    }

    #[test]
    fn simple_union() {
        assert_eq!(reg_exp!([""]), Box::new(Epsilon));

        assert_eq!(reg_exp!(["x"]), Box::new(Single('x')));

        assert_eq!(reg_exp!(["test"]), Box::new(BigUnion("test")));
    }

    #[test]
    fn zero_or_more() {
        assert_eq!(zero_or_more!(reg_exp!([A - Z])),
                   Box::new(ZeroOrMore(Box::new(Range('A', 'Z')))));
    }

    #[test]
    fn one_or_more() {
        assert_eq!(one_or_more!(reg_exp!([A - Z])),
                   Box::new(OneOrMore(Box::new(Range('A', 'Z')))));
    }

    #[test]
    fn zero_or_one() {
        assert_eq!(zero_or_one!(reg_exp!([A - Z])),
                   Box::new(ZeroOrOne(Box::new(Range('A', 'Z')))));
    }

    #[test]
    fn concatenation() {
        assert_eq!(reg_exp!([0 - 9]) & reg_exp!("a"),
                   Box::new(Concatenation(Box::new(Range('0', '9')), Box::new(Single('a')))));
    }

    #[test]
    fn union() {
        assert_eq!(reg_exp!(":") | reg_exp!(")"),
                   Box::new(Union(Box::new(Single(':')), Box::new(Single(')')))));
    }

    #[test]
    fn everything_but() {
        assert_eq!(reg_exp!(!["Aj+0§]"]),
                   Box::new(EverythingBut(vec!['A', 'j', '+', '0', '§', ']'])));
    }
}
