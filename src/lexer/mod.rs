//! The lexer module performs the lexical analysis of the given source code.
//!
//! The input is the raw unprocessed string of the file and the result is a list of annotated tokens.

#[macro_use]
pub mod regexp;

use automata::dfa::DFA;
use automata::nfa::NFA;
use file_handle::FileHandle;
use language::errors::PROBLEMS;
use language::token::Token;
use std::iter::{Peekable, Enumerate};
use std::str::Chars;
use problem_reporting::{InputPosition, Problem};

pub use self::regexp::RegularExpression;

/// The problem number for an invalid token character.
const UNKNOWN_TOKEN: usize = 0;

/// A token stream is a list of tokens.
pub type TokenStream = Vec<Token>;

/// This is the type of the function that a lexer action calls.
///
/// This may return a token which will be fed to the next compilation phase. It may also return nothing if there is no applicable token.
///
/// The arguments are used to decide which token to create. The first argument is a convenience argument, the others are mainly for error reporting.
/// - The first argument is the string which the lexer identified as the token string.
/// - The second argument is the whole input string.
/// - The third argument is the the start position of the token in the whole input.
///
/// The length of the given section of the input string can be obtained from the first argument.
type LexerAction = fn(&str, &str, usize) -> Option<Token>;

/// Represents a regular expression the lexer will search for and an associated action.
pub struct LexerDescription {
    /// The regular expression the lexer will search for.
    reg_exp: Box<RegularExpression>,
    /// The action that is performed when the expression was found.
    action: LexerAction
}

impl LexerDescription {
    /// Creates a new ´LexerDescription´ which can be used to generate a lexer.
    pub fn new(reg_exp: Box<RegularExpression>, action: LexerAction) -> LexerDescription {
        LexerDescription {
            reg_exp,
            action
        }
    }
}

/// Represents a lexer that can be fed a program.
pub struct Lexer {
    /// The DFA this lexer uses.
    dfa: DFA<LexerAction>
}

impl Lexer {
    /// Generates the lexer from the given lexer descriptions.
    pub fn new(descriptions: Vec<LexerDescription>) -> Lexer {
        assert!(descriptions.len() > 0, "Trying to generate an empty lexer.");

        let mut nfas = Vec::new();

        for (priority, description) in descriptions.into_iter().enumerate() {
            nfas.push(description.reg_exp.to_nfa(description.action, priority as u32));
        }

        let dfa = NFA::combine(nfas).to_dfa();

        Lexer {
            dfa
        }
    }

    /// Runs the lexer on the given text returning the token stream read.
    pub fn run<'a>(&self, file: &'a FileHandle) -> Result<TokenStream, Vec<Problem<'a>>> {
        // To shorten future accesses.
        let text = &file.content;
        let dfa = &self.dfa;

        let mut current_state = self.dfa.start();
        let mut last_accepting_state = None;

        if dfa.is_accepting(current_state) {
            last_accepting_state = Some(current_state);
        }

        let mut reader = text.chars().enumerate().peekable();
        let mut tokens = Vec::new();

        let mut last_accepted_reader = reader.clone();

        while last_accepted_reader.peek().is_some() {
            while let Some((index, character)) = reader.next() {
                let next_state = dfa.transition(current_state, character);

                if let Some(next_state) = next_state {
                    current_state = next_state;

                    if dfa.is_accepting(next_state) {
                        last_accepting_state = Some(next_state);
                    }
                } else {
                    if let Some(accepting_state) = last_accepting_state {
                        let action = dfa.get_accepting_value(accepting_state).unwrap();

                        let token = create_token(&mut last_accepted_reader, action, Some(index), &text);

                        if let Some(token) = token {
                            tokens.push(token);
                        }

                        last_accepting_state = None;
                        current_state = self.dfa.start();
                        reader = last_accepted_reader.clone();
                    } else {
                        let input_position = InputPosition::new(file, index, 1);
                        let problem = Problem::new(&PROBLEMS[UNKNOWN_TOKEN], input_position);
                        return Err(vec![problem]);
                    }
                }
            }

            if let Some(accepting_state) = last_accepting_state {
                let action = dfa.get_accepting_value(accepting_state).unwrap();

                let token = create_token(&mut last_accepted_reader, action, None, &text);

                if let Some(token) = token {
                    tokens.push(token);
                }

                last_accepting_state = None;
                current_state = self.dfa.start();
                reader = last_accepted_reader.clone();
            } else {
                if let Some((index, _)) = last_accepted_reader.next() {
                    let input_position = InputPosition::new(file, index, 1);
                    let problem = Problem::new(&PROBLEMS[UNKNOWN_TOKEN], input_position);
                    return Err(vec![problem]);
                }
                panic!("Aborting lexing, even though the output was successfully read.");
            }
        }

        Ok(tokens)
    }
}

/// Creates a token from the given reader, the action, the end_index and the text.
fn create_token(reader: &mut Peekable<Enumerate<Chars>>, action: LexerAction, end_index: Option<usize>, text: &str) -> Option<Token> {
    let mut token_string: String = String::new();

    let (first_iterator_index, first_char) = reader.next().unwrap();
    token_string.push(first_char);

    if let Some(end_index) = end_index {
        // Collect chars to the end index.
        let mut iterator_index = first_iterator_index;

        while iterator_index < end_index - 1 {
            let (next_iterator_index, char_to_add) = reader.next().unwrap();

            iterator_index = next_iterator_index;

            token_string.push(char_to_add);
        }
    } else {
        // Just collect everything if there is no end index.
        token_string.push_str(&reader.map(|(_, character)| character).collect::<String>());
    }

    action(&token_string, text, first_iterator_index)
}
