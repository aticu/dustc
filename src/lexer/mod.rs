//! The lexer module performs the lexical analysis of the given source code.
//!
//! The input is the raw unprocessed string of the file and the result is
//! a list of annotated tokens.

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

/// The problem number for an unmatched '"' character.
const UNMATCHED_STRING: usize = 1;

/// A token stream is a list of tokens.
pub type TokenStream = Vec<Token>;

/// This is the type of the function that a lexer action calls.
///
/// This may return a token which will be fed to the next compilation phase.
/// It may also return nothing if there is no applicable token.
///
/// The arguments are used to decide which token to create.
/// - The first argument is the string which the lexer identified as the token string.
/// - The second argument is the position of the token in the input.
type LexerAction = fn(String, InputPosition) -> Result<Token, Vec<Problem>>;

/// Represents a regular expression the lexer will search for and an associated action.
pub struct LexerDescription {
    /// The regular expression the lexer will search for.
    reg_exp: Box<RegularExpression>,
    /// The action that is performed when the expression was found.
    action: LexerAction,
}

impl LexerDescription {
    /// Creates a new ´LexerDescription´ which can be used to generate a lexer.
    pub fn new(reg_exp: Box<RegularExpression>, action: LexerAction) -> LexerDescription {
        LexerDescription { reg_exp, action }
    }
}

/// Represents a lexer that can be fed a program.
#[derive(Debug)]
pub struct Lexer {
    /// The DFA this lexer uses.
    dfa: DFA<LexerAction, char>,
}

impl Lexer {
    /// Generates the lexer from the given lexer descriptions.
    pub fn new(descriptions: Vec<LexerDescription>) -> Lexer {
        assert!(descriptions.len() > 0, "Trying to generate an empty lexer.");

        let mut nfas = Vec::new();

        for (priority, description) in descriptions.into_iter().enumerate() {
            nfas.push(description
                          .reg_exp
                          .to_nfa(description.action, priority as u32));
        }

        let dfa = NFA::combine(nfas).to_dfa();

        Lexer { dfa }
    }

    /// Runs the lexer on the given text returning the token stream read.
    pub fn run<'a>(&self, file: &'a FileHandle) -> Result<TokenStream, Vec<Problem<'a>>> {
        // To shorten future accesses.
        let text = &file.content;
        let dfa = &self.dfa;

        let mut current_state = self.dfa.start();
        let mut last_accepting_state = None;
        let mut last_accepting_index = 0;

        if dfa.is_accepting(current_state) {
            last_accepting_state = Some(current_state);
        }

        let mut reader = text.chars().enumerate().peekable();
        let mut tokens = Vec::new();

        let mut errors = Vec::new();

        let mut last_accepted_reader = reader.clone();

        macro_rules! add_token {
            ($accepting_state: expr, $end_index: expr) => {{
                let action = dfa.get_accepting_value($accepting_state).unwrap();

                let token = create_token(&mut last_accepted_reader, action, $end_index, &file);

                match token {
                    Ok(token) => tokens.push(token),
                    Err(error_list) => errors.extend(error_list)
                }

                last_accepting_state = None;
                current_state = self.dfa.start();
                reader = last_accepted_reader.clone();
            }};
        }

        while last_accepted_reader.peek().is_some() {
            while let Some((index, character)) = reader.next() {
                let next_state = dfa.transition(current_state, character);

                if let Some(next_state) = next_state {
                    current_state = next_state;

                    if dfa.is_accepting(next_state) {
                        last_accepting_state = Some(next_state);
                        last_accepting_index = index;
                    }
                } else {
                    if let Some(accepting_state) = last_accepting_state {
                        add_token!(accepting_state, Some(last_accepting_index + 1));
                    } else {
                        let input_position = InputPosition::new(file, index, 1);
                        let problem = Problem::new(&PROBLEMS[UNKNOWN_TOKEN], input_position);
                        return Err(vec![problem]);
                    }
                }
            }

            if let Some(accepting_state) = last_accepting_state {
                add_token!(accepting_state, None);
            } else {
                if let Some((index, character)) = last_accepted_reader.next() {
                    let input_position = InputPosition::new(file, index, 1);

                    let problem = if character == '"' {
                        Problem::new(&PROBLEMS[UNMATCHED_STRING], input_position)
                    } else {
                        Problem::new(&PROBLEMS[UNKNOWN_TOKEN], input_position)
                    };

                    return Err(vec![problem]);
                }
                panic!("Aborting lexing, even though the output was successfully read.");
            }
        }

        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(tokens)
        }
    }
}

/// Creates a token from the given reader, the action, the end_index and the text.
fn create_token<'a>(reader: &mut Peekable<Enumerate<Chars>>,
                    action: LexerAction,
                    end_index: Option<usize>,
                    file: &'a FileHandle)
                    -> Result<Token, Vec<Problem<'a>>> {
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
        token_string.push_str(&reader
                                   .map(|(_, character)| character)
                                   .collect::<String>());
    }

    let input_position = InputPosition::new(file, first_iterator_index, token_string.len());

    action(token_string, input_position)
}

#[cfg(test)]
mod tests {
    use super::{Lexer, LexerDescription, UNKNOWN_TOKEN};
    use language::token::Token::*;
    use file_handle::FileHandle;
    use language::errors::PROBLEMS;
    use problem_reporting::{Problem, InputPosition};

    fn generate_test_lexer() -> Lexer {
        Lexer::new(vec![LexerDescription::new(zero_or_one!(reg_exp!("-")) &
                                              one_or_more!(reg_exp!([0 - 9])),
                                              |token, _| Ok(Integer(token.to_owned()))),
                        LexerDescription::new(reg_exp!("keyword") |
                                              reg_exp!("cOmPlicated keyWORD"),
                                              |token, _| Ok(Keyword(token.to_owned()))),
                        LexerDescription::new((reg_exp!([a - zA - Z]) | reg_exp!("_")) &
                                              zero_or_more!(reg_exp!([a - zA - Z0 - 9]) |
                                                            reg_exp!("_")),
                                              |token, _| Ok(Identifier(token.to_owned()))),
                        LexerDescription::new(reg_exp!(" "), |_, _| Err(Vec::new()))])
    }

    #[test]
    fn small_file() {
        let lexer = generate_test_lexer();
        let file = FileHandle::test_new("file_name".to_owned(), "123 te_ST123".to_owned());

        assert_eq!(lexer.run(&file),
                   Ok(vec![Integer("123".to_owned()), Identifier("te_ST123".to_owned())]));
    }

    #[test]
    #[should_panic]
    fn small_file_fails() {
        let lexer = generate_test_lexer();
        let file = FileHandle::test_new("file_name".to_owned(), "123 te_ST123".to_owned());

        assert_eq!(lexer.run(&file),
                   Ok(vec![Integer("456".to_owned()), Identifier("te_ST123".to_owned())]));
    }

    #[test]
    fn unknown_token() {
        let lexer = generate_test_lexer();
        let file = FileHandle::test_new("file_name".to_owned(), "123 §te_ST123".to_owned());

        assert_eq!(lexer.run(&file),
                   Err(vec![Problem::new(&PROBLEMS[UNKNOWN_TOKEN],
                                         InputPosition::new(&file, 4, 1))]));
    }

    #[test]
    #[should_panic]
    fn unknown_token_fails() {
        let lexer = generate_test_lexer();
        let file = FileHandle::test_new("file_name".to_owned(), "123 §te_ST123".to_owned());

        assert_eq!(lexer.run(&file),
                   Err(vec![Problem::new(&PROBLEMS[UNKNOWN_TOKEN],
                                         InputPosition::new(&file, 5, 1))]));
    }

    #[test]
    fn many_identifiers() {
        let lexer = generate_test_lexer();
        let file = FileHandle::test_new("file_name".to_owned(),
                                        "a1234 B4DB0Y normal_name other_thing I_RAGE_ALOT"
                                            .to_owned());

        assert_eq!(lexer.run(&file),
                   Ok(vec![Identifier("a1234".to_owned()),
                           Identifier("B4DB0Y".to_owned()),
                           Identifier("normal_name".to_owned()),
                           Identifier("other_thing".to_owned()),
                           Identifier("I_RAGE_ALOT".to_owned())]));
    }

    #[test]
    fn many_integers() {
        let lexer = generate_test_lexer();
        let file = FileHandle::test_new("file_name".to_owned(),
                                        "42 -743 1337 -3243 -42".to_owned());

        assert_eq!(lexer.run(&file),
                   Ok(vec![Integer("42".to_owned()),
                           Integer("-743".to_owned()),
                           Integer("1337".to_owned()),
                           Integer("-3243".to_owned()),
                           Integer("-42".to_owned())]));
    }

    #[test]
    fn white_space_ignored() {
        let lexer = generate_test_lexer();
        let file = FileHandle::test_new("file_name".to_owned(), "42    test   -42".to_owned());

        assert_eq!(lexer.run(&file),
                   Ok(vec![Integer("42".to_owned()),
                           Identifier("test".to_owned()),
                           Integer("-42".to_owned())]));
    }

    #[test]
    fn precedence() {
        let lexer = generate_test_lexer();
        let file = FileHandle::test_new("file_name".to_owned(),
                                        "keyword cOmPlicated keyWORD cOmPlicated  keyWORD"
                                            .to_owned());

        assert_eq!(lexer.run(&file),
                   Ok(vec![Keyword("keyword".to_owned()),
                           Keyword("cOmPlicated keyWORD".to_owned()),
                           Identifier("cOmPlicated".to_owned()),
                           Identifier("keyWORD".to_owned())]));
    }

    #[test]
    fn everything_but() {
        let lexer =
            Lexer::new(vec![LexerDescription::new(zero_or_more!(reg_exp!(!["abc"])),
                                                  |token, _| Ok(Identifier(token.to_owned())))]);

        let should_fail = FileHandle::test_new("file_name".to_owned(),
                                               "8734)§&gf$''\nsfkj'$a768sadgk".to_owned());

        assert_eq!(lexer.run(&should_fail),
                   Err(vec![Problem::new(&PROBLEMS[UNKNOWN_TOKEN],
                                         InputPosition::new(&should_fail, 19, 1))]));

        let should_work = FileHandle::test_new("file_name".to_owned(),
                                               "8734)§&gf$''\nsfkj'$768sdgk".to_owned());

        assert_eq!(lexer.run(&should_work),
                   Ok(vec![Identifier("8734)§&gf$''\nsfkj'$768sdgk".to_owned())]));
    }
}
