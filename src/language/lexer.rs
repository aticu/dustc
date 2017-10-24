//! This module is supposed to define the definitions for the lexer.

use language::errors::PROBLEMS;
use language::token::{ParenthesesType, Token};
use lexer::{Lexer, LexerDescription};
use problem_reporting::{InputPosition, Problem};

/// The problem number for an unknown escape sequence.
const UNKNOWN_ESCAPE_SEQUENCE: usize = 2;

lazy_static! {
    /// This defines the lexer descriptions for the lexer.
    pub static ref LEXER: Lexer = {
        let descriptions = vec![
            // KEYWORDS
            LexerDescription::new(reg_exp!("if") | reg_exp!("fn") | reg_exp!("let"),
                                  |token, _| {
                                      Ok(Token::Keyword(token.to_owned()))
                                  }
            ),

            // IDENTIFIERS
            LexerDescription::new((reg_exp!([a-zA-Z]) | reg_exp!("_")) &
                                  zero_or_more!(reg_exp!([a-zA-Z0-9]) | reg_exp!("_")),
                                  |token, _| {
                                      Ok(Token::Identifier(token.to_owned()))
                                  }
            ),

            // PARENTHESIS
            LexerDescription::new(reg_exp!(["()"]),
                                  |token, _| {
                                      let token: &str = &token;
                                      match token {
                                          "(" => Ok(Token::Parentheses(ParenthesesType::Opening)),
                                          ")" => Ok(Token::Parentheses(ParenthesesType::Closing)),
                                          _ => panic!("Unexpected parenthesis match in the lexer.")
                                      }
                                  }
            ),

            // BRACKETS
            LexerDescription::new(reg_exp!(["[]"]),
                                  |token, _| {
                                      let token: &str = &token;
                                      match token {
                                          "[" => Ok(Token::Brackets(ParenthesesType::Opening)),
                                          "]" => Ok(Token::Brackets(ParenthesesType::Closing)),
                                          _ => panic!("Unexpected braces match in the lexer.")
                                      }
                                  }
            ),

            // BRACES
            LexerDescription::new(reg_exp!(["{}"]),
                                  |token, _| {
                                      let token: &str = &token;
                                      match token {
                                          "{" => Ok(Token::Braces(ParenthesesType::Opening)),
                                          "}" => Ok(Token::Braces(ParenthesesType::Closing)),
                                          _ => panic!("Unexpected braces match in the lexer.")
                                      }
                                  }
            ),

            // OPERATORS
            LexerDescription::new(reg_exp!(["+-*/><=|&"]) & zero_or_one!(reg_exp!("=")),
                                  |token, _| {
                                      Ok(Token::Operator(token))
                                  }
            ),

            // INTEGERS
            LexerDescription::new(one_or_more!(reg_exp!([0-9])),
                                  |token, _| {
                                      Ok(Token::Integer(token))
                                  }
            ),

            // STATEMENT SEPARATOR
            LexerDescription::new(reg_exp!(";"),
                                  |_, _| {
                                      Ok(Token::StatementSeparator)
                                  }
            ),

            // STRING
            LexerDescription::new(reg_exp!("\"") &
                                  zero_or_more!(
                                      reg_exp!("\\\"") |
                                      (reg_exp!("\\") & reg_exp!(!["\""])) |
                                      reg_exp!(!["\\\""])) &
                                  reg_exp!("\""),
                                  |token, input_position| {
                                      parse_string(token, input_position)
                                  }
            ),

            // COMMENT
            LexerDescription::new(reg_exp!("//") & zero_or_more!(reg_exp!(!["\n"])),
                                  |_, _| {
                                      Err(Vec::new())
                                  }
            ),

            // WHITESPACE
            LexerDescription::new(reg_exp!([" \n\r\t"]),
                                  |_, _| {
                                      Err(Vec::new())
                                  }
            )
        ];
        Lexer::new(descriptions)
    };
}

/// This function parses a string from the source code.
fn parse_string(input_string: String,
                input_position: InputPosition)
                -> Result<Token, Vec<Problem>> {
    enum State {
        Escape,
        Normal
    };

    let mut string = String::new();
    let mut state = State::Normal;

    let mut errors = Vec::new();

    for (index, character) in
        input_string
            .chars()
            .skip(1)
            .take(input_position.length - 2)
            .enumerate() {
        match state {
            State::Escape => {
                match character {
                    '"' => {
                        string.push('"');
                        state = State::Normal;
                    },
                    _ => {
                        let input_position = InputPosition::new(input_position.file,
                                                                input_position.index + index,
                                                                2);
                        let problem = Problem::new(&PROBLEMS[UNKNOWN_ESCAPE_SEQUENCE],
                                                   input_position);
                        errors.push(problem);
                        state = State::Normal;
                    },
                }
            },
            State::Normal => {
                match character {
                    '\\' => state = State::Escape,
                    _ => string.push(character),
                }
            },
        }
    }

    if errors.len() > 0 {
        Err(errors)
    } else {
        Ok(Token::String(string))
    }
}
