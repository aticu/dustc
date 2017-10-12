//! This module is supposed to define the definitions for the lexer.

use lexer::{LexerDescription, Lexer};
use language::token::{Token, ParenthesesType};

lazy_static! {
    /// This defines the lexer descriptions for the lexer.
    pub static ref LEXER: Lexer = {
        let descriptions = vec![
            // KEYWORDS
            LexerDescription::new(reg_exp!("if") | reg_exp!("fn") | reg_exp!("let"),
                                  |token, _| {
                                      Some(Token::Keyword(token.to_owned()))
                                  }
            ),

            // IDENTIFIERS
            LexerDescription::new((reg_exp!([a-zA-Z]) | reg_exp!("_")) &
                                  zero_or_more!(reg_exp!([a-zA-Z0-9]) | reg_exp!("_")),
                                  |token, _| {
                                      Some(Token::Identifier(token.to_owned()))
                                  }
            ),

            // PARENTHESIS
            LexerDescription::new(reg_exp!(["()"]),
                                  |token, _| {
                                      match token {
                                          "(" => Some(Token::Parentheses(ParenthesesType::Opening)),
                                          ")" => Some(Token::Parentheses(ParenthesesType::Closing)),
                                          _ => panic!("Unexpected parenthesis match in the lexer.")
                                      }
                                  }
            ),

            // BRACKETS
            LexerDescription::new(reg_exp!(["[]"]),
                                  |token, _| {
                                      match token {
                                          "[" => Some(Token::Brackets(ParenthesesType::Opening)),
                                          "]" => Some(Token::Brackets(ParenthesesType::Closing)),
                                          _ => panic!("Unexpected braces match in the lexer.")
                                      }
                                  }
            ),

            // BRACES
            LexerDescription::new(reg_exp!(["{}"]),
                                  |token, _| {
                                      match token {
                                          "{" => Some(Token::Braces(ParenthesesType::Opening)),
                                          "}" => Some(Token::Braces(ParenthesesType::Closing)),
                                          _ => panic!("Unexpected braces match in the lexer.")
                                      }
                                  }
            ),

            // OPERATORS
            LexerDescription::new(reg_exp!(["+-*/><=|&"]) & zero_or_one!(reg_exp!("=")),
                                  |token, _| {
                                      Some(Token::Operator(token.to_owned()))
                                  }
            ),

            // INTEGERS
            LexerDescription::new(one_or_more!(reg_exp!([0-9])),
                                  |token, _| {
                                      Some(Token::Integer(token.to_owned()))
                                  }
            ),

            // STATEMENT SEPARATOR
            LexerDescription::new(reg_exp!(";"),
                                  |_, _| {
                                      Some(Token::StatementSeparator)
                                  }
            ),

            // WHITESPACE
            LexerDescription::new(reg_exp!([" \n\r\t"]),
                                  |_, _| {
                                      None
                                  }
            )
        ];
        Lexer::new(descriptions)
    };
}
