//! This module is supposed to define the grammer for the language.

use super::nonterminal::Nonterminal;
use super::nonterminal::Nonterminal::*;
use super::token::Token;
use super::token::Token::*;
use super::token::ParenthesesType::*;
use parser::grammer::Grammer;

lazy_static! {
    /// This defines the grammer for the language.
    pub static ref GRAMMER: Grammer<Nonterminal, Token> = {
        let int = terminal_matcher!(Integer(_));
        let plus = terminal_matcher!(Operator(operator) => operator == "+");
        let parentheses_open = terminal_matcher!(Parentheses(Opening));
        let parentheses_close = terminal_matcher!(Parentheses(Closing));

        let productions = vec![
            production!(Program => Expression),
            production!(Expression => Expression, plus, Expression),
            production!(Expression => parentheses_open, Expression, parentheses_close),
            production!(Expression => int)
        ];

        let grammer = Grammer::new(Program, productions);

        grammer
    };
}
