//! This module is supposed to define the grammar for the language.

use super::ast::AST;
use super::ast::AST::*;
use super::nonterminal::Nonterminal;
use super::nonterminal::Nonterminal::*;
use super::token::ParenthesesType::*;
use super::token::Token;
use parser::{Associativity, Operator, Parser};
use parser::grammar::Grammar;

lazy_static! {
    /// This defines the parser for the language.
    pub static ref PARSER: Parser<Nonterminal, Token, AST> = {
        macro_rules! match_operator {
            ($operator: expr) => {
                terminal_matcher!($operator, Token::Operator(operator) => operator == $operator)
            }
        }

        let int = terminal_matcher!("Integer", Token::Integer(_));
        let plus = match_operator!("+");
        let minus = match_operator!("-");
        let times = match_operator!("*");
        let divides = match_operator!("/");
        let parentheses_open = terminal_matcher!("(", Token::Parentheses(Opening));
        let parentheses_close = terminal_matcher!(")", Token::Parentheses(Closing));

        macro_rules! extract_binary_arguments {
            ($symbols: expr) => {{
                let rhs = Box::new($symbols.remove(2));
                let lhs = Box::new($symbols.remove(0));
                (lhs, rhs)
            }}
        }

        let productions = vec![
            production!([Program => Expression],
                        |mut symbols| {
                            TEST(Box::new(symbols.remove(0)))
                        }),
            production!([Expression => Expression, plus, Expression],
                        |mut symbols| {
                            let (lhs, rhs) = extract_binary_arguments!(symbols);
                            Addition(lhs, rhs)
                        }),
            production!([Expression => Expression, minus, Expression],
                        |mut symbols| {
                            let (lhs, rhs) = extract_binary_arguments!(symbols);
                            Subtraction(lhs, rhs)
                        }),
            production!([Expression => minus, Expression],
                        |mut symbols| {
                            Negation(Box::new(symbols.remove(1)))
                        }),
            production!([Expression => Expression, times, Expression],
                        |mut symbols| {
                            let (lhs, rhs) = extract_binary_arguments!(symbols);
                            Multiplication(lhs, rhs)
                        }),
            production!([Expression => Expression, divides, Expression],
                        |mut symbols| {
                            let (lhs, rhs) = extract_binary_arguments!(symbols);
                            Division(lhs, rhs)
                        }),
            production!([Expression => parentheses_open, Expression, parentheses_close],
                        |mut symbols| {
                            symbols.remove(1)
                        }),
            production!([Expression => int],
                        |mut symbols| {
                            match symbols.remove(0).token() {
                                Token::Integer(value) => Integer(value),
                                _ => panic!("Non-integer match on an integer.")
                            }
                        })
        ];

        let operators = vec![
            Operator::new(plus, Associativity::Left, 1),
            Operator::new(minus, Associativity::Left, 1),
            Operator::new(times, Associativity::Left, 0),
            Operator::new(divides, Associativity::Left, 0)
        ];

        let grammar = Grammar::new(Program, productions);

        grammar.to_parser(operators)
    };
}
