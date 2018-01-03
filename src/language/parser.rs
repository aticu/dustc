//! This module is supposed to define the grammar for the language.

use super::ast::AST;
use super::expression::Expression;
use super::nonterminal::Nonterminal;
use super::nonterminal::Nonterminal::*;
use super::statement::Statement;
use super::token::ParenthesesType::*;
use super::token::Token;
use super::types::Type;
use parser::{Associativity, Operator, Parser};
use parser::grammar::Grammar;

lazy_static! {
    /// This defines the parser for the language.
    pub static ref PARSER: Parser<Nonterminal, Token, AST> = {
        macro_rules! match_operator {
            ($operator: expr) => {
                terminal_matcher!(concat!("`", $operator, "`"),
                                  Token::Operator(operator) => operator == $operator)
            }
        }

        macro_rules! match_keywords {
            ($string: expr, $($keyword: expr),*) => {
                match $string.as_ref() {
                    $(
                        $keyword => true,
                    )*
                    _ => false
                }
            }
        }

        macro_rules! binary_operator_production {
            ($operator: expr, $ast: path) => {{
                production!([Expression => Expression, $operator, Expression],
                            |mut symbols| {
                                let rhs = Box::new(symbols.remove(2).extract_expression());
                                let lhs = Box::new(symbols.remove(0).extract_expression());
                                AST::Expression($ast(lhs, rhs))
                            })
            }}
        }

        // Operator matchers
        let plus = match_operator!("+");
        let minus = match_operator!("-");
        let times = match_operator!("*");
        let divides = match_operator!("/");

        // Symbols matchers
        let parentheses_open = terminal_matcher!("`(`", Token::Parentheses(Opening));
        let parentheses_close = terminal_matcher!("`)`", Token::Parentheses(Closing));
        let braces_open = terminal_matcher!("`{`", Token::Braces(Opening));
        let braces_close = terminal_matcher!("`}`", Token::Braces(Closing));
        let statement_separator = terminal_matcher!("`;`", Token::StatementSeparator);

        let int = terminal_matcher!("an integer", Token::Integer(_));
        let identifier = terminal_matcher!("an identifier", Token::Identifier(_));

        let primitive_type = terminal_matcher!("a primitive type",
                                               Token::Keyword(keyword) =>
                                                   match_keywords!(keyword, "u64", "i64"));
        let fn_keyword = terminal_matcher!("fn",
                                           Token::Keyword(keyword) =>
                                            match_keywords!(keyword, "fn"));

        let productions = vec![
            // MODULES
            production!([Module => ModuleContent],
                        |mut symbols| {
                            symbols.remove(0)
                        }),
            production!([ModuleContent => ModuleContent, Function],
                        |mut symbols| {
                            let function = symbols.remove(1);
                            let module = symbols.remove(0);

                            let mut list = match module {
                                AST::Module(module) => module,
                                _ => unreachable!()
                            };

                            list.push(function);

                            AST::Module(list)
                        }),
            production!([ModuleContent => ],
                        |_| {
                            AST::Module(Vec::new())
                        }),

            // EXPRESSIONS
            binary_operator_production!(plus, Expression::Addition),
            binary_operator_production!(minus, Expression::Subtraction),
            binary_operator_production!(times, Expression::Multiplication),
            binary_operator_production!(divides, Expression::Division),
            production!([Expression => minus, Expression],
                        |mut symbols| {
                            let expr = symbols.remove(1).extract_expression();
                            AST::Expression(Expression::Negation(Box::new(expr)))
                        }),
            production!([Expression => parentheses_open, Expression, parentheses_close],
                        |mut symbols| {
                            symbols.remove(1)
                        }),
            production!([Expression => int],
                        |mut symbols| {
                            let int = match symbols.remove(0).extract_token() {
                                Token::Integer(value) => value,
                                _ => unreachable!("Non-integer match on an integer.")
                            };
                            AST::Expression(Expression::Integer(int))
                        }),
            production!([Expression => identifier],
                        |mut symbols| {
                            let name = match symbols.remove(0).extract_token() {
                                Token::Identifier(name) => name,
                                _ => unreachable!("Non-identifier match on an identifier.")
                            };
                            AST::Expression(Expression::Identifier(name))
                        }),
            production!([Expression => Block],
                        |mut symbols| {
                            AST::Expression(symbols.remove(0).extract_expression())
                        }),

            // STATEMENTS AND BLOCKS
            production!([Statements => Statement],
                        |mut symbols| {
                            let statement = symbols.remove(0).extract_statement();
                            let possible_expression = statement.extract_expression();
                            match possible_expression {
                                Ok(expression) => AST::Expression(Expression::Block(
                                        vec![],
                                        Some(Box::new(expression)))),
                                Err(statement) => AST::Expression(Expression::Block(
                                        vec![statement],
                                        None))
                            }
                        }),
            production!([Statements => Statement, statement_separator, Statements],
                        |mut symbols| {
                            let expr = symbols.remove(2).extract_expression();
                            let (mut statements, expression) = match expr {
                                Expression::Block(statements, expression) =>
                                    (statements, expression),
                                _ => unreachable!()
                            };
                            let statement = symbols.remove(0).extract_statement();

                            statements.insert(0, statement);

                            AST::Expression(Expression::Block(statements, expression))
                        }),
            production!([Block => braces_open, Statements, braces_close],
                        |mut symbols| {
                            let expr = symbols.remove(1).extract_expression();
                            let (mut statements, expression) = match expr {
                                Expression::Block(statements, expression) =>
                                    (statements, expression),
                                _ => unreachable!()
                            };

                            statements = statements
                                .into_iter()
                                .filter(|statement| match statement {
                                    &Statement::Empty => false,
                                    _ => true
                                })
                                .collect();

                            AST::Expression(Expression::Block(statements, expression))
                        }),

            // SINGLE STATEMENTS
            production!([Statement => Expression],
                        |mut symbols| {
                            let expr = symbols.remove(0).extract_expression();
                            AST::Statement(Statement::Expression(expr))
                        }),
            production!([Statement => ],
                        |_| {
                            AST::Statement(Statement::Empty)
                        }),

            // FUNCTIONS
            production!([Function =>
                        fn_keyword,
                        identifier,
                        parentheses_open,
                        parentheses_close,
                        Block],
                        |mut symbols| {
                            let content = symbols.remove(4).extract_expression();
                            let name = match symbols.remove(1).extract_token() {
                                Token::Identifier(name) => name,
                                _ => unreachable!()
                            };

                            AST::Function(name, content)
                        }),

            // TYPES
            production!([Type => primitive_type],
                        |mut symbols| {
                            match symbols.remove(0).extract_token() {
                                Token::Keyword(value) => AST::Type(Type::PrimitiveType(value)),
                                _ => unreachable!("Non-keyword match on a primitive type.")
                            }
                        })
        ];

        let operators = vec![
            Operator::new(plus, Associativity::Left, 1),
            Operator::new(minus, Associativity::Left, 1),
            Operator::new(times, Associativity::Left, 0),
            Operator::new(divides, Associativity::Left, 0)
        ];

        let grammar = Grammar::new(ModuleContent, productions);

        grammar.to_parser(operators)
    };
}
