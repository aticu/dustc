//! This module is supposed to parse the given token string.
//!
//! The input token string should be converted into an abstract syntax tree
//! (AST) which can later be used for further processing.

#[macro_use]
pub mod grammar;

use self::grammar::Production;
use self::grammar::symbol::{Symbol, TerminalMatcher};
use automata::State;
use problem_reporting::{InputPosition, Problem, ProblemDescription, Locatable};
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

/// Corresponds to an action a parser can take upon reading a symbol.
#[derive(Debug, PartialEq, Eq)]
enum ParserAction<Nonterminal, Terminal, AST>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash,
          AST: Clone + Debug + Eq + Default + From<(Terminal, InputPosition)> + Locatable
{
    /// The shift action just pushes the next input symbol onto the stack.
    Shift(State),
    /// The go action pushes a nonterminal onto the stack.
    Go(State),
    /// The reduce action reduces the top of the stack using a production.
    Reduce(Production<Nonterminal, Terminal, AST>),
    /// The accept action signals the successful completion of parsing.
    Accept
}

/// Allows to specify the associativity of operators in grammars.
#[derive(Debug, PartialEq, Eq)]
pub enum Associativity {
    /// The operator is left-associative.
    ///
    /// That means ´a + b + c´ will become ´(a + b) + c´.
    Left,
    /// The operator is right-associative.
    ///
    /// That means ´a + b + c´ will become ´a + (b + c)´.
    Right,
    /// The operator is non-associative.
    ///
    /// That means ´a + b + c´ will generator an error.
    None
}

/// Allows further specification of operators in grammars.
#[derive(Debug)]
pub struct Operator<Terminal>
    where Terminal: Clone + Debug + Eq + Hash
{
    /// The terminal which specifies the operator.
    terminal: TerminalMatcher<Terminal>,
    /// The associativity of the operator.
    associativity: Associativity,
    /// The precedence of the operator.
    ///
    /// Lower numbers mean higher precedence.
    precedence: u32
}

impl<Terminal> Operator<Terminal>
    where Terminal: Clone + Debug + Eq + Hash
{
    /// Creates a new operator specification for grammars.
    pub fn new(terminal: TerminalMatcher<Terminal>,
               associativity: Associativity,
               precedence: u32)
               -> Operator<Terminal> {
        Operator {
            terminal,
            associativity,
            precedence
        }
    }
}
/// Represents an SLR parser.
pub struct Parser<Nonterminal, Terminal, AST>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash,
          AST: Clone + Debug + Eq + Default + From<(Terminal, InputPosition)> + Locatable
{
    /// The parsing table of the parser.
    table:
        HashMap<(State, Symbol<Nonterminal, Terminal>), ParserAction<Nonterminal, Terminal, AST>>,
    /// The starting state of the parser.
    starting_state: State
}

impl<Nonterminal, Terminal, AST> Parser<Nonterminal, Terminal, AST>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash,
          AST: Clone + Debug + Eq + Default + From<(Terminal, InputPosition)> + Locatable
{
    /// Creates a new parser using the given parts.
    fn new(table: HashMap<(State, Symbol<Nonterminal, Terminal>),
                          ParserAction<Nonterminal, Terminal, AST>>,
           starting_state: State)
           -> Parser<Nonterminal, Terminal, AST> {
        Parser {
            table,
            starting_state
        }
    }
}

impl<Nonterminal, Terminal, AST> Parser<Nonterminal, Terminal, AST>
    where Nonterminal: Clone + Debug + Eq + Hash + Ord,
          Terminal: Clone + Debug + Eq + Hash + Ord,
          AST: Clone + Debug + Eq + Default + From<(Terminal, InputPosition)> + Locatable
{
    /// Returns the entry in the parsing table, if it exists.
    fn get(&self,
           key: &(State, &Symbol<Nonterminal, Terminal>))
           -> Option<&ParserAction<Nonterminal, Terminal, AST>> {
        for (&(state, ref symbol), ref action) in &self.table {
            if &(state, symbol) == key {
                return Some(&action);
            }
        }

        None
    }

    /// Returns all the transitions from the specified state.
    fn transitions_from(&self, state: State) -> Vec<Symbol<Nonterminal, Terminal>> {
        let mut transitions: Vec<Symbol<Nonterminal, Terminal>> = (&self.table)
            .into_iter()
            .filter(|&(&(other_state, _), _)| other_state == state)
            .map(|(&(_, ref symbol), _)| symbol.clone())
            .collect();
        transitions.sort();
        transitions
    }

    /// Parses the given input, producing an abstract syntax tree.
    pub fn parse(&self,
                 input_stream: Vec<(InputPosition, Terminal)>,
                 generate_problem: Box<Fn(Option<Terminal>, Vec<Symbol<Nonterminal, Terminal>>)
                                          -> ProblemDescription>)
                 -> Result<AST, Vec<Problem>> {
        // Handle the "empty file" special case.
        if input_stream.len() == 0 {
            return Ok(AST::default());
        }

        // First transform the input and add the EndOfInput symbol.
        // Also make a map from the symbols to their input positions.
        let (mut input, position_list) = {
            let mut input: Vec<Symbol<Nonterminal, Terminal>> = Vec::new();
            let mut position_list: Vec<InputPosition> = Vec::new();

            for (index, (input_position, symbol)) in input_stream.into_iter().enumerate() {
                input.push(Symbol::InputSymbol(index, symbol));
                position_list.push(input_position);
            }

            (input, position_list)
        };
        input.push(Symbol::EndOfInput);
        let mut input = input.into_iter();

        // Keep the current DFA state on a stack.
        let mut stack: Vec<(State, AST)> = Vec::new();

        let mut next_symbol = input.next().unwrap();
        stack.push((self.starting_state,
                    AST::from((next_symbol.get_input_symbol().unwrap(),
                               position_list[next_symbol.get_input_position_index()].clone()))));

        let mut problems = Vec::new();

        loop {
            let current_state = stack.last().unwrap().0;
            match self.get(&(current_state, &next_symbol)) {
                Some(&ParserAction::Shift(next_state)) => {
                    let input_symbol = next_symbol.get_input_symbol();
                    let input_position = position_list[next_symbol.get_input_position_index()].clone();

                    let next_symbol_ast = match input_symbol {
                        Some(input_symbol) => AST::from((input_symbol, input_position)),
                        None => AST::default(),
                    };
                    stack.push((next_state, next_symbol_ast));

                    next_symbol = input.next().unwrap();
                },

                Some(&ParserAction::Reduce(ref production)) => {
                    let (nonterminal, ast) = production.reduce(&mut stack);
                    let new_state = stack.last().unwrap().0;

                    let next_state = self.get(&(new_state, &nonterminal)).unwrap();

                    if let &ParserAction::Go(state) = next_state {
                        stack.push((state, ast));
                    } else {
                        panic!("Parse error: Non go-action on a nonterminal.");
                    }
                },

                Some(&ParserAction::Accept) => {
                    if let Some((_, ast)) = stack.pop() {
                        return Ok(ast);
                    }
                    panic!("Parse error: The accept action didn't produce a syntax tree.");
                },

                None => {
                    let current_symbol = next_symbol.get_input_symbol();
                    if current_symbol.is_some() {
                        let input_position = position_list[next_symbol.get_input_position_index()]
                            .clone();
                        problems.push(Problem::new(
                                generate_problem(current_symbol,
                                                 self.transitions_from(current_state)),
                                                 input_position));
                    } else {
                        problems.push(Problem::new(
                                generate_problem(current_symbol,
                                                 self.transitions_from(current_state)),
                                                 position_list.last().unwrap().clone()));
                    }
                    return Err(problems);
                },

                _ => unreachable!(),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Associativity, Operator, Parser};
    use super::grammar::Grammar;
    use super::grammar::symbol::{IntoSymbol, Symbol};
    use file_handle::FileHandle;
    use problem_reporting::{InputPosition, Problem, ProblemDescription, ProblemSummary,
                            ProblemType};

    #[derive(PartialEq, Eq, Clone, Hash, Debug, PartialOrd, Ord)]
    enum Nonterminal {
        Program,
        Expression,
        Function
    }

    impl IntoSymbol<Nonterminal, char> for Nonterminal {
        fn into_symbol(self) -> Symbol<Nonterminal, char> {
            Symbol::Nonterminal(self)
        }
    }

    #[derive(PartialEq, Eq, Clone, Hash, Debug)]
    enum AST {
        None,
        Program(Vec<AST>),
        Function(char, Box<AST>),
        PlusExpr(Box<AST>, Box<AST>),
        TimesExpr(Box<AST>, Box<AST>),
        Num(char),
        SingleChar(char)
    }

    impl Default for AST {
        fn default() -> AST {
            AST::None
        }
    }

    impl From<char> for AST {
        fn from(character: char) -> AST {
            AST::SingleChar(character)
        }
    }

    lazy_static! {
        static ref PROBLEM: ProblemSummary = ProblemSummary::new("Short description",
                                                                 "Long description",
                                                                 ProblemType::Error);
    }

    fn generate_parser() -> Parser<Nonterminal, char, AST> {
        use self::Nonterminal::*;

        let ident = terminal_matcher!("Identifier", 'a'...'z');
        let num = terminal_matcher!("Identifier", '0'...'9');
        let open_brace = terminal_matcher!("{", '{');
        let close_brace = terminal_matcher!("}", '}');
        let open_parenthesis = terminal_matcher!("(", '(');
        let close_parenthesis = terminal_matcher!(")", ')');
        let plus = terminal_matcher!("+", '+');
        let times = terminal_matcher!("*", '*');
        let space = terminal_matcher!(" ", ' ');

        let productions = vec![production!([Program => Function],
                        |symbols| {
                            AST::Program(symbols)
                        }),
                 production!([Program => Function, space, Program],
                        |mut symbols| {
                            let mut ast = symbols.remove(2);
                            match ast {
                                AST::Program(ref mut functions) =>
                                    functions.insert(0, symbols.remove(0)),
                                _ => unreachable!()
                            }
                            ast
                        }),
                 production!([Function =>
                        ident,
                        open_parenthesis,
                        close_parenthesis,
                        open_brace,
                        Expression,
                        close_brace],
                        |mut symbols| {
                            let body = symbols.remove(4);
                            let ident = match symbols.remove(0) {
                                AST::SingleChar(character) => character,
                                _ => unreachable!()
                            };
                            AST::Function(ident, Box::new(body))
                        }),
                 production!([Expression => Expression, plus, Expression],
                        |mut symbols| {
                            let rhs = symbols.remove(2);
                            let lhs = symbols.remove(0);
                            AST::PlusExpr(Box::new(lhs), Box::new(rhs))
                        }),
                 production!([Expression => Expression, times, Expression],
                        |mut symbols| {
                            let rhs = symbols.remove(2);
                            let lhs = symbols.remove(0);
                            AST::TimesExpr(Box::new(lhs), Box::new(rhs))
                        }),
                 production!([Expression => open_parenthesis, Expression, close_parenthesis],
                        |mut symbols| {
                            symbols.remove(1)
                        }),
                 production!([Expression => num],
                        |mut symbols| {
                            let num = match symbols.remove(0) {
                                AST::SingleChar(character) => character,
                                _ => unreachable!()
                            };
                            AST::Num(num)
                        })];

        let operators = vec![Operator::new(plus, Associativity::Left, 1),
                             Operator::new(times, Associativity::Right, 0)];

        Grammar::new(Program, productions).to_parser(operators)
    }

    fn to_vec<'a>(file: &'a FileHandle) -> Vec<(InputPosition<'a>, char)> {
        file.content
            .chars()
            .enumerate()
            .map(|(index, character)| (InputPosition::new(&file, index, 1), character))
            .collect()
    }

    fn problem<'a>() -> ProblemDescription<'a> {
        ProblemDescription::new(&PROBLEM, Vec::new())
    }

    #[test]
    fn const_function() {
        use self::AST::*;

        let parser = generate_parser();

        let file = FileHandle::test_new("file_name".to_owned(), "e(){2}".to_owned());

        let result = parser.parse(to_vec(&file), Box::new(|_, _| problem()));

        assert_eq!(result, Ok(Program(vec![Function('e', Box::new(Num('2')))])));
    }

    #[test]
    #[should_panic]
    fn const_function_fails() {
        use self::AST::*;

        let parser = generate_parser();

        let file = FileHandle::test_new("file_name".to_owned(), "e(){2}".to_owned());

        let result = parser.parse(to_vec(&file), Box::new(|_, _| problem()));

        assert_eq!(result, Ok(Program(vec![Function('p', Box::new(Num('3')))])));
    }

    #[test]
    fn error() {
        let parser = generate_parser();

        let file = FileHandle::test_new("file_name".to_owned(), "e-(){2}".to_owned());

        let result = parser.parse(to_vec(&file), Box::new(|_, _| problem()));

        assert_eq!(result,
                   Err(vec![Problem::new(problem(), InputPosition::new(&file, 1, 1))]));
    }

    #[test]
    fn precedence() {
        use self::AST::*;

        let parser = generate_parser();

        // multiplication second
        let file = FileHandle::test_new("file_name".to_owned(), "e(){1+2*5}".to_owned());

        let result = parser.parse(to_vec(&file), Box::new(|_, _| problem()));

        assert_eq!(result,
                   Ok(Program(vec![Function('e',
                                                Box::new(PlusExpr(
                                                    Box::new(Num('1')),
                                                    Box::new(TimesExpr(Box::new(Num('2')),
                                                        Box::new(Num('5'))))
                                                )))])));

        // multiplication first
        let file = FileHandle::test_new("file_name".to_owned(), "e(){1*2+5}".to_owned());

        let result = parser.parse(to_vec(&file), Box::new(|_, _| problem()));
        assert_eq!(result,
                   Ok(Program(vec![Function('e',
                                                Box::new(PlusExpr(
                                                    Box::new(TimesExpr(Box::new(Num('1')),
                                                        Box::new(Num('2')))),
                                                    Box::new(Num('5'))
                                                )))])));
    }

    #[test]
    fn associativity() {
        use self::AST::*;

        let parser = generate_parser();

        // addition is left associative in this grammer
        let file = FileHandle::test_new("file_name".to_owned(), "e(){1+2+5}".to_owned());

        let result = parser.parse(to_vec(&file), Box::new(|_, _| problem()));

        assert_eq!(result,
                   Ok(Program(vec![Function('e',
                                                Box::new(PlusExpr(
                                                    Box::new(PlusExpr(Box::new(Num('1')),
                                                        Box::new(Num('2')))),
                                                    Box::new(Num('5'))
                                                )))])));

        // multiplication is right associative in this grammer
        let file = FileHandle::test_new("file_name".to_owned(), "e(){1*2*5}".to_owned());

        let result = parser.parse(to_vec(&file), Box::new(|_, _| problem()));
        assert_eq!(result,
                   Ok(Program(vec![Function('e',
                                                Box::new(TimesExpr(
                                                    Box::new(Num('1')),
                                                    Box::new(TimesExpr(Box::new(Num('2')),
                                                        Box::new(Num('5'))))
                                                )))])));
    }

    #[test]
    fn multiple_functions() {
        use self::AST::*;

        let parser = generate_parser();

        let file = FileHandle::test_new("file_name".to_owned(), "p(){3} d(){5} f(){4}".to_owned());

        let result = parser.parse(to_vec(&file), Box::new(|_, _| problem()));

        assert_eq!(result,
                   Ok(Program(vec![Function('p', Box::new(Num('3'))),
                                   Function('d', Box::new(Num('5'))),
                                   Function('f', Box::new(Num('4')))])));
    }
}
