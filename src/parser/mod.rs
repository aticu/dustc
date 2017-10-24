//! This module is supposed to parse the given token string.
//!
//! The input token string should be converted into an abstract syntax tree
//! (AST) which can later be used for further processing.

#[macro_use]
pub mod grammar;

use self::grammar::Production;
use self::grammar::symbol::{Symbol, TerminalMatcher};
use automata::State;
use automata::dfa::DFA;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

/// Corresponds to an action a parser can take upon reading a symbol.
#[derive(Debug, PartialEq, Eq)]
enum ParserAction<Nonterminal, Terminal, AST>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash,
          AST: Clone + Debug + Eq + Default + From<Terminal>
{
    /// The shift action just pushes the next input symbol onto the stack.
    Shift,
    /// The go action pushes a nonterminal onto the stack.
    Go,
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

/// Represents a parsing table table.
struct ParsingTable<Nonterminal, Terminal, AST>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash,
          AST: Clone + Debug + Eq + Default + From<Terminal>
{
    /// The internally used table.
    table:
        Vec<((State, Symbol<Nonterminal, Terminal>), ParserAction<Nonterminal, Terminal, AST>)>
}

impl<Nonterminal, Terminal, AST> ParsingTable<Nonterminal, Terminal, AST>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash,
          AST: Clone + Debug + Eq + Default + From<Terminal>
{
    /// Creates a new parsing table from a ´HashMap´.
    fn new(table: HashMap<(State, Symbol<Nonterminal, Terminal>),
                          ParserAction<Nonterminal, Terminal, AST>>)
           -> ParsingTable<Nonterminal, Terminal, AST> {
        let mut new_table = Vec::new();

        for (from, to) in table {
            new_table.push((from, to));
        }

        ParsingTable { table: new_table }
    }

    /// Returns the entry in the parsing table, if it exists.
    fn get(&self,
           key: &(State, &Symbol<Nonterminal, Terminal>))
           -> Option<&ParserAction<Nonterminal, Terminal, AST>> {
        for &((state, ref symbol), ref action) in &self.table {
            if &(state, symbol) == key {
                return Some(&action);
            }
        }

        None
    }
}

/// Represents an SLR parser.
pub struct Parser<Nonterminal, Terminal, AST>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash,
          AST: Clone + Debug + Eq + Default + From<Terminal>
{
    /// The parsing table of the parser.
    table: ParsingTable<Nonterminal, Terminal, AST>,
    /// The DFA of the parser.
    dfa: DFA<Production<Nonterminal, Terminal, AST>, Symbol<Nonterminal, Terminal>>
}

impl<Nonterminal, Terminal, AST> Parser<Nonterminal, Terminal, AST>
    where Nonterminal: Clone + Debug + Eq + Hash,
          Terminal: Clone + Debug + Eq + Hash,
          AST: Clone + Debug + Eq + Default + From<Terminal>
{
    /// Creates a new parser using the given parts.
    fn new(table: ParsingTable<Nonterminal, Terminal, AST>,
           dfa: DFA<Production<Nonterminal, Terminal, AST>, Symbol<Nonterminal, Terminal>>)
           -> Parser<Nonterminal, Terminal, AST> {
        Parser { table, dfa }
    }

    /// Parses the given input, producing an abstract syntax tree.
    pub fn parse(&self, input_stream: Vec<Terminal>) -> Result<AST, String> {
        // First transform the input and add the EndOfInput symbol.
        let mut input: Vec<Symbol<Nonterminal, Terminal>> = input_stream
            .into_iter()
            .map(|symbol| Symbol::InputSymbol(symbol))
            .collect();
        input.push(Symbol::EndOfInput);
        let mut input = input.into_iter();

        // Keep the current DFA state on a stack.
        let mut stack: Vec<(State, AST)> = Vec::new();

        let mut next_symbol = input.next().unwrap();
        stack.push((self.dfa.start(), From::from(next_symbol.get_input_symbol())));

        loop {
            let current_state = stack.last().unwrap().0;
            match self.table.get(&(current_state, &next_symbol)) {
                Some(&ParserAction::Shift) => {
                    let next_symbol_ast = From::from(next_symbol.get_input_symbol());
                    stack.push((self.dfa.transition(current_state, next_symbol).unwrap(),
                                next_symbol_ast));
                    next_symbol = input.next().unwrap();
                },

                Some(&ParserAction::Reduce(ref production)) => {
                    let (nonterminal, ast) = production.reduce(&mut stack);
                    let new_state = stack.last().unwrap().0;

                    assert_eq!(self.table.get(&(new_state, &nonterminal)).unwrap(),
                               &ParserAction::Go);

                    stack.push((self.dfa.transition(new_state, nonterminal).unwrap(), ast));
                },

                Some(&ParserAction::Accept) => {
                    if let Some((_, ast)) = stack.pop() {
                        return Ok(ast);
                    }
                    panic!("Accept didn't produce a syntax tree during parsing.");
                },

                None => {
                    return Err(format!("Unexpected symbol: {:?}", next_symbol));
                },

                _ => unreachable!(),
            }
        }
    }
}
