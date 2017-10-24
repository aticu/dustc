#[macro_use]
extern crate lazy_static;
extern crate char_iter;

#[macro_use]
pub mod lexer;
#[macro_use]
pub mod parser;
mod automata;
mod problem_reporting;
mod language;
mod file_handle;

use file_handle::FileHandle;
use problem_reporting::report;

/// Runs the compiler.
pub fn run() {
    let files_to_parse = vec!["test.qs"];
    let lexer = &language::lexer::LEXER;
    let mut tokens = Vec::new();

    for file_name in files_to_parse {
        let file = FileHandle::new(file_name.to_owned()).expect("File couldn't be processed");

        match lexer.run(&file) {
            Ok(result) => tokens.extend(result),
            Err(errors) => report(&errors),
        }
    }

    println!("{:?}", tokens);

    let parser = &language::parser::PARSER;

    println!("{:#?}", parser.parse(tokens).unwrap());
}
