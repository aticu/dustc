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
use std::sync::Arc;

/// Runs the compiler.
pub fn run() {
    let files_to_parse = vec!["test.qm"];

    for file_name in files_to_parse {
        let file = FileHandle::new(file_name.to_owned()).expect("File couldn't be processed");
        let file = Arc::new(file);

        let file_result = language::handle_file(&file);

        match file_result {
            Ok(_) => (),
            Err(problems) => report(&problems),
        }
    }

}
