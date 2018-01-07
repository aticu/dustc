//! This module is supposed to define the language with all its parts.

mod problems;
mod token;
mod lexer;
mod parser;
mod nonterminal;
mod ast;
mod expression;
mod statement;
mod types;

use self::lexer::LEXER;
use self::parser::PARSER;
use self::problems::{PROBLEMS, UNEXPECTED_TOKEN, UNKNOWN_TOKEN, UNMATCHED_STRING};
use file_handle::FileHandle;
use problem_reporting::{Problem, ProblemDescription, ProblemInformation};
use std::sync::Arc;

pub fn handle_file(file: &Arc<FileHandle>) -> Result<(), Vec<Problem>> {
    let tokens = LEXER
        .run(&file,
             |problematic_character| if problematic_character == '"' {
                 problem(UNMATCHED_STRING)
             } else {
                 problem(UNKNOWN_TOKEN)
             })?;

    let ast = PARSER
        .parse(tokens,
               Box::new(|token, possible_followers| match token {
                            Some(token) => {
            assert!(possible_followers.len() > 0);
            let follower_strings: Vec<String> = possible_followers
                .iter()
                .map(|follower| format!("{}", follower))
                .filter(|string| string != "")
                .collect();

            let message = match follower_strings.len() {
                1 => format!("{}, found {}", possible_followers[0], token),
                num @ 2...10 => {
                    let mut string = format!("{}", follower_strings[0]);

                    for i in 1..num - 1 {
                        string.push_str(&format!(", {}", follower_strings[i]));
                    }

                    string.push_str(&format!(" or {}", follower_strings[num - 1]));
                    string.push_str(&format!(", found {}", token));
                    string
                },
                num => format!("one of {} possible tokens, found {}", num, token),
            };

            ProblemDescription::new(&PROBLEMS[UNEXPECTED_TOKEN],
                                    vec![ProblemInformation::Content(message)])
        },
                            None => unimplemented!("Unexpected end of file during parsing error"),
                        }))?;

    println!("{:#?}", ast);

    Ok(())
}

fn problem(number: usize) -> ProblemDescription {
    ProblemDescription::new(&PROBLEMS[number], Vec::new())
}
