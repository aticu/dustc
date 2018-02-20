//! This module is supposed to handle reporting of problems during compilation.

use file_handle::FileHandle;
use std::sync::Arc;
use std::sync::Mutex;
use std::cmp::{max, min};

/// Resets the visual appearance of text in a terminal.
const RESET: &str = "\x1B[0m";

/// Sets the text to bold text.
const BOLD: &str = "\x1B[1m";

/// Sets the text color to red.
const RED: &str = "\x1B[31m";

/// Sets the text color to yellow.
const YELLOW: &str = "\x1B[33m";

/// Sets the text color to blue.
const BLUE: &str = "\x1B[34m";

/// This struct references some input data to the compiler.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct InputPosition {
    /// The file the referenced data is located in.
    pub file: Arc<FileHandle>,
    /// The index of the first character of the referenced data.
    pub index: usize,
    /// The length of the referenced data.
    pub length: usize
}

impl InputPosition {
    /// Creates a new input position reference.
    pub fn new(file: &Arc<FileHandle>, index: usize, length: usize) -> InputPosition {
        InputPosition {
            file: file.clone(),
            index,
            length
        }
    }

    /// Creates a new input position starting at `start` and ending after `end`.
    ///
    /// # Panics
    /// This function will panic if
    /// - the two files don't match up.
    pub fn merge(start: &InputPosition, end: &InputPosition) -> InputPosition {
        assert_eq!(start.file, end.file);

        let first_index = min(start.index, end.index);
        let last_index = max(start.index + start.length, end.index + end.length);

        InputPosition {
            file: start.file.clone(),
            index: first_index,
            length: last_index - first_index
        }
    }
}

/// This trait defines that an object is locatable in the input to the compiler.
///
/// That means that it is possible to find the corresponding `InputPosition`.
pub trait Locatable {
    /// This function returns the position of the `Locatable`-item in the input.
    fn get_input_position(&self) -> &InputPosition;
}

/// Represents the different types of problems.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProblemType {
    /// The problem is a warning.
    Warning,
    /// The problem is an error.
    ///
    /// Compilation cannot be continued.
    Error
}

/// Represents a possible problem during compilation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ProblemSummary {
    /// The problem number.
    number: u32,
    /// A short summary of the problem.
    short: &'static str,
    /// A long problem description.
    ///
    /// It should also include indications on how to fix it.
    long: &'static str,
    /// The type of problem.
    problem_type: ProblemType
}

lazy_static! {
    /// A counter to give the problems the numbers of their creation order.
    static ref CURRENT_PROBLEM_NUM: Mutex<u32> = Mutex::new(0);
}

impl ProblemSummary {
    /// Creates a new problem description.
    pub fn new(short: &'static str,
               long: &'static str,
               problem_type: ProblemType)
               -> ProblemSummary {
        let mut num = CURRENT_PROBLEM_NUM
            .lock()
            .expect("The problem description creation mutex is corrupted");

        *num += 1;

        ProblemSummary {
            number: *num - 1,
            short,
            long,
            problem_type
        }
    }
}

/// Represents some additional information about a problem.
#[derive(Debug, PartialEq, Eq)]
pub enum ProblemInformation {
    /// The additional content of the error message.
    Content(String)
}

/// Describes a concrete problem during compilation.
#[derive(Debug, PartialEq, Eq)]
pub struct ProblemDescription {
    /// The generic summary of the problem.
    summary: Arc<ProblemSummary>,
    /// Additional information to further specify the problem.
    additional_information: Vec<ProblemInformation>
}

impl ProblemDescription {
    /// Creates a new problem description.
    pub fn new(summary: &Arc<ProblemSummary>,
               additional_information: Vec<ProblemInformation>)
               -> ProblemDescription {
        ProblemDescription {
            summary: summary.clone(),
            additional_information
        }
    }
}

/// Represents an actual problem during compilation.
#[derive(Debug, PartialEq, Eq)]
pub struct Problem {
    /// The description of the problem.
    description: ProblemDescription,
    /// The position the problem occured at.
    position: InputPosition
}

impl Problem {
    /// Creates a new problem with the given description and position.
    pub fn new(description: ProblemDescription, position: InputPosition) -> Problem {
        Problem {
            description,
            position
        }
    }
}

/// Reports the given problems.
pub fn report(problems: &Vec<Problem>) {
    for problem in problems {
        report_problem(problem)
    }
}

/// Reports the given problem.
fn report_problem(problem: &Problem) {
    let mut iterator = problem.position.file.content.chars();

    enum State {
        CR,
        NotCR
    }
    let mut state = State::NotCR;

    let mut line_number = 1;

    let mut current_line = String::new();

    let mut line_start_index = 0;

    // First calculate the line number and position within the line.
    {
        for current_index in 0..problem.position.index {
            if let Some(character) = iterator.next() {
                match character {
                    '\n' => {
                        line_start_index = current_index + 1;
                        line_number += 1;
                        current_line.clear();
                        state = State::NotCR;
                    },
                    '\r' => {
                        match state {
                            State::CR => {
                                line_start_index = current_index + 1;
                                line_number += 1;
                                current_line.clear();
                            },
                            _ => (),
                        }
                        state = State::CR;
                    },
                    _ => {
                        current_line.push(character);
                        state = State::NotCR;
                    },
                }
            }
        }
    }

    current_line.extend(iterator.take_while(|character| character != &'\r' && character != &'\n'));

    let character_index = problem.position.index - line_start_index;

    // Extract some of the additional information.
    let content = problem
        .description
        .additional_information
        .iter()
        .find(|additional_information| match *additional_information {
                  &ProblemInformation::Content(_) => true,
                  _ => false,
              });

    // The offset from which the line will be printed.
    let line_offset = format!("{}", line_number).len();

    let offset = || for _ in 0..line_offset {
        eprint!(" ");
    };

    // Print the problem summary.
    let (problem_color, problem_text, problem_initial) =
        match problem.description.summary.problem_type {
            ProblemType::Error => (RED, "error", "E"),
            ProblemType::Warning => (YELLOW, "warning", "W"),
        };

    eprint!("{}{}{} [{}{:04}]{}",
            problem_color,
            BOLD,
            problem_text,
            problem_initial,
            problem.description.summary.number,
            RESET);
    match content {
        Some(&ProblemInformation::Content(ref content)) => {
            eprintln!("{}: {} {}{}",
                      BOLD,
                      problem.description.summary.short,
                      content,
                      RESET)
        },
        _ => eprintln!("{}: {}{}", BOLD, problem.description.summary.short, RESET),
    }

    // Print the position information.
    offset();
    eprintln!("{}{}-->{} {}:{}:{}",
              BLUE,
              BOLD,
              RESET,
              problem.position.file.path,
              line_number,
              character_index + 1);
    offset();
    eprintln!("{}{} |", BLUE, BOLD);
    eprintln!("{} | {}{}", line_number, RESET, current_line);
    offset();
    eprint!("{}{} | {}", BLUE, BOLD, RESET);


    // Print the line and underline the relevant information.
    for _ in 0..character_index {
        eprint!(" ");
    }
    eprint!("{}{}", BOLD, problem_color);
    for _ in 0..problem.position.length {
        eprint!("^");
    }
    eprintln!("{}", RESET);
}
