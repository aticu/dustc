//! This module defines the syntactic category of types in the target language.

use problem_reporting::{InputPosition, Locatable};

/// This enum represents an expression in the AST.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    /// Represents a primitive type.
    PrimitiveType(String, InputPosition)
}

impl Locatable for Type {
    fn get_input_position(&self) -> &InputPosition {
        match self {
            &Type::PrimitiveType(_, ref position) => position
        }
    }
}
