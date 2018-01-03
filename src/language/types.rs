//! This module defines the syntactic category of types in the target language.

/// This enum represents an expression in the AST.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    /// Represents a primitive type.
    PrimitiveType(String)
}
