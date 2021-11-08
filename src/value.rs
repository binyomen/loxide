//! Utilities for dealing with Lox values.

/// A Lox value as represented in the interpreter. For example, this can
/// represent a number, a string, a class instance, etc.
#[derive(Debug, PartialEq)]
pub struct Value(f64);

impl Value {
    pub fn new(f: f64) -> Self {
        Value(f)
    }
}

pub fn value_to_string(value: &Value) -> String {
    value.0.to_string()
}
