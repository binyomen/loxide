//! Utilities for dealing with Lox values.

#[cfg(test)]
mod tests;

/// A Lox value as represented in the interpreter. For example, this can
/// represent a number, a string, a class instance, etc.
#[derive(Clone, Debug, PartialEq)]
pub struct Value(f64);

impl Value {
    pub fn new(f: f64) -> Self {
        Value(f)
    }

    pub fn add(&self, other: Value) -> Value {
        Value::new(self.0 + other.0)
    }

    pub fn subtract(&self, other: Value) -> Value {
        Value::new(self.0 - other.0)
    }

    pub fn multiply(&self, other: Value) -> Value {
        Value::new(self.0 * other.0)
    }

    pub fn divide(&self, other: Value) -> Value {
        Value::new(self.0 / other.0)
    }

    pub fn negate(&self) -> Value {
        Value::new(-self.0)
    }
}

pub fn value_to_string(value: &Value) -> String {
    value.0.to_string()
}
