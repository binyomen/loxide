//! Utilities for dealing with Lox values.

/// A Lox value as represented in the interpreter. For example, this can
/// represent a number, a string, a class instance, etc.
#[derive(Clone, Debug, PartialEq)]
pub struct Value(f64);

impl Value {
    pub fn new(f: f64) -> Self {
        Value(f)
    }

    pub fn negate(&self) -> Value {
        Value::new(-self.0)
    }
}

pub fn value_to_string(value: &Value) -> String {
    value.0.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn value_to_string_works() {
        assert_eq!(value_to_string(&Value::new(0.0)), "0");
        assert_eq!(value_to_string(&Value::new(-0.0)), "-0");
        assert_eq!(value_to_string(&Value::new(1.0)), "1");
        assert_eq!(value_to_string(&Value::new(-1.0)), "-1");
        assert_eq!(
            value_to_string(&Value::new(283928.0283093)),
            "283928.0283093"
        );
        assert_eq!(
            value_to_string(&Value::new(-283928.0283093)),
            "-283928.0283093"
        );

        assert_eq!(value_to_string(&Value::new(1.0 / 0.0)), "inf");
        assert_eq!(value_to_string(&Value::new(-1.0 / 0.0)), "-inf");
        assert_eq!(value_to_string(&Value::new(0.0 / 0.0)), "NaN");
        assert_eq!(value_to_string(&Value::new(-0.0 / 0.0)), "NaN");
    }
}
