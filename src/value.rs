//! Utilities for dealing with Lox values.

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
        assert_eq!(value_to_string(&Value::new(f64::NAN)), "NaN");
        assert_eq!(value_to_string(&Value::new(-0.0 / 0.0)), "NaN");
    }

    #[test]
    fn value_operations_work() {
        assert_eq!(Value::new(2.3).add(Value::new(7.2)), Value::new(9.5));
        assert_eq!(Value::new(2.3).subtract(Value::new(7.1)), Value::new(-4.8));
        assert_eq!(Value::new(2.5).multiply(Value::new(3.0)), Value::new(7.5));
        assert_eq!(Value::new(20.8).divide(Value::new(-2.0)), Value::new(-10.4));
        assert_eq!(Value::new(1839.1932).negate(), Value::new(-1839.1932));
    }
}
