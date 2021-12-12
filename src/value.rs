//! Utilities for dealing with Lox values.

#[cfg(test)]
mod tests;

/// Validates that the operands to the [`Value`] operation are of the correct
/// type. If they are of the correct type, returns the inner value(s) of the
/// [`Value`] enum. Otherwise, returns an error.
macro_rules! check_operand_types {
    ($operand:expr, $match_type:ident, $type_name:literal) => {
        match $operand {
            Value::$match_type(v) => Ok(v),
            _ => Err(format!(
                "Operand '{}' must be a {}.",
                value_to_string($operand),
                $type_name
            )),
        }?
    };
    ($operand1:expr, $operand2:expr, $match_type1:ident, $match_type2:ident, $type_name:literal) => {
        match ($operand1, $operand2) {
            (Value::$match_type1(v1), Value::$match_type2(v2)) => Ok((v1, v2)),
            (_, Value::$match_type2(_)) => Err(format!(
                "Operand '{}' must be a {}.",
                value_to_string($operand1),
                $type_name
            )),
            (Value::$match_type1(_), _) => Err(format!(
                "Operand '{}' must be a {}.",
                value_to_string($operand2),
                $type_name
            )),
            (_, _) => Err(format!(
                "Operand '{}' must be a {}.",
                value_to_string($operand1),
                $type_name
            )),
        }?
    };
}

/// A Lox value as represented in the interpreter. For example, this can
/// represent a number, a string, a class instance, etc.
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    Number(f64),
    Bool(bool),
}

impl Value {
    pub fn add(&self, other: &Value) -> Result<Value, String> {
        let (n1, n2) = check_operand_types!(self, other, Number, Number, "number");
        Ok(Value::Number(n1 + n2))
    }

    pub fn subtract(&self, other: &Value) -> Result<Value, String> {
        let (n1, n2) = check_operand_types!(self, other, Number, Number, "number");
        Ok(Value::Number(n1 - n2))
    }

    pub fn multiply(&self, other: &Value) -> Result<Value, String> {
        let (n1, n2) = check_operand_types!(self, other, Number, Number, "number");
        Ok(Value::Number(n1 * n2))
    }

    pub fn divide(&self, other: &Value) -> Result<Value, String> {
        let (n1, n2) = check_operand_types!(self, other, Number, Number, "number");
        Ok(Value::Number(n1 / n2))
    }

    pub fn negate(&self) -> Result<Value, String> {
        let n = check_operand_types!(self, Number, "number");
        Ok(Value::Number(-n))
    }
}

pub fn value_to_string(value: &Value) -> String {
    match value {
        Value::Nil => "nil".to_owned(),
        Value::Number(n) => n.to_string(),
        Value::Bool(b) => (if *b { "true" } else { "false" }).to_owned(),
    }
}
