use super::*;

fn vni() -> Value {
    Value::Nil
}

fn vn(n: f64) -> Value {
    Value::Number(n)
}

fn vb(b: bool) -> Value {
    Value::Bool(b)
}

#[test]
fn value_to_string_works() {
    assert_eq!(value_to_string(&vni()), "nil");

    assert_eq!(value_to_string(&vn(0.0)), "0");
    assert_eq!(value_to_string(&vn(-0.0)), "-0");
    assert_eq!(value_to_string(&vn(1.0)), "1");
    assert_eq!(value_to_string(&vn(-1.0)), "-1");
    assert_eq!(value_to_string(&vn(283928.0283093)), "283928.0283093");
    assert_eq!(value_to_string(&vn(-283928.0283093)), "-283928.0283093");

    assert_eq!(value_to_string(&vn(1.0 / 0.0)), "inf");
    assert_eq!(value_to_string(&vn(-1.0 / 0.0)), "-inf");
    assert_eq!(value_to_string(&vn(f64::NAN)), "NaN");
    assert_eq!(value_to_string(&vn(-0.0 / 0.0)), "NaN");

    assert_eq!(value_to_string(&vb(true)), "true");
    assert_eq!(value_to_string(&vb(false)), "false");
}

#[test]
fn number_operations() {
    assert_eq!(vn(2.3).add(&vn(7.2)).unwrap(), vn(9.5));
    assert_eq!(vn(2.3).subtract(&vn(7.1)).unwrap(), vn(-4.8));
    assert_eq!(vn(2.5).multiply(&vn(3.0)).unwrap(), vn(7.5));
    assert_eq!(vn(20.8).divide(&vn(-2.0)).unwrap(), vn(-10.4));
    assert_eq!(vn(1839.1932).negate().unwrap(), vn(-1839.1932));

    assert_eq!(vn(1.0).divide(&vn(0.0)).unwrap(), vn(f64::INFINITY));
    assert_eq!(vn(-1.0).divide(&vn(0.0)).unwrap(), vn(f64::NEG_INFINITY));

    // This is necessary since assigning to a variable and checking that the
    // variable doesn't equal itself fires clippy::eq_op, and an allow
    // attribute couldn't be put on the assert_ne invocation.
    fn divide_0_by_0() -> Value {
        vn(0.0).divide(&vn(0.0)).unwrap()
    }
    assert_ne!(divide_0_by_0(), divide_0_by_0());
}

#[test]
fn bool_operations() {
    assert_eq!(vb(true).not().unwrap(), vb(false));
    assert_eq!(vb(false).not().unwrap(), vb(true));
    assert_eq!(vni().not().unwrap(), vb(true));
    assert_eq!(vn(0.0).not().unwrap(), vb(false));
}

#[test]
fn number_operation_type_errors() {
    for op in [Value::add, Value::subtract, Value::multiply, Value::divide] {
        assert_eq!(
            op(&vni(), &vni()).err().unwrap(),
            "Operand 'nil' must be a number."
        );
        assert_eq!(
            op(&vni(), &vn(0.0)).err().unwrap(),
            "Operand 'nil' must be a number."
        );
        assert_eq!(
            op(&vn(0.0), &vni()).err().unwrap(),
            "Operand 'nil' must be a number."
        );

        assert_eq!(
            op(&vb(false), &vb(false)).err().unwrap(),
            "Operand 'false' must be a number."
        );
        assert_eq!(
            op(&vb(false), &vn(0.0)).err().unwrap(),
            "Operand 'false' must be a number."
        );
        assert_eq!(
            op(&vn(0.0), &vb(false)).err().unwrap(),
            "Operand 'false' must be a number."
        );

        assert_eq!(
            op(&vni(), &vb(false)).err().unwrap(),
            "Operand 'nil' must be a number."
        );
        assert_eq!(
            op(&vb(false), &vni()).err().unwrap(),
            "Operand 'false' must be a number."
        );
    }

    assert_eq!(
        vni().negate().err().unwrap(),
        "Operand 'nil' must be a number."
    );
    assert_eq!(
        vb(false).negate().err().unwrap(),
        "Operand 'false' must be a number."
    );
}
