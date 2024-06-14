use super::super::*;
use super::*;

#[derive(Debug)]
struct CommaReductionTestCase {
    input: Vec<PartialCommaOnly>,
    expected_output: Option<Expression>,
}

#[cfg(test)]
fn test_comma_reduction(test_data: CommaReductionTestCase) {
    let input_repr = format!("{:?}", &test_data.input);
    let vdq = make_vdq(test_data.input);
    match (reduce_comma_ops(vdq), test_data.expected_output) {
        (Ok(got), Some(expected)) => {
            let got_repr = format!("{got:?}");
            let expected_repr = format!("{expected:?}");
            if got_repr != expected_repr {
                panic!("for input {input_repr}, expected output {expected_repr} but got output {got_repr}");
            }
        }
        (Err(_), None) => (),
        (Err(e), Some(expected)) => {
            panic!(
                "expected result {expected:?} for input {input_repr} but got error instead: {e}"
            );
        }
        (Ok(got), None) => {
            panic!("expected an error for input {input_repr} but got result instead: {got:?}");
        }
    }
}

#[test]
fn empty() {
    // Verify the parsing of an empty expression.
    test_comma_reduction(CommaReductionTestCase {
        input: vec![],
        expected_output: Some(make_default_print()),
    });
}

#[test]
fn just_print() {
    // Verify the parsing of an expression containing just -print.
    test_comma_reduction(CommaReductionTestCase {
        input: vec![PartialCommaOnly::Expr(make_default_print())],
        expected_output: Some(make_default_print()),
    });
}

#[test]
fn double_print() {
    // Verify the parsing of -print , -print.
    test_comma_reduction(CommaReductionTestCase {
        input: vec![
            PartialCommaOnly::Expr(make_default_print()),
            PartialCommaOnly::Comma,
            PartialCommaOnly::Expr(make_default_print()),
        ],
        expected_output: Some(Expression::BinaryOp(BinaryOperation::new(
            BinaryOperationKind::Comma,
            vec![make_default_print(), make_default_print()],
        ))),
    });
}

#[test]
fn leading_comma() {
    // Verify that a leading comma is an error.,
    test_comma_reduction(CommaReductionTestCase {
        input: vec![
            PartialCommaOnly::Comma,
            PartialCommaOnly::Expr(make_default_print()),
        ],
        expected_output: None,
    });
}

#[test]
fn trailing_comma() {
    // Verify that a trailing comma is an error.,
    test_comma_reduction(CommaReductionTestCase {
        input: vec![
            PartialCommaOnly::Expr(make_default_print()),
            PartialCommaOnly::Comma,
        ],
        expected_output: None,
    });
}

#[test]
fn lonely_comma() {
    // Verify that a lonely comma is an error.,
    test_comma_reduction(CommaReductionTestCase {
        input: vec![
            PartialCommaOnly::Expr(make_default_print()),
            PartialCommaOnly::Comma,
        ],
        expected_output: None,
    });
}

fn double_comma() {
    // Verify that a double comma is an error.
    test_comma_reduction(CommaReductionTestCase {
        input: vec![
            PartialCommaOnly::Expr(make_default_print()),
            PartialCommaOnly::Comma,
            PartialCommaOnly::Comma,
            PartialCommaOnly::Expr(make_default_print()),
        ],
        expected_output: None,
    });
}
