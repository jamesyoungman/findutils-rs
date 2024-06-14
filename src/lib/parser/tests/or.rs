// Tests for parsing -or
use super::super::*;
use super::*;

#[derive(Debug)]
struct OrReductionTestCase {
    input: Vec<PartialOrCommaOnly>,
    expected_output: Option<Vec<PartialCommaOnly>>,
}

type TestCase = OrReductionTestCase;

#[cfg(test)]
fn test_or_reduction(test_data: OrReductionTestCase) {
    let input_repr = format!("{:?}", &test_data.input);
    let vdq = make_vdq(test_data.input);
    match (reduce_or_ops(vdq), test_data.expected_output) {
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
    test_or_reduction(OrReductionTestCase {
        input: vec![],
        expected_output: Some(vec![]),
    });
}

#[test]
fn just_print() {
    // Verify the parsing of an expression containing just -print.
    test_or_reduction(TestCase {
        input: vec![PartialOrCommaOnly::Expr(make_default_print())],
        expected_output: Some(vec![PartialCommaOnly::Expr(make_default_print())]),
    });
}

#[test]
fn double_print_with_or() {
    // Verify the parsing of -print -or -print.
    test_or_reduction(TestCase {
        input: vec![
            PartialOrCommaOnly::Expr(make_default_print()),
            PartialOrCommaOnly::Or,
            PartialOrCommaOnly::Expr(make_default_print()),
        ],
        expected_output: Some(vec![PartialCommaOnly::Expr(Expression::BinaryOp(
            BinaryOperation::new(
                BinaryOperationKind::Or,
                vec![make_default_print(), make_default_print()],
            ),
        ))]),
    });
}

#[test]
fn double_print_with_comma() {
    // Verify the parsing of -print , -print.
    test_or_reduction(TestCase {
        input: vec![
            PartialOrCommaOnly::Expr(make_default_print()),
            PartialOrCommaOnly::Comma,
            PartialOrCommaOnly::Expr(make_default_print()),
        ],
        expected_output: Some(vec![
            PartialCommaOnly::Expr(make_default_print()),
            PartialCommaOnly::Comma,
            PartialCommaOnly::Expr(make_default_print()),
        ]),
    });
}

#[test]
fn leading_or() {
    // Verify that a leading -or is an error.,
    test_or_reduction(TestCase {
        input: vec![
            PartialOrCommaOnly::Or,
            PartialOrCommaOnly::Expr(make_default_print()),
        ],
        expected_output: None,
    });
}

#[test]
fn trailing_or() {
    // Verify that a trailing -or is an error.,
    test_or_reduction(TestCase {
        input: vec![
            PartialOrCommaOnly::Expr(make_default_print()),
            PartialOrCommaOnly::Or,
        ],
        expected_output: None,
    });
}

#[test]
fn lonely_or() {
    // Verify that a -or by itself is an error.,
    test_or_reduction(TestCase {
        input: vec![PartialOrCommaOnly::Or],
        expected_output: None,
    });
}

#[test]
fn double_or() {
    // Verify that a -print -or -or -print is an error.,
    test_or_reduction(TestCase {
        input: vec![
            PartialOrCommaOnly::Expr(make_default_print()),
            PartialOrCommaOnly::Or,
            PartialOrCommaOnly::Or,
            PartialOrCommaOnly::Expr(make_default_print()),
        ],
        expected_output: None,
    });
}

#[test]
fn or_comma() {
    // Verify that a -print -or , -print is an error.,
    test_or_reduction(TestCase {
        input: vec![
            PartialOrCommaOnly::Expr(make_default_print()),
            PartialOrCommaOnly::Or,
            PartialOrCommaOnly::Comma,
            PartialOrCommaOnly::Expr(make_default_print()),
        ],
        expected_output: None,
    });
}

#[test]
fn comma_or() {
    // Verify that a -print , -or -print is an error.,
    test_or_reduction(TestCase {
        input: vec![
            PartialOrCommaOnly::Expr(make_default_print()),
            PartialOrCommaOnly::Comma,
            PartialOrCommaOnly::Or,
            PartialOrCommaOnly::Expr(make_default_print()),
        ],
        expected_output: None,
    });
}
