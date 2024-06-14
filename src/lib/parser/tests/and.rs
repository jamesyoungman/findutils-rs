// Tests for parsing -and
//
// We don't verify that a leading -or is rejected, since
// that will happen when we reduce the or operations anyway,
// so this check doesn't need to be made in reduce_and_ops.
use super::super::*;
use super::*;

use BinaryOperationKind::*;
use PartialBinaryOpOnly::*;

#[derive(Debug)]
struct AndReductionTestCase {
    input: Vec<PartialBinaryOpOnly>,
    expected_output: Option<Vec<PartialOrCommaOnly>>,
}

type TestCase = AndReductionTestCase;

#[cfg(test)]
fn test_and_reduction(test_data: TestCase) {
    let input_repr = format!("{:?}", &test_data.input);
    let vdq = make_vdq(test_data.input);
    match (reduce_and_ops(vdq), test_data.expected_output) {
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
    test_and_reduction(TestCase {
        input: vec![],
        expected_output: Some(vec![]),
    });
}

#[test]
fn just_print() {
    // Verify the parsing of an expression containing just -print.
    test_and_reduction(TestCase {
        input: vec![Expr(make_default_print())],
        expected_output: Some(vec![PartialOrCommaOnly::Expr(make_default_print())]),
    });
}

#[test]
fn print_and_print() {
    // Verify the parsing of -print -and -print.
    test_and_reduction(TestCase {
        input: vec![
            Expr(make_default_print()),
            Op(BinaryOperationKind::And),
            Expr(make_default_print()),
        ],
        expected_output: Some(vec![PartialOrCommaOnly::Expr(Expression::BinaryOp(
            BinaryOperation::new(
                BinaryOperationKind::And,
                vec![make_default_print(), make_default_print()],
            ),
        ))]),
    });
}

#[test]
fn double_print_with_comma() {
    // Verify the parsing of -print , -print.
    test_and_reduction(TestCase {
        input: vec![
            Expr(make_default_print()),
            Op(Comma),
            Expr(make_default_print()),
        ],
        expected_output: Some(vec![
            PartialOrCommaOnly::Expr(make_default_print()),
            PartialOrCommaOnly::Comma,
            PartialOrCommaOnly::Expr(make_default_print()),
        ]),
    });
}

#[test]
fn print_or_print() {
    // Verify the parsing of -print -or -print.
    test_and_reduction(TestCase {
        input: vec![
            Expr(make_default_print()),
            Op(Or),
            Expr(make_default_print()),
        ],
        expected_output: Some(vec![
            PartialOrCommaOnly::Expr(make_default_print()),
            PartialOrCommaOnly::Or,
            PartialOrCommaOnly::Expr(make_default_print()),
        ]),
    });
}

#[test]
fn leading_and() {
    // Verify that a leading -and is an error.,
    test_and_reduction(TestCase {
        input: vec![Op(And), Expr(make_default_print())],
        expected_output: None,
    });
}

#[test]
fn trailing_and() {
    // Verify that a trailing -and is an error.,
    test_and_reduction(TestCase {
        input: vec![Expr(make_default_print()), Op(BinaryOperationKind::And)],
        expected_output: None,
    });
}

#[test]
fn lonely_and() {
    // Verify that a -and by itself is an error.,
    test_and_reduction(TestCase {
        input: vec![Op(And)],
        expected_output: None,
    });
}

#[test]
fn double_and() {
    // Verify that a -print -and -and -print is an error.,
    test_and_reduction(TestCase {
        input: vec![
            Expr(make_default_print()),
            Op(And),
            Op(And),
            Expr(make_default_print()),
        ],
        expected_output: None,
    });
}

#[test]
fn or_and() {
    // Verify that a -print -and -or -print is an error.,
    test_and_reduction(TestCase {
        input: vec![
            Expr(make_default_print()),
            Op(BinaryOperationKind::And),
            Op(BinaryOperationKind::Or),
            Expr(make_default_print()),
        ],
        expected_output: None,
    });
}

#[test]
fn and_or() {
    // Verify that a -print -and -or -print is an error.,
    test_and_reduction(TestCase {
        input: vec![
            Expr(make_default_print()),
            Op(BinaryOperationKind::And),
            Op(BinaryOperationKind::Or),
            Expr(make_default_print()),
        ],
        expected_output: None,
    });
}
