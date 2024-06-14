use super::super::*;
use super::*;

#[derive(Debug)]
struct TestCase {
    input: Vec<PartialNoPredicates>,
    expected_output: Option<Vec<PartialNoParens>>,
}

#[cfg(test)]
fn test_paren_reduction(test_data: TestCase) {
    let input_repr = format!("{:?}", &test_data.input);
    let vdq = make_vdq(test_data.input);
    match (reduce_paren_expressions(vdq), test_data.expected_output) {
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
    test_paren_reduction(TestCase {
        input: vec![],
        expected_output: Some(vec![]),
    });
}

#[test]
fn just_print() {
    // Verify the parsing of an expression containing just -print.
    test_paren_reduction(TestCase {
        input: vec![PartialNoPredicates::Expr(make_default_print())],
        expected_output: Some(vec![PartialNoParens::Expr(make_default_print())]),
    });
}

#[test]
fn lparen_print_rparen() {
    // Verify the parsing of an expression containing just -print.
    test_paren_reduction(TestCase {
        input: vec![
            PartialNoPredicates::Lparen,
            PartialNoPredicates::Expr(make_default_print()),
            PartialNoPredicates::Rparen,
        ],
        expected_output: Some(vec![PartialNoParens::Expr(make_default_print())]),
    });
}

#[test]
fn lparen_2_print_rparen_2() {
    // Verify the parsing of an expression containing just -print.
    test_paren_reduction(TestCase {
        input: vec![
            PartialNoPredicates::Lparen,
            PartialNoPredicates::Lparen,
            PartialNoPredicates::Expr(make_default_print()),
            PartialNoPredicates::Rparen,
            PartialNoPredicates::Rparen,
        ],
        expected_output: Some(vec![PartialNoParens::Expr(make_default_print())]),
    });
}

#[test]
fn lone_lparen() {
    // Verify the parsing of an expression containing just -print.
    test_paren_reduction(TestCase {
        input: vec![PartialNoPredicates::Lparen],
        expected_output: None,
    });
}

#[test]
fn lone_rparen() {
    // Verify the parsing of an expression containing just -print.
    test_paren_reduction(TestCase {
        input: vec![PartialNoPredicates::Rparen],
        expected_output: None,
    });
}

#[test]
fn unclosed_lparen() {
    // Verify the parsing of an expression containing just -print.
    test_paren_reduction(TestCase {
        input: vec![
            PartialNoPredicates::Lparen,
            PartialNoPredicates::Expr(make_default_print()),
        ],
        expected_output: None,
    });
}

#[test]
fn unmatched_rparen() {
    // Verify the parsing of an expression containing just -print.
    test_paren_reduction(TestCase {
        input: vec![
            PartialNoPredicates::Expr(make_default_print()),
            PartialNoPredicates::Rparen,
        ],
        expected_output: None,
    });
}
