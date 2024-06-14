// Parser tests.
use std::collections::HashSet;
use std::collections::VecDeque;

#[cfg(test)]
use super::super::ast::BinaryOperation;
use super::*;

use enum_iterator::all;

fn make_vdq<T, I>(items: I) -> VecDeque<T>
where
    I: IntoIterator<Item = T>,
{
    items.into_iter().collect()
}

#[test]
fn test_tokenize_word() {
    const TESTCASES: &[(&str, TokenType)] = &[
        ("-print", TokenType::Pred(PredicateToken::Print)),
        ("-type", TokenType::Pred(PredicateToken::Type)),
        ("(", TokenType::Paren(Parenthesis::Left)),
        (")", TokenType::Paren(Parenthesis::Right)),
        ("!", TokenType::Op(OperatorToken::Not)),
        ("-not", TokenType::Op(OperatorToken::Not)),
        (
            "-a",
            TokenType::Op(OperatorToken::Binary(BinaryOperationKind::And)),
        ),
        (
            "-and",
            TokenType::Op(OperatorToken::Binary(BinaryOperationKind::And)),
        ),
        (
            "-o",
            TokenType::Op(OperatorToken::Binary(BinaryOperationKind::Or)),
        ),
        (
            "-or",
            TokenType::Op(OperatorToken::Binary(BinaryOperationKind::Or)),
        ),
        (
            ",",
            TokenType::Op(OperatorToken::Binary(BinaryOperationKind::Comma)),
        ),
    ];
    for (rep, tok_expected) in TESTCASES {
        match tokenize_word(rep) {
            Err(e) => {
                panic!("{rep} should be a valid token but it is rejected: {e}");
            }
            Ok(tok_got) => {
                assert_eq!(
                    *tok_expected, tok_got,
                    "Expected {rep} to parse to {tok_expected:?}, but got {tok_got:?}"
                );
            }
        }
    }
}

#[test]
fn test_parse_next_item_exhaustive_for_predicates() {
    fn tokenize_valid_predicate<'a>(
        input: &'a [&'a str],
    ) -> Option<(PredicateToken, Option<&'a str>)> {
        match tokenize_single_predicate(input) {
            Err(e) => {
                panic!("input {input:?} should be valid but was not: {e}");
            }
            Ok(out) => out,
        }
    }
    let all_tokens: HashSet<PredicateToken> = all::<PredicateToken>().collect();
    let mut tokens_seen: HashSet<PredicateToken> = HashSet::new();
    let test_inputs: Vec<&[&str]> = vec![
        &[],
        &["-print"],
        &["-false"],
        &["-true"],
        &["-type", "f"],
        &["-type", "d"],
        &["-type", "l"],
        &["-type", "b"],
        &["-type", "c"],
        &["-type", "p"],
        &["-type", "s"],
        &["-type", "D"],
    ];
    for fragment in test_inputs.iter() {
        match tokenize_valid_predicate(fragment) {
            Some((pred_token_type, _maybe_arg)) => {
                tokens_seen.insert(pred_token_type);
            }
            None => (),
        }
    }

    let missing: HashSet<PredicateToken> = all_tokens.difference(&tokens_seen).copied().collect();
    if !missing.is_empty() {
        panic!("unit test does not cover some token types: {missing:?}");
    }
}

#[test]
fn test_parse_type_invalid() {
    let test_inputs: Vec<Vec<&str>> = vec![
        vec!["-type"],       // invalid because missing argument
        vec!["-typeZ", "f"], // invalid because not actually -type
        vec!["-type", "q"],  // invalid argument
    ];
    for input in test_inputs {
        match parse_program(input.as_slice()) {
            Err(_) => (), // as excpted
            Ok(out) => {
                panic!("parsed invalid input {input:?} but unexpectedly got valid result {out:?}");
            }
        }
    }
}

#[test]
fn test_parse_type_valid() {
    let test_inputs: Vec<(&str, TypePredicateFileType)> = vec![
        ("b", TypePredicateFileType::BlockDevice),
        ("c", TypePredicateFileType::CharacterDevice),
        ("d", TypePredicateFileType::Directory),
        ("p", TypePredicateFileType::NamedPipe),
        ("f", TypePredicateFileType::RegularFile),
        ("l", TypePredicateFileType::SymbolicLink),
        ("s", TypePredicateFileType::Socket),
        ("D", TypePredicateFileType::Door),
    ];
    for (letter, _expected_indicator) in test_inputs {
        let input = &["-type", letter];
        match tokenize_single_predicate(input) {
            Err(e) => {
                panic!("failed to parse {input:?}: {e}");
            }
            Ok(Some((_, _boxed_predicate))) => {
                // TODO: verify the value of boxed_predicate.
            }
            other => {
                panic!("parsed valid input {input:?} got unexpected result {other:?}");
            }
        }
    }
}

#[cfg(test)]
fn verify_parse(input: &[&str], expected_starts: &[&str], expected_expr: &Expression) {
    match parse_program(input) {
        Err(e) => {
            panic!("failed to parse {input:?}: {e}");
        }
        Ok((got_starts, got_expression)) => {
            assert_eq!(got_starts, expected_starts);
            let expected_repr = format!("{expected_expr:#?}");
            let got_repr = format!("{got_expression:#?}");
            if expected_repr != got_repr {
                panic!("parsed expressions differ\nexpected:\n{expected_repr}\ngot:\n{got_repr}");
            }
        }
    }
}

#[cfg(test)]
fn print_expr() -> Expression {
    Expression::Just(Box::new(PrintPredicate::new()))
}

#[cfg(test)]
fn type_expr(arg: &str) -> Expression {
    Expression::Just(Box::new(
        TypePredicate::new(arg).expect("argument to TypePredicate::new() should be valid"),
    ))
}

#[test]
fn test_parse_program_empty() {
    verify_parse(
        &[],
        &["."],
        &Expression::Just(Box::new(PrintPredicate::new())),
    );
}

#[test]
fn test_parse_program_explicit_start() {
    verify_parse(
        &[".", "-print"],
        &["."],
        &Expression::Just(Box::new(PrintPredicate::new())),
    );

    verify_parse(&["foo/", "-print"], &["foo/"], &print_expr());
}

#[test]
fn test_parse_program_implicit_start() {
    verify_parse(
        &["-print"],
        &["."],
        &Expression::Just(Box::new(PrintPredicate::new())),
    );
}

#[test]
fn test_parse_program_binary_op_and() {
    verify_parse(
        &["foo/", "-print", "-a", "-print"],
        &["foo/"],
        &Expression::BinaryOp(BinaryOperation::new(
            BinaryOperationKind::And,
            vec![print_expr(), print_expr()],
        )),
    );

    verify_parse(
        &["foo/", "-print", "-and", "-print"],
        &["foo/"],
        &Expression::BinaryOp(BinaryOperation::new(
            BinaryOperationKind::And,
            vec![print_expr(), print_expr()],
        )),
    );
}

#[test]
fn test_parse_program_binary_op_or_symmetrical() {
    verify_parse(
        &["foo/", "-print", "-o", "-print"],
        &["foo/"],
        &Expression::BinaryOp(BinaryOperation::new(
            BinaryOperationKind::Or,
            vec![print_expr(), print_expr()],
        )),
    );
}

#[test]
fn test_parse_program_binary_op_or_asymmetric() {
    verify_parse(
        &["foo/", "-type", "f", "-o", "-print"],
        &["foo/"],
        &Expression::BinaryOp(BinaryOperation::new(
            BinaryOperationKind::Or,
            vec![type_expr("f"), print_expr()],
        )),
    );
}

#[test]
fn test_parse_program_binary_op_precedence() {
    // TODO: verify "x -o -y -o z" once we can actually parse that as
    // an operation with 3 children instead of two binary operations.

    verify_parse(
        &["foo/", "-print", "-o", "-print", "-a", "-print"],
        &["foo/"],
        &Expression::BinaryOp(BinaryOperation::new(
            BinaryOperationKind::Or,
            vec![
                print_expr(),
                Expression::BinaryOp(BinaryOperation::new(
                    BinaryOperationKind::And,
                    vec![print_expr(), print_expr()],
                )),
            ],
        )),
    );
}

#[test]
fn test_parse_program_parens_trivial_case() {
    verify_parse(&["foo/", "(", "-print", ")"], &["foo/"], &print_expr());
}

#[test]
fn test_parse_program_parens_nested() {
    verify_parse(
        &["foo/", "(", "(", "-print", ")", ")"],
        &["foo/"],
        &print_expr(),
    );
}

#[test]
fn test_parse_program_parens_non_trivial_case() {
    verify_parse(
        &["foo/", "(", "-print", "-print", ")"],
        &["foo/"],
        &Expression::BinaryOp(BinaryOperation::new(
            BinaryOperationKind::And,
            vec![print_expr(), print_expr()],
        )),
    );
}

#[test]
fn test_parse_program_parens_before_implicit_and() {
    verify_parse(
        &["foo/", "-type", "d", "(", "-print", ")"],
        &["foo/"],
        &Expression::BinaryOp(BinaryOperation::new(
            BinaryOperationKind::And,
            vec![type_expr("d"), print_expr()],
        )),
    );
}

#[test]
fn test_parse_program_parens_before_explicit_and() {
    verify_parse(
        &["foo/", "-type", "d", "-a", "(", "-print", ")"],
        &["foo/"],
        &Expression::BinaryOp(BinaryOperation::new(
            BinaryOperationKind::And,
            vec![type_expr("d"), print_expr()],
        )),
    );
}

#[test]
fn test_parse_program_true() {
    verify_parse(
        &["foo/", "-true"],
        &["foo/"],
        &Expression::Just(Box::new(TruePredicate {})),
    );
}

#[test]
fn test_parse_program_false() {
    verify_parse(
        &["foo/", "-false"],
        &["foo/"],
        &Expression::Just(Box::new(FalsePredicate {})),
    );
}

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
fn test_reduce_comma_ops() {
    // Verify the parsing of an empty expression.
    test_comma_reduction(CommaReductionTestCase {
        input: vec![],
        expected_output: Some(make_default_print()),
    });

    // Verify the parsing of an expression containing just -print.
    test_comma_reduction(CommaReductionTestCase {
        input: vec![PartialCommaOnly::Expr(make_default_print())],
        expected_output: Some(make_default_print()),
    });

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

    // Verify that a leading comma is an error.,
    test_comma_reduction(CommaReductionTestCase {
        input: vec![
            PartialCommaOnly::Comma,
            PartialCommaOnly::Expr(make_default_print()),
        ],
        expected_output: None,
    });

    // Verify that a trailing comma is an error.,
    test_comma_reduction(CommaReductionTestCase {
        input: vec![
            PartialCommaOnly::Expr(make_default_print()),
            PartialCommaOnly::Comma,
        ],
        expected_output: None,
    });

    // Verify that a lonely comma is an error.,
    test_comma_reduction(CommaReductionTestCase {
        input: vec![
            PartialCommaOnly::Expr(make_default_print()),
            PartialCommaOnly::Comma,
        ],
        expected_output: None,
    });

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
