use super::*;

#[cfg(test)]
fn verify_parse(input: &[&str], expected_starts: &[&str], expected_expr: &Expression) {
    let mut options = Options::default();
    match parse_program(input, &mut options) {
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
fn test_empty() {
    verify_parse(&[], &[], &Expression::Just(Box::new(PrintPredicate::new())));
}

#[test]
fn test_explicit_start() {
    verify_parse(
        &[".", "-print"],
        &["."],
        &Expression::Just(Box::new(PrintPredicate::new())),
    );

    verify_parse(&["foo/", "-print"], &["foo/"], &print_expr());
}

#[test]
fn test_missing_start() {
    verify_parse(
        &["-print"],
        // We no longer have the parser emit a default starting point;
        // instead we take care of that at the top level (in the
        // binary).
        &[],
        &Expression::Just(Box::new(PrintPredicate::new())),
    );
}

#[test]
fn test_binary_op_and() {
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
fn test_binary_op_or_symmetrical() {
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
fn test_binary_op_or_asymmetric() {
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
fn test_binary_op_precedence() {
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
fn test_parens_trivial_case() {
    verify_parse(&["foo/", "(", "-print", ")"], &["foo/"], &print_expr());
}

#[test]
fn test_parens_nested() {
    verify_parse(
        &["foo/", "(", "(", "-print", ")", ")"],
        &["foo/"],
        &print_expr(),
    );
}

#[test]
fn test_parens_non_trivial_case() {
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
fn test_parens_before_implicit_and() {
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
fn test_parens_before_explicit_and() {
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
fn test_true() {
    verify_parse(
        &["foo/", "-true"],
        &["foo/"],
        &Expression::BinaryOp(BinaryOperation::new(
            BinaryOperationKind::And,
            vec![
                Expression::Just(Box::new(TruePredicate {})),
                make_default_print(),
            ],
        )),
    );
}

#[test]
fn test_false() {
    verify_parse(
        &["foo/", "-false"],
        &["foo/"],
        &Expression::BinaryOp(BinaryOperation::new(
            BinaryOperationKind::And,
            vec![
                Expression::Just(Box::new(FalsePredicate {})),
                make_default_print(),
            ],
        )),
    );
}

#[test]
fn test_type_invalid() {
    let test_inputs: Vec<Vec<&str>> = vec![
        vec!["-type"],       // invalid because missing argument
        vec!["-typeZ", "f"], // invalid because not actually -type
        vec!["-type", "q"],  // invalid argument
    ];
    for input in test_inputs {
        let mut options = Options::default();
        match parse_program(input.as_slice(), &mut options) {
            Err(_) => (), // as excpted
            Ok(out) => {
                panic!("parsed invalid input {input:?} but unexpectedly got valid result {out:?}");
            }
        }
    }
}

#[test]
fn test_depth_option() {
    verify_parse(
        &["foo/", "-depth"],
        &["foo/"],
        &Expression::BinaryOp(BinaryOperation::new(
            BinaryOperationKind::And,
            vec![
                Expression::Just(Box::new(GlobalOptionPlaceholder::new(
                    GlobalOption::DepthFirst,
                ))),
                make_default_print(),
            ],
        )),
    );
}
