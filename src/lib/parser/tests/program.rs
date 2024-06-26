// This file is part of findutils-rs
// Copyright (C) 2024 James Youngman
//
// findutils-rs is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

use std::ffi::OsStr;

use super::super::super::{
    ast::{BinaryOperation, BinaryOperationKind, Expression},
    options::{GlobalOptionWithoutArg, Options},
    parse_program,
    parser::{make_default_print, print_expr},
    predicate::{
        FalsePredicate, GlobalOptionPlaceholder, PrintPredicate, TruePredicate, TypePredicate,
    },
};

#[cfg(test)]
fn verify_parse_and_options(
    input: &[&str],
    expected_starts: &[&str],
    expected_expr: &Expression,
    options: &Options,
) {
    verify_parse_and_maybe_options(input, expected_starts, expected_expr, Some(options));
}

#[cfg(test)]
fn verify_parse(input: &[&str], expected_starts: &[&str], expected_expr: &Expression) {
    verify_parse_and_maybe_options(input, expected_starts, expected_expr, None);
}

#[cfg(test)]
fn verify_parse_and_maybe_options(
    input: &[&str],
    expected_starts: &[&str],
    expected_expr: &Expression,
    expected_options: Option<&Options>,
) {
    let mut options = Options::default();
    let input: Vec<&OsStr> = input.iter().map(|s| OsStr::new(s)).collect();
    match parse_program(&input, &mut options) {
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
    if let Some(expected) = expected_options {
        assert_eq!(&options, expected);
    }
}

#[cfg(test)]
fn type_expr(arg: &str) -> Expression {
    Expression::Just(Box::new(
        TypePredicate::new(OsStr::new(arg))
            .expect("argument to TypePredicate::new() should be valid"),
    ))
}

#[test]
fn test_empty() {
    verify_parse(&[], &[], &Expression::Just(Box::new(PrintPredicate::new())));
}

#[test]
fn test_explicit_start() {
    verify_parse_and_options(
        &[".", "-print"],
        &["."],
        &Expression::Just(Box::new(PrintPredicate::new())),
        &Options::default(),
    );

    verify_parse_and_options(
        &["foo/", "-print"],
        &["foo/"],
        &print_expr(),
        &Options::default(),
    );
}

#[test]
fn test_missing_start() {
    verify_parse_and_options(
        &["-print"],
        // We no longer have the parser emit a default starting point;
        // instead we take care of that at the top level (in the
        // binary).
        &[],
        &Expression::Just(Box::new(PrintPredicate::new())),
        &Options::default(),
    );
}

#[test]
fn test_binary_op_and() {
    verify_parse_and_options(
        &["foo/", "-print", "-a", "-print"],
        &["foo/"],
        &Expression::BinaryOp(BinaryOperation::new(
            BinaryOperationKind::And,
            vec![print_expr(), print_expr()],
        )),
        &Options::default(),
    );

    verify_parse_and_options(
        &["foo/", "-print", "-and", "-print"],
        &["foo/"],
        &Expression::BinaryOp(BinaryOperation::new(
            BinaryOperationKind::And,
            vec![print_expr(), print_expr()],
        )),
        &Options::default(),
    );
}

#[test]
fn test_binary_op_or_symmetrical() {
    verify_parse_and_options(
        &["foo/", "-print", "-o", "-print"],
        &["foo/"],
        &Expression::BinaryOp(BinaryOperation::new(
            BinaryOperationKind::Or,
            vec![print_expr(), print_expr()],
        )),
        &Options::default(),
    );
}

#[test]
fn test_binary_op_or_asymmetric() {
    verify_parse_and_options(
        &["foo/", "-type", "f", "-o", "-print"],
        &["foo/"],
        &Expression::BinaryOp(BinaryOperation::new(
            BinaryOperationKind::Or,
            vec![type_expr("f"), print_expr()],
        )),
        &Options::default(),
    );
}

#[test]
fn test_binary_op_precedence() {
    // TODO: verify "x -o -y -o z" once we can actually parse that as
    // an operation with 3 children instead of two binary operations.

    verify_parse_and_options(
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
        &Options::default(),
    );
}

#[test]
fn test_parens_trivial_case() {
    verify_parse_and_options(
        &["foo/", "(", "-print", ")"],
        &["foo/"],
        &print_expr(),
        &Options::default(),
    );
}

#[test]
fn test_parens_nested() {
    verify_parse_and_options(
        &["foo/", "(", "(", "-print", ")", ")"],
        &["foo/"],
        &print_expr(),
        &Options::default(),
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
        vec!["find", "-type"],       // invalid because missing argument
        vec!["find", "-typeZ", "f"], // invalid because not actually -type
        vec!["find", "-type", "q"],  // invalid argument
    ];
    for input in test_inputs {
        let mut options = Options::default();
        let input: Vec<&OsStr> = input.into_iter().map(|s| OsStr::new(s)).collect();
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
    let mut expected_options = Options::default();
    expected_options.apply(GlobalOptionWithoutArg::DepthFirst);
    verify_parse_and_options(
        &["foo/", "-depth"],
        &["foo/"],
        &Expression::BinaryOp(BinaryOperation::new(
            BinaryOperationKind::And,
            vec![
                Expression::Just(Box::new(GlobalOptionPlaceholder::without_arg(
                    GlobalOptionWithoutArg::DepthFirst,
                ))),
                make_default_print(),
            ],
        )),
        &expected_options,
    );
}

#[test]
fn parse_otions_with_invalid_negative_mindepth() {
    let mut options = Options::default();
    match parse_program(
        &[
            OsStr::new("find"),
            OsStr::new("-mindepth"),
            OsStr::new("-1"),
            OsStr::new("-print"),
        ],
        &mut options,
    ) {
        Err(e) => {
            dbg!(&e);
            assert!(e.to_string().contains("negative"));
        }
        Ok(result) => {
            panic!("parse should have failed but it returned {:?}", result);
        }
    }
}

#[test]
fn parse_options_with_invalid_nonnumeric_mindepth() {
    let mut options = Options::default();
    match parse_program(
        &[
            OsStr::new("find"),
            OsStr::new("-mindepth"),
            OsStr::new("BAD"),
            OsStr::new("-print"),
        ],
        &mut options,
    ) {
        Err(e) => {
            dbg!(&e);
            assert!(e.to_string().contains("BAD"));
        }
        Ok(result) => {
            panic!("parse should have failed but it returned {:?}", result);
        }
    }
}

#[test]
fn parse_options_with_invalid_negative_maxdepth() {
    let mut options = Options::default();
    match parse_program(
        &[
            OsStr::new("find"),
            OsStr::new("-maxdepth"),
            OsStr::new("-1"),
            OsStr::new("-print"),
        ],
        &mut options,
    ) {
        Err(e) => {
            dbg!(&e);
            assert!(e.to_string().contains("negative"));
        }
        Ok(result) => {
            panic!("parse should have failed but it returned {:?}", result);
        }
    }
}

#[test]
fn parse_options_with_invalid_nonnumeric_maxdepth() {
    let mut options = Options::default();
    match parse_program(
        &[
            OsStr::new("find"),
            OsStr::new("-maxdepth"),
            OsStr::new("FOO"),
            OsStr::new("-print"),
        ],
        &mut options,
    ) {
        Err(e) => {
            dbg!(&e);
            assert!(e.to_string().contains("FOO"));
        }
        Ok(result) => {
            panic!("parse should have failed but it returned {:?}", result);
        }
    }
}
