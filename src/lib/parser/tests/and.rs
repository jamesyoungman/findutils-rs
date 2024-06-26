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
