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

use super::super::*;
use super::*;

#[derive(Debug)]
enum Outcome {
    Fail,
    None,
    Expr(Expression),
}

#[derive(Debug)]
struct CommaReductionTestCase {
    input: Vec<PartialCommaOnly>,
    expected_output: Outcome,
}

#[cfg(test)]
fn test_comma_reduction(test_data: CommaReductionTestCase) {
    let input_repr = format!("{:?}", &test_data.input);
    let vdq = make_vdq(test_data.input);
    match (reduce_comma_ops(vdq), test_data.expected_output) {
        (Ok(Some(got)), Outcome::Expr(expected)) => {
            let got_repr = format!("{got:?}");
            let expected_repr = format!("{expected:?}");
            if got_repr != expected_repr {
                panic!("for input {input_repr}, expected output {expected_repr} but got output {got_repr}");
            }
        }
        (Ok(None), Outcome::None) => (),
        (Err(_), Outcome::Fail) => (),
        (Ok(Some(got)), Outcome::None) => {
            panic!("expected a successful parse but no resulting expression for input {input_repr} but got result instead: {got:?}");
        }
        (Ok(None), Outcome::Expr(expected)) => {
            panic!("for input {input_repr}, expected output {expected:?} but got no expression");
        }
        (Err(e), Outcome::Expr(expr)) => {
            panic!("expected result {expr:?} for input {input_repr} but got error instead: {e}");
        }
        (Err(e), Outcome::None) => {
            panic!(
                "expected no-expression output for input {input_repr} but got error instead: {e}"
            );
        }
        (Ok(expr), Outcome::Fail) => {
            panic!("expected a failure for input {input_repr} but got a prased expression instead: {expr:?}");
        }
    }
}

#[test]
fn empty() {
    // Verify the parsing of an empty expression.
    test_comma_reduction(CommaReductionTestCase {
        input: vec![],
        expected_output: Outcome::None,
    });
}

#[test]
fn just_print() {
    // Verify the parsing of an expression containing just -print.
    test_comma_reduction(CommaReductionTestCase {
        input: vec![PartialCommaOnly::Expr(make_default_print())],
        expected_output: Outcome::Expr(make_default_print()),
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
        expected_output: Outcome::Expr(Expression::BinaryOp(BinaryOperation::new(
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
        expected_output: Outcome::Fail,
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
        expected_output: Outcome::Fail,
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
        expected_output: Outcome::Fail,
    });
}

#[test]
fn double_comma() {
    // Verify that a double comma is an error.
    test_comma_reduction(CommaReductionTestCase {
        input: vec![
            PartialCommaOnly::Expr(make_default_print()),
            PartialCommaOnly::Comma,
            PartialCommaOnly::Comma,
            PartialCommaOnly::Expr(make_default_print()),
        ],
        expected_output: Outcome::Fail,
    });
}
