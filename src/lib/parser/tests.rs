// Parser tests.
use std::collections::HashSet;
use std::collections::VecDeque;

#[cfg(test)]
use super::super::ast::BinaryOperation;
use super::*;

mod and;
mod comma;
mod or;
mod parens;
mod program;
mod type_pred;

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
