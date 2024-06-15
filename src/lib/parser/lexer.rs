use std::fmt::Display;

use enum_iterator::Sequence;

use super::super::ast::BinaryOperationKind;
use super::super::errors::ParseError;
use super::super::options::{GlobalOptionWithArg, GlobalOptionWithoutArg};

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum Arity {
    Zero, // e.g. -print
    One,  // e.g. -name, -type
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Sequence, Hash)]
pub enum PredicateToken {
    False,
    Print,
    True,
    Type,
}

impl PredicateToken {
    fn arity(&self) -> Arity {
        use PredicateToken::*;
        match self {
            False | Print | True => Arity::Zero,
            Type => Arity::One,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub enum OperatorToken {
    Binary(BinaryOperationKind),
    Not,
}

impl Display for OperatorToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OperatorToken::Binary(kind) => write!(f, "{}", kind),
            OperatorToken::Not => f.write_str("!"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Sequence, Hash)]
pub enum Parenthesis {
    Left,
    Right,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
enum TokenType {
    Pred(PredicateToken),
    Op(OperatorToken),
    Paren(Parenthesis),
    GlobalOptNoArg(GlobalOptionWithoutArg),
    GlobalOptWithArg(GlobalOptionWithArg),
}

fn tokenize_word(s: &str) -> Result<TokenType, ParseError> {
    use BinaryOperationKind::*;
    match s {
        "-depth" => Ok(TokenType::GlobalOptNoArg(
            GlobalOptionWithoutArg::DepthFirst,
        )),
        "-print" => Ok(TokenType::Pred(PredicateToken::Print)),
        "-true" => Ok(TokenType::Pred(PredicateToken::True)),
        "-false" => Ok(TokenType::Pred(PredicateToken::False)),
        "-maxdepth" => Ok(TokenType::GlobalOptWithArg(GlobalOptionWithArg::MaxDepth)),
        "-mindepth" => Ok(TokenType::GlobalOptWithArg(GlobalOptionWithArg::MinDepth)),
        "-type" => Ok(TokenType::Pred(PredicateToken::Type)),
        "(" => Ok(TokenType::Paren(Parenthesis::Left)),
        ")" => Ok(TokenType::Paren(Parenthesis::Right)),
        "!" | "-not" => Ok(TokenType::Op(OperatorToken::Not)),
        "-and" | "-a" => Ok(TokenType::Op(OperatorToken::Binary(And))),
        "-or" | "-o" => Ok(TokenType::Op(OperatorToken::Binary(Or))),
        "," => Ok(TokenType::Op(OperatorToken::Binary(Comma))),
        _ => Err(ParseError(format!("unknown token {s}"))),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PredOrSyntax<'a> {
    Predicate {
        token_type: PredicateToken,
        arg: Option<&'a str>,
    },
    Op(OperatorToken),
    Paren(Parenthesis),
    GlobalOptWithoutArg(GlobalOptionWithoutArg),
    GlobalOptWithArg(GlobalOptionWithArg, &'a str),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CommandLineItem<'a> {
    StartingPoint(&'a str),
    Code(PredOrSyntax<'a>),
    GlobalOption0(GlobalOptionWithoutArg),
    GlobalOption1(GlobalOptionWithArg, &'a str),
}

pub fn tokenize_next_item<'a>(
    input: &mut &'a [&'a str],
) -> Result<Option<CommandLineItem<'a>>, ParseError> {
    let mut pop_arg = || -> Option<&str> {
        match input {
            [first, rest @ ..] => {
                *input = rest;
                Some(first)
            }
            [] => None,
        }
    };

    if let Some(orig_token) = pop_arg() {
        match tokenize_word(orig_token) {
            Err(e) => {
                if orig_token.starts_with('-') {
                    return Err(e);
                } else {
                    Ok(Some(CommandLineItem::StartingPoint(orig_token)))
                }
            }
            Ok(TokenType::Pred(pred_tok)) => match pred_tok.arity() {
                Arity::Zero => Ok(Some(CommandLineItem::Code(PredOrSyntax::Predicate {
                    token_type: pred_tok,
                    arg: None,
                }))),
                Arity::One => match pop_arg() {
                    Some(arg) => Ok(Some(CommandLineItem::Code(PredOrSyntax::Predicate {
                        token_type: pred_tok,
                        arg: Some(arg),
                    }))),
                    None => Err(ParseError(format!(
                        "predicate {orig_token} takes an argument but one was not specified"
                    ))),
                },
            },
            Ok(TokenType::Op(op)) => Ok(Some(CommandLineItem::Code(PredOrSyntax::Op(op)))),
            Ok(TokenType::Paren(side)) => {
                Ok(Some(CommandLineItem::Code(PredOrSyntax::Paren(side))))
            }
            Ok(TokenType::GlobalOptNoArg(option)) => {
                Ok(Some(CommandLineItem::GlobalOption0(option)))
            }
            Ok(TokenType::GlobalOptWithArg(option)) => match pop_arg() {
                Some(arg) => Ok(Some(CommandLineItem::GlobalOption1(option, arg))),
                None => Err(ParseError(format!(
                    "global option {orig_token} needs an argument"
                ))),
            },
        }
    } else {
        Ok(None) // end of input
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use enum_iterator::all;

    use super::super::super::predicate::TypePredicateFileType;
    use super::*;

    #[cfg(test)]
    fn tokenize_single_predicate<'a>(
        mut input: &'a [&'a str],
    ) -> Result<Option<(PredicateToken, Option<&'a str>)>, ParseError> {
        match tokenize_next_item(&mut input) {
            Err(e) => Err(e),
            Ok(Some(CommandLineItem::Code(PredOrSyntax::Predicate { token_type, arg }))) => {
                let result = Some((token_type, arg));
                // Check that we have exhausted the input.
                match tokenize_next_item(&mut input) {
                    Err(e) => panic!("unexpected parse error {e}"),
                    Ok(None) => (),
                    Ok(Some(parsed)) => {
                        panic!(
                            "input should include only one item, but we saw {parsed:?} at the end"
                        );
                    }
                }
                Ok(result)
            }
            Ok(Some(CommandLineItem::StartingPoint(here))) => {
                panic!("input token {here:?} could not be parsed as a predicate");
            }
            Ok(None) => Ok(None),
            other => {
                panic!("input {input:?} should have been parsed as predicates only, but appears to contain {other:?}");
            }
        }
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

        let missing: HashSet<PredicateToken> =
            all_tokens.difference(&tokens_seen).copied().collect();
        if !missing.is_empty() {
            panic!("unit test does not cover some token types: {missing:?}");
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
}
