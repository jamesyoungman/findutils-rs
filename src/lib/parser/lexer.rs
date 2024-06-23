use std::ffi::OsStr;
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

fn tokenize_word(s: &OsStr) -> Result<TokenType, ParseError> {
    use BinaryOperationKind::*;
    match s.to_str() {
        Some(s) => match s {
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
        },
        None => Err(ParseError("byte sequence is not valif UTF-8".to_string())),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PredOrSyntax<'a> {
    Predicate {
        token_type: PredicateToken,
        arg: Option<&'a OsStr>,
    },
    Op(OperatorToken),
    Paren(Parenthesis),
    GlobalOptWithoutArg(GlobalOptionWithoutArg),
    GlobalOptWithArg(GlobalOptionWithArg, &'a OsStr),
}

fn starts_with(s: &OsStr, prefix: &str) -> bool {
    let prefix = prefix.as_bytes();
    s.as_encoded_bytes().starts_with(prefix)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CommandLineItem<'a> {
    StartingPoint(&'a OsStr),
    Code(PredOrSyntax<'a>),
    GlobalOption0(GlobalOptionWithoutArg),
    GlobalOption1(GlobalOptionWithArg, &'a OsStr),
}

pub fn tokenize_next_item<'a>(
    input: &mut &'a [&'a OsStr],
) -> Result<Option<CommandLineItem<'a>>, ParseError> {
    let mut pop_arg = || -> Option<&OsStr> {
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
                if starts_with(orig_token, "-") {
                    Err(e)
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
                        "predicate {} takes an argument but one was not specified",
                        orig_token.to_string_lossy(),
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
                    "global option {} needs an argument",
                    orig_token.to_string_lossy()
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

    use super::super::super::metadata::TypePredicateFileType;
    use super::*;

    #[cfg(test)]
    fn tokenize_single_predicate<'a>(
        mut input: &'a [&'a OsStr],
    ) -> Result<Option<(PredicateToken, Option<&'a OsStr>)>, ParseError> {
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
        for (display_rep, osrep, tok_expected) in
            TESTCASES.iter().map(|(s, tok)| (s, OsStr::new(s), tok))
        {
            match tokenize_word(osrep) {
                Err(e) => {
                    panic!("{display_rep} should be a valid token but it is rejected: {e}");
                }
                Ok(tok_got) => {
                    assert_eq!(
                        *tok_expected, tok_got,
                        "Expected {display_rep} to parse to {tok_expected:?}, but got {tok_got:?}"
                    );
                }
            }
        }
    }

    #[test]
    fn test_parse_next_item_exhaustive_for_predicates() {
        fn tokenize_valid_predicate<'a>(
            input: &'a [&'a OsStr],
        ) -> Option<(PredicateToken, Option<&'a OsStr>)> {
            match tokenize_single_predicate(&input) {
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
        test_inputs.into_iter().for_each(|fragment: &[&str]| {
            let args: Vec<&OsStr> = fragment.iter().map(|s| OsStr::new(s)).collect();
            match tokenize_valid_predicate(&args) {
                Some((pred_token_type, _maybe_arg)) => {
                    tokens_seen.insert(pred_token_type);
                }
                None => (),
            }
        });

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
            let input = &[OsStr::new("-type"), OsStr::new(letter)];
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
