#[cfg(test)]
use std::collections::HashSet;
use std::collections::VecDeque;

use super::ast::{BoxedPredicate, Expression, Predicate};
use super::errors::ParseError;
use super::predicate::*;

use enum_iterator::{all, Sequence};

#[derive(Debug, Eq, PartialEq, Clone, Copy, PartialOrd, Ord)]
enum Precedence {
    Comma, // lowest
    Or,
    And,
    Not,
    Paren, // highest
}

#[test]
fn test_precedence_comparison() {
    assert!(Precedence::Comma < Precedence::Or);
    assert!(Precedence::Or < Precedence::And);
    assert!(Precedence::And < Precedence::Not);
    assert!(Precedence::Not < Precedence::Paren);
}

/// We could represent Arity as u32 but we want to structure parts of
/// the code so that we can be sure that all possible arity values are
/// covered.  Therefore we make it an enum.
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum Arity {
    Zero, // e.g. -print
    One,  // e.g. -name, -type
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Sequence, Hash)]
enum TokenType {
    Print,
    Type,
    Lparen,
    Rparen,
    And,
    Or,
    Not,
    Comma,
}

impl TokenType {
    fn arity(&self) -> Arity {
        use Arity::*;
        use TokenType::*;
        match self {
            Print => Zero,
            Type => One,
            Lparen | Rparen | And | Or | Not | Comma => Zero,
        }
    }

    fn precedence(&self) -> Option<Precedence> {
        use TokenType::*;
        match self {
            Lparen | Rparen => Some(Precedence::Paren),
            Not => Some(Precedence::Not),
            And => Some(Precedence::And),
            Or => Some(Precedence::Or),
            Comma => Some(Precedence::Comma),
            _ => None,
        }
    }
}

fn tokenize_word(s: &str) -> Result<TokenType, ParseError> {
    use TokenType::*;
    let tok = match s {
        "-print" => Print,
        "-type" => Type,
        "(" => Lparen,
        ")" => Rparen,
        "-not" => Not,
        "-and" | "-a" => And,
        "-or" | "-o" => Or,
        "," => Comma,
        _ => {
            return Err(ParseError(format!("unknown token {s}")));
        }
    };
    Ok(tok)
}

fn expect1<'a>(predicate: &str, mut v: Vec<&'a str>) -> &'a str {
    match v.pop() {
        Some(s) => {
            if v.is_empty() {
                s
            } else {
                unreachable!("predicate {predicate} was parsed with too many arguments")
            }
        }
        None => {
            unreachable!("predicate {predicate} was parsed with no arguments")
        }
    }
}

fn build_zero_arg_predicate(
    token_type: TokenType,
    orig_token: &str,
) -> Result<Box<dyn Predicate + Send + Sync>, ParseError> {
    use TokenType::*;
    match token_type {
        Print => Ok(Box::new(PrintPredicate::new())),
        Type => unreachable!(
            "build_zero_arg_predicate called on {token_type:?} token {orig_token} having non-zero arity"
        ),
	syntax @ (Lparen | Rparen | And | Or | Not | Comma) => unreachable!("build_zero_arg_predicate called on {syntax:?}"),
    }
}

fn build_one_arg_predicate(
    token_type: TokenType,
    orig_token: &str,
    arg: &str,
) -> Result<Box<dyn Predicate + Send + Sync>, ParseError> {
    use TokenType::*;
    match token_type {
        Type => Ok(Box::new(TypePredicate::new(arg)?)),
        Print => unreachable!(
            "build_one_arg_predicate called on {token_type:?} token {orig_token} having arity other than 1"
        ),
	syntax @ (Lparen | Rparen | And | Or | Not | Comma) => unreachable!("build_one_arg_predicate called on {syntax:?}"),
    }
}

#[derive(Debug)]
enum PredOrSyntax {
    Predicate {
        token_type: TokenType,
        predicate: Box<dyn Predicate + Send + Sync>,
    },
    Syntax(TokenType),
}

#[derive(Debug)]
enum ParsedItem<'a> {
    StartPoint(&'a str),
    Program(PredOrSyntax),
}

fn parse_next_item<'a>(
    input: &mut VecDeque<&'a str>,
    in_program: bool,
) -> Result<Option<ParsedItem<'a>>, ParseError> {
    if let Some(orig_token) = input.pop_front() {
        match tokenize_word(orig_token) {
            Err(_) => {
                if in_program {
                    Err(ParseError(format!(
                        "unexpected word '{orig_token}'; expected a test or action"
                    )))
                } else {
                    Ok(Some(ParsedItem::StartPoint(orig_token)))
                }
            }
            Ok(element @ (TokenType::Lparen | TokenType::Rparen)) => {
                Ok(Some(ParsedItem::Program(PredOrSyntax::Syntax(element))))
            }
            Ok(token_type) => match token_type.arity() {
                Arity::Zero => Ok(Some(ParsedItem::Program(PredOrSyntax::Predicate {
                    token_type,
                    predicate: build_zero_arg_predicate(token_type, orig_token)?,
                }))),
                Arity::One => match input.pop_front() {
                    Some(arg) => Ok(Some(ParsedItem::Program(PredOrSyntax::Predicate {
                        token_type,
                        predicate: build_one_arg_predicate(token_type, orig_token, arg)?,
                    }))),
                    None => Err(ParseError(format!(
                        "predicate {orig_token} takes an argument but one was not specified"
                    ))),
                },
            },
        }
    } else {
        Ok(None) // end of input
    }
}

#[cfg(test)]
fn parse_single_predicate(
    input: &[&str],
) -> Result<Option<(TokenType, Box<dyn Predicate + Send + Sync>)>, ParseError> {
    let mut queue: VecDeque<&str> = input.into_iter().copied().collect();
    match parse_next_item(&mut queue, true) {
        Err(e) => Err(e),
        Ok(Some(ParsedItem::StartPoint(top))) => {
            panic!("input {input:?} should have been parsed as a predicate, but appears to be a start point");
        }
        Ok(Some(ParsedItem::Predicate {
            token_type,
            predicate,
        })) => {
            let result = Some((token_type, predicate));
            // Check that we have exhausted the input.
            match parse_next_item(&mut queue, true) {
                Err(e) => panic!("unexpected parse error {e}"),
                Ok(None) => (),
                Ok(Some(parsed)) => {
                    panic!("input should include only one item, but we saw {parsed:?} at the end");
                }
            }
            Ok(result)
        }
        Ok(None) => Ok(None),
    }
}

#[test]
fn test_parse_next_item_exhaustive() {
    fn do_valid_parse(input: &[&str]) -> Option<(TokenType, BoxedPredicate)> {
        match parse_single_predicate(input) {
            Err(e) => {
                panic!("input {input:?} should be valid but was not: {e}");
            }
            Ok(out) => out,
        }
    }
    let mut all_tokens: HashSet<TokenType> = all::<TokenType>().collect();
    let mut tokens_seen: HashSet<TokenType> = HashSet::new();
    let test_inputs: Vec<&[&str]> = vec![
        &[],
        &["-print"],
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
        match do_valid_parse(fragment) {
            Some((token_type, _predicate)) => {
                tokens_seen.insert(token_type);
            }
            None => (),
        }
    }

    let missing: HashSet<TokenType> = all_tokens.difference(&tokens_seen).copied().collect();
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
        match parse_single_predicate(input.as_slice()) {
            Err(e) => (),
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
    for (letter, expected_indicator) in test_inputs {
        let input = &["-type", letter];
        match parse_single_predicate(input) {
            Err(e) => {
                panic!("failed to parse {input:?}: {e}");
            }
            Ok(Some((_, boxed_predicate))) => {
                // TODO: verify the value of boxed_predicate.
            }
            other => {
                panic!("parsed valid input {input:?} got unexpected result {other:?}");
            }
        }
    }
}

fn build_program(predicates: VecDeque<PredOrSyntax>) -> Result<Option<Expression>, ParseError> {
    todo!("write build_program")
}

fn make_default_print() -> Expression {
    Expression::Just(Box::new(PrintPredicate::new()))
}

pub fn parse_program(mut input: VecDeque<&str>) -> Result<(Vec<&str>, Expression), ParseError> {
    let mut starting_points: Vec<&str> = Vec::new();
    let mut predicates: VecDeque<PredOrSyntax> = VecDeque::new();
    loop {
        match parse_next_item(&mut input, !predicates.is_empty()) {
            Err(e) => {
                return Err(e);
            }
            Ok(None) => {
                if starting_points.is_empty() {
                    starting_points.push(".");
                }
                let expr = build_program(predicates)?.unwrap_or_else(make_default_print);
                return Ok((starting_points, expr));
            }
            Ok(Some(ParsedItem::StartPoint(top))) => {
                starting_points.push(top);
            }
            Ok(Some(ParsedItem::Program(pred))) => {
                predicates.push_back(pred);
            }
        }
    }
}
