#[cfg(test)]
mod tests;

use std::collections::VecDeque;

use std::fmt::Display;
use std::num::NonZeroUsize;

use super::ast::BoxedPredicate;
use super::ast::{BinaryOperation, BinaryOperationKind, Expression, Predicate};
use super::errors::ParseError;
use super::predicate::*;

use enum_iterator::Sequence;

/// We could represent Arity as u32 but we want to structure parts of
/// the code so that we can be sure that all possible arity values are
/// covered.  Therefore we make it an enum.
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum Arity {
    Zero, // e.g. -print
    One,  // e.g. -name, -type
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Sequence, Hash)]
enum PredicateToken {
    False,
    Print,
    True,
    Type,
}

impl PredicateToken {
    pub fn arity(&self) -> Arity {
        use Arity::*;
        use PredicateToken::*;
        match self {
            False | Print | True => Zero,
            Type => One,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
enum OperatorToken {
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
enum Parenthesis {
    Left,
    Right,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
enum TokenType {
    Pred(PredicateToken),
    Op(OperatorToken),
    Paren(Parenthesis),
}

fn tokenize_word(s: &str) -> Result<TokenType, ParseError> {
    use BinaryOperationKind::*;
    match s {
        "-print" => Ok(TokenType::Pred(PredicateToken::Print)),
        "-true" => Ok(TokenType::Pred(PredicateToken::True)),
        "-false" => Ok(TokenType::Pred(PredicateToken::False)),
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
enum PredOrSyntax<'a> {
    Predicate {
        token_type: PredicateToken,
        arg: Option<&'a str>,
    },
    Op(OperatorToken),
    Paren(Parenthesis),
}

impl<'a> PredOrSyntax<'a> {
    fn maybe_get_paren(&self) -> Option<Parenthesis> {
        if let PredOrSyntax::Paren(paren_type) = self {
            Some(*paren_type)
        } else {
            None
        }
    }
}

fn tokenize_next_item<'a>(
    input: &mut &'a [&'a str],
) -> Result<Option<PredOrSyntax<'a>>, ParseError> {
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
            Err(_) => Err(ParseError(format!(
                "unexpected word '{orig_token}'; expected a test or action"
            ))),
            Ok(TokenType::Pred(pred_tok)) => match pred_tok.arity() {
                Arity::Zero => Ok(Some(PredOrSyntax::Predicate {
                    token_type: pred_tok,
                    arg: None,
                })),
                Arity::One => match pop_arg() {
                    Some(arg) => Ok(Some(PredOrSyntax::Predicate {
                        token_type: pred_tok,
                        arg: Some(arg),
                    })),
                    None => Err(ParseError(format!(
                        "predicate {orig_token} takes an argument but one was not specified"
                    ))),
                },
            },
            Ok(TokenType::Op(op)) => Ok(Some(PredOrSyntax::Op(op))),
            Ok(TokenType::Paren(side)) => Ok(Some(PredOrSyntax::Paren(side))),
        }
    } else {
        Ok(None) // end of input
    }
}

#[cfg(test)]
fn tokenize_single_predicate<'a>(
    mut input: &'a [&'a str],
) -> Result<Option<(PredicateToken, Option<&'a str>)>, ParseError> {
    match tokenize_next_item(&mut input) {
        Err(e) => Err(e),
        Ok(Some(PredOrSyntax::Predicate { token_type, arg })) => {
            let result = Some((token_type, arg));
            // Check that we have exhausted the input.
            match tokenize_next_item(&mut input) {
                Err(e) => panic!("unexpected parse error {e}"),
                Ok(None) => (),
                Ok(Some(parsed)) => {
                    panic!("input should include only one item, but we saw {parsed:?} at the end");
                }
            }
            Ok(result)
        }
        Ok(None) => Ok(None),
        other => {
            panic!("input {input:?} should have been parsed as predicates only, but appears to contain {other:?}");
        }
    }
}

fn find_matching_parens(input: &[PredOrSyntax]) -> Result<Vec<Option<NonZeroUsize>>, ParseError> {
    let mut result = Vec::with_capacity(input.len());
    result.resize(input.len(), None);
    let mut last_pos_of_lparen_at_depth: Vec<usize> = Vec::new();
    for (i, terminal) in input.iter().enumerate() {
        match terminal.maybe_get_paren() {
            None => (),
            Some(Parenthesis::Left) => {
                last_pos_of_lparen_at_depth.push(i);
            }
            Some(Parenthesis::Right) => match last_pos_of_lparen_at_depth.pop() {
                Some(leftpos) => {
                    let distance = i - leftpos;
                    assert!(result[leftpos].is_none());
                    result[leftpos] = NonZeroUsize::new(distance);
                }
                None => {
                    return Err(ParseError("unbalances parentheses".to_string()));
                }
            },
        }
    }
    Ok(result)
}

fn just(t: PredicateToken, arg: Option<&str>) -> Result<Expression, ParseError> {
    fn mb<P: Predicate + Send + Sync>(p: P) -> BoxedPredicate {
        Box::new(p)
    }

    use PredicateToken::*;
    let b: BoxedPredicate = match (t, arg) {
        (False, None) => mb(FalsePredicate {}),
        (False, _) => unreachable!(),

        (Print, None) => mb(PrintPredicate::new()),
        (Print, _) => unreachable!(),

        (True, None) => mb(TruePredicate {}),
        (True, _) => unreachable!(),

        (Type, Some(arg)) => {
            let pred = TypePredicate::new(arg)?;
            mb(pred)
        }
        (Type, None) => unreachable!(),
    };
    Ok(Expression::Just(b))
}

fn make_default_print() -> Expression {
    Expression::Just(Box::new(PrintPredicate::new()))
}

fn tokenize_program<'a>(
    mut input: &'a [&'a str],
) -> Result<(&'a [&'a str], Vec<PredOrSyntax>), ParseError> {
    let original_input = input;
    let starting_points_end: usize = input
        .iter()
        .take_while(|item| !item.starts_with('-'))
        .count();
    let starting_points = &original_input[0..starting_points_end];
    let mut program_tokens = &original_input[starting_points_end..];
    let mut predicates: Vec<PredOrSyntax> = Vec::with_capacity(input.len());
    while let Some(item) = tokenize_next_item(&mut program_tokens)? {
        predicates.push(item);
    }
    Ok((starting_points, predicates))
}

fn reduce_paren_expressions<'a>(
    _input: VecDeque<PartialNoPredicates>,
) -> Result<VecDeque<PartialNoParens>, ParseError> {
    todo!("write reduce_paren_expressions")
}

#[derive(Debug)]
enum PartialNoParens {
    Expr(Expression),
    Op(OperatorToken), // binary operator or a not operator
}

#[derive(Debug)]
enum PartialBinaryOpOnly {
    Expr(Expression),
    Op(BinaryOperationKind),
    // Not operators are not allowed
}

#[derive(Debug)]
enum PartialOrCommaOnly {
    Expr(Expression),
    // And operators are not allowed
    Or,
    Comma,
}

#[derive(Debug)]
enum PartialCommaOnly {
    Expr(Expression),
    // And and Or operators are not allowed
    Comma,
}

fn reduce_not_ops<'a>(
    mut input: VecDeque<PartialNoParens>,
) -> Result<VecDeque<PartialBinaryOpOnly>, ParseError> {
    let mut output = VecDeque::with_capacity(input.len());
    while let Some(item) = input.pop_front() {
        match item {
            PartialNoParens::Expr(expression) => {
                output.push_back(PartialBinaryOpOnly::Expr(expression));
            }
            PartialNoParens::Op(OperatorToken::Not) => {
                let mut invert = true;
                'count_nots: loop {
                    match input.pop_front() {
                        Some(PartialNoParens::Expr(expression)) => {
                            let expr = if invert {
                                Expression::Not(Box::new(expression))
                            } else {
                                expression
                            };
                            output.push_back(PartialBinaryOpOnly::Expr(expr));
                            break 'count_nots; // Break out of innermost loop only
                        }
                        Some(PartialNoParens::Op(OperatorToken::Not)) => {
                            invert = !invert;
                        }
                        Some(PartialNoParens::Op(op)) => {
                            // ! followed by a binary operator
                            return Err(ParseError(format!("The sequence ! {op} is not valid")));
                        }
                        None => {
                            return Err(ParseError(format!(
                                "! must not appear at the end of an expression"
                            )));
                        }
                    }
                }
            }
            PartialNoParens::Op(OperatorToken::Binary(binop_token)) => {
                output.push_back(PartialBinaryOpOnly::Op(binop_token));
            }
        }
    }
    Ok(output)
}

fn reduce_and_ops<'a>(
    _input: VecDeque<PartialBinaryOpOnly>,
) -> Result<VecDeque<PartialOrCommaOnly>, ParseError> {
    todo!("write reduce_and_ops")
}

fn reduce_or_ops<'a>(
    _input: VecDeque<PartialOrCommaOnly>,
) -> Result<VecDeque<PartialCommaOnly>, ParseError> {
    todo!("write reduce_or_ops")
}

fn reduce_comma_ops<'a>(_input: VecDeque<PartialCommaOnly>) -> Result<Expression, ParseError> {
    todo!("write reduce_comma_ops")
}

#[derive(Debug)]
enum PartialNoPredicates {
    Expr(Expression),
    Op(OperatorToken), // binary operator or a not operator
    Lparen,
    Rparen,
}

fn reduce_predicates<'a>(
    _input: &[PredOrSyntax<'a>],
) -> Result<VecDeque<PartialNoPredicates>, ParseError> {
    todo!("write reduce_predicates")
}

fn fully_parse<'a>(input: &[PredOrSyntax<'a>]) -> Result<Expression, ParseError> {
    reduce_predicates(input)
        .and_then(reduce_paren_expressions)
        .and_then(reduce_not_ops)
        .and_then(reduce_and_ops)
        .and_then(reduce_or_ops)
        .and_then(reduce_comma_ops)
}

pub fn parse_program<'a>(input: &'a [&'a str]) -> Result<(&'a [&'a str], Expression), ParseError> {
    tokenize_program(input).and_then(|(starting_points, predicates)| {
        let expr = fully_parse(&predicates)?;
        Ok((starting_points, expr))
    })
}
