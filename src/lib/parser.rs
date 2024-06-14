#[cfg(test)]
mod tests;

use std::collections::BTreeMap;
use std::collections::VecDeque;

use std::fmt::Display;
use std::num::NonZeroUsize;

use crate::ast::BinaryOperation;

use super::ast::BoxedPredicate;
use super::ast::{BinaryOperationKind, Expression, Predicate};
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

fn find_matching_parens(
    input: &[PartialNoPredicates],
) -> Result<BTreeMap<usize, NonZeroUsize>, ParseError> {
    let mut result: BTreeMap<usize, NonZeroUsize> = BTreeMap::new();
    let mut last_pos_of_lparen_at_depth: Vec<usize> = Vec::new();
    for (i, terminal) in input.iter().enumerate() {
        match terminal.maybe_get_paren() {
            None => (),
            Some(Parenthesis::Left) => {
                last_pos_of_lparen_at_depth.push(i);
            }
            Some(Parenthesis::Right) => match last_pos_of_lparen_at_depth.pop() {
                Some(leftpos) => match NonZeroUsize::new(i - leftpos) {
                    Some(value) => {
                        let previous = result.insert(leftpos, value);
                        assert!(previous.is_none());
                    }
                    None => {
                        unreachable!("internal error: a parenthesis cannot match itself");
                    }
                },
                None => {
                    return Err(ParseError("unbalanced parentheses".to_string()));
                }
            },
        }
    }
    Ok(result)
}

#[derive(Debug)]
struct ParenMap {
    map: BTreeMap<usize, NonZeroUsize>,
}

impl ParenMap {
    fn new(input: &[PartialNoPredicates]) -> Result<ParenMap, ParseError> {
        Ok(ParenMap {
            map: find_matching_parens(input)?,
        })
    }

    fn lookup(&self, origin: usize, from: usize) -> Option<NonZeroUsize> {
        let key: usize = origin + from;
        self.map.get(&key).copied()
    }
}

#[derive(Debug)]
enum PartialNoParens {
    Expr(Expression),
    Op(OperatorToken), // binary operator or a not operator
}

fn reduce_paren_expressions<'a>(
    mut input: VecDeque<PartialNoPredicates>,
    paren_map_origin: usize,
    paren_map: &ParenMap,
) -> Result<VecDeque<PartialNoParens>, ParseError> {
    let mut output: VecDeque<PartialNoParens> = VecDeque::with_capacity(input.len());
    let mut pos_in_paren_map = 0;
    while let Some(item) = input.pop_front() {
        match item {
            PartialNoPredicates::Expr(e) => {
                output.push_back(PartialNoParens::Expr(e));
            }
            PartialNoPredicates::Op(op) => {
                output.push_back(PartialNoParens::Op(op));
            }
            PartialNoPredicates::Lparen => {
                match paren_map
                    .lookup(paren_map_origin, pos_in_paren_map)
                    .map(|pos| pos.get())
                {
                    None => {
                        return Err(ParseError("unmatched '('".to_string()));
                    }
                    Some(rparen_pos) => {
                        let mut tail = input.split_off(rparen_pos);
                        // tail starts at the right paren; discard that.
                        match tail.pop_front() {
                            Some(PartialNoPredicates::Rparen) => (),
                            Some(other) => {
                                unreachable!("epected ')', got {other:?}");
                            }
                            None => {
                                unreachable!(
                                    "tried to consume a token from beyond the end of the input."
                                );
                            }
                        }
                        // This is a recursive call (because
                        // parse_expression is our caller), but we
                        // recurse on a smaller slice of the overall
                        // expression, so the recursion must
                        // terminate.
                        let consumed = input.len();
                        let expr = parse_expression(
                            input,
                            paren_map_origin + pos_in_paren_map,
                            paren_map,
                        )?;
                        input = tail;
                        pos_in_paren_map += consumed;
                        output.push_back(PartialNoParens::Expr(expr));
                    }
                }
            }
            PartialNoPredicates::Rparen => {
                return Err(ParseError(
                    "found right-parenthesis '(' without previous balancing left-parenthesis '('"
                        .to_string(),
                ));
            }
        }
        pos_in_paren_map += 1;
    }
    Ok(output)
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

fn reduce_comma_ops<'a>(mut input: VecDeque<PartialCommaOnly>) -> Result<Expression, ParseError> {
    let mut expressions: Vec<Expression> = Vec::with_capacity(input.len());
    let mut at_comma = false;
    while let Some(item) = input.pop_front() {
        match item {
            PartialCommaOnly::Expr(expression) => {
                if expressions.is_empty() {
                    expressions.push(expression);
                } else if !at_comma {
                    unreachable!(
                        "implicit '-and' should have been parsed before we get to this point"
                    );
                } else {
                    expressions.push(expression);
                }
                at_comma = false;
            }
            PartialCommaOnly::Comma => {
                if at_comma {
                    return Err(ParseError(
                        "commas must have an expression in between them".to_string(),
                    ));
                } else if expressions.is_empty() {
                    return Err(ParseError(
                        "expressions cannot start with a comma".to_string(),
                    ));
                } else {
                    at_comma = true;
                }
            }
        }
    }
    if at_comma {
        return Err(ParseError(
            "expressions cannot end with a comma".to_string(),
        ));
    }

    fn build_comma_or_direct_expression(mut children: Vec<Expression>) -> Option<Expression> {
        children.pop().map(|first| {
            if children.is_empty() {
                first
            } else {
                let mut result = Vec::with_capacity(children.len() + 1);
                result.push(first);
                result.extend(children.drain(0..));
                Expression::BinaryOp(BinaryOperation::new(BinaryOperationKind::Comma, result))
            }
        })
    }

    Ok(build_comma_or_direct_expression(expressions).unwrap_or_else(make_default_print))
}

#[derive(Debug)]
enum PartialNoPredicates {
    Expr(Expression),
    Op(OperatorToken), // binary operator or a not operator
    Lparen,
    Rparen,
}

impl PartialNoPredicates {
    fn maybe_get_paren(&self) -> Option<Parenthesis> {
        match self {
            PartialNoPredicates::Lparen => Some(Parenthesis::Left),
            PartialNoPredicates::Rparen => Some(Parenthesis::Right),
            _ => None,
        }
    }
}

// parse expression parses the expression as a whole and, recursively,
// the contents of parenthesised expressions.
fn parse_expression(
    input: VecDeque<PartialNoPredicates>,
    paren_map_origin: usize,
    paren_map: &ParenMap,
) -> Result<Expression, ParseError> {
    reduce_paren_expressions(input, paren_map_origin, paren_map)
        .and_then(reduce_not_ops)
        .and_then(reduce_and_ops)
        .and_then(reduce_or_ops)
        .and_then(reduce_comma_ops)
}

pub fn parse_program<'a>(input: &'a [&'a str]) -> Result<(&'a [&'a str], Expression), ParseError> {
    fn tokenize_program<'a>(
        input: &'a [&'a str],
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

    fn reduce_predicates<'a>(
        _input: &[PredOrSyntax<'a>],
    ) -> Result<Vec<PartialNoPredicates>, ParseError> {
        todo!("write reduce_predicates")
    }

    // Separate out the initial arguments representing the starting
    // points, and turn the remainder of the input words into tokens.
    // The tokenization process will group words together where they
    // are arguments belonging to a leading token.
    let (starting_points, predicates) = tokenize_program(input)?;

    // Convert the tokens-with-arguments into a sequence of predicates
    // (representing things like "-type f") and punctuation
    // (parentheses and unary or binary operators).
    let no_predicates = reduce_predicates(&predicates)?;

    // For each open parenthesis, locate the matching close-parenthesis.
    let paren_map = ParenMap::new(&no_predicates)?;

    // Convert no_predicates from a Vec (required by ParenMap::new) to
    // a VecDeque (required by the remaining parse steps).
    let no_predicates: VecDeque<PartialNoPredicates> = no_predicates.into_iter().collect();

    // Parse the program into a single expression and return it, with
    // the starting points.
    let expr: Expression = parse_expression(no_predicates, 0, &paren_map)?;
    Ok((starting_points, expr))
}
