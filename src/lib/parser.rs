#[cfg(test)]
mod tests;

use std::collections::VecDeque;

use std::fmt::Display;

use crate::ast::BinaryOperation;

use super::ast::BoxedPredicate;
use super::ast::{BinaryOperationKind, Expression, Predicate};
use super::errors::ParseError;
use super::options::{DebugOption, GlobalOption, Options};
use super::predicate::*;

use enum_iterator::Sequence;

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
    GlobalOpt(GlobalOption),
}

fn tokenize_word(s: &str) -> Result<TokenType, ParseError> {
    use BinaryOperationKind::*;
    match s {
        "-depth" => Ok(TokenType::GlobalOpt(GlobalOption::DepthFirst)),
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
    GlobalOpt(GlobalOption),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CommandLineItem<'a> {
    StartingPoint(&'a str),
    Code(PredOrSyntax<'a>),
    GlobalOption(GlobalOption),
}

fn tokenize_next_item<'a>(
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
            Ok(TokenType::GlobalOpt(option)) => Ok(Some(CommandLineItem::GlobalOption(option))),
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
        Ok(Some(CommandLineItem::Code(PredOrSyntax::Predicate { token_type, arg }))) => {
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
        Ok(Some(CommandLineItem::StartingPoint(here))) => {
            panic!("input token {here:?} could not be parsed as a predicate");
        }
        Ok(None) => Ok(None),
        other => {
            panic!("input {input:?} should have been parsed as predicates only, but appears to contain {other:?}");
        }
    }
}

fn global_option_placeholder(opt: GlobalOption) -> Expression {
    Expression::Just(Box::new(GlobalOptionPlaceholder::new(opt)))
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

#[derive(Debug)]
enum PartialNoParens {
    Expr(Expression),
    Op(OperatorToken), // binary operator or a not operator
}

fn reduce_paren_expressions<'a>(
    mut input: VecDeque<PartialNoPredicates>,
) -> Result<VecDeque<PartialNoParens>, ParseError> {
    // The member of `output` at position i represents an incompletely
    // parsed parenthesis expression containing i open parentheses to
    // its left.
    let mut output: Vec<VecDeque<PartialNoParens>> = Vec::new();
    // Before we've started parsing anything, we have an empty
    // expression having 0 open parentheses to its left, so output
    // should have one member at position 0.
    output.push(VecDeque::new());

    // Shift items from the terminals in `input`.
    while let Some(item) = input.pop_front() {
        match item {
            PartialNoPredicates::Expr(expr) => {
                // Add this expression to the current
                // most-deeply-nested parenthesis expression.
                output
                    .last_mut()
                    .unwrap()
                    .push_back(PartialNoParens::Expr(expr));
            }
            PartialNoPredicates::Op(op) => {
                // Add this operator to the current
                // most-deeply-nested parenthesis expression.
                output
                    .last_mut()
                    .unwrap()
                    .push_back(PartialNoParens::Op(op));
            }
            PartialNoPredicates::Lparen => {
                // Open a deeper level of nesting.  The expression at
                // the end of `output` is still incomplete, and when
                // we see the ')' for this new parenthesis expression,
                // we will turn that into an expression and push it
                // onto the end of the expression which until just now
                // was the most deeply nested one.
                output.push(VecDeque::new());
            }
            PartialNoPredicates::Rparen => match output.pop() {
                // The expression we just popped off the back of the
                // stack is a completed parenthesis expression.
                Some(items) => {
                    if output.len() < 1 {
                        // This is not a valid state as there must
                        // always be a top-level expression.
                        return Err(ParseError("too many ')'".to_string()));
                    }
                    // Reduce the newly-parsed complete parentheised
                    // expression and push it onto the end of the
                    // still-open parenthesis expression which
                    // encloses it.
                    match parse_noparen_expression(items)? {
                        Some(expr) => {
                            output
                                .last_mut()
                                .unwrap()
                                .push_back(PartialNoParens::Expr(expr));
                        }
                        None => {
                            return Err(ParseError("empty parentheses".to_string()));
                        }
                    }
                }
                None => unreachable!(),
            },
        }
    }
    // There are no more items to shift.  If the parenthesised
    // expression is balanced, `output` will contain exactly one
    // member.
    if let Some(items) = output.pop() {
        if output.is_empty() {
            // It had contained one member so the expression must have
            // been balanced.
            Ok(items)
        } else {
            // We ran out of input, but there were not enough
            // right-parens to balance everthing.
            Err(ParseError("too many '('".to_string()))
        }
    } else {
        unreachable!()
    }
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
    mut input: VecDeque<PartialBinaryOpOnly>,
) -> Result<VecDeque<PartialOrCommaOnly>, ParseError> {
    let mut output: VecDeque<PartialOrCommaOnly> = VecDeque::with_capacity(input.len());
    let mut current_and_expression: Vec<Expression> = Vec::with_capacity(input.len());
    let mut at_operator = false;

    fn reduce_and_expression(
        mut and_expression: Vec<Expression>,
        output: &mut VecDeque<PartialOrCommaOnly>,
    ) -> Vec<Expression> {
        match and_expression.len() {
            0 | 1 => {
                if let Some(expr) = and_expression.pop() {
                    output.push_back(PartialOrCommaOnly::Expr(expr));
                }
                and_expression
            }
            _ => {
                output.push_back(PartialOrCommaOnly::Expr(Expression::BinaryOp(
                    BinaryOperation::new(BinaryOperationKind::And, and_expression),
                )));
                Vec::new()
            }
        }
    }

    while let Some(item) = input.pop_front() {
        match item {
            PartialBinaryOpOnly::Expr(expression) => {
                current_and_expression.push(expression);
                at_operator = false;
            }
            PartialBinaryOpOnly::Op(_) if at_operator => {
                return Err(ParseError(
                    "operators must have an expression in between them".to_string(),
                ));
            }
            PartialBinaryOpOnly::Op(op) => {
                // Decide between a reduce operation (if the operation
                // is -and) and a shift operation (for -or and ,).  If
                // we need to shift a lower-precedence operator, then
                // complete the reduction of any incomplete -and
                // expression first, and shift the reduced expression.
                let op_to_shift = match op {
                    BinaryOperationKind::And => {
                        if current_and_expression.is_empty() {
                            return Err(ParseError(
                                "expressions cannot start with -and".to_string(),
                            ));
                        }
                        None
                    }
                    BinaryOperationKind::Or => Some(PartialOrCommaOnly::Or),
                    BinaryOperationKind::Comma => Some(PartialOrCommaOnly::Comma),
                };
                if let Some(op) = op_to_shift {
                    if !current_and_expression.is_empty() {
                        current_and_expression =
                            reduce_and_expression(current_and_expression, &mut output);
                    }
                    output.push_back(op);
                }
                at_operator = true;
            }
        }
    }
    if at_operator {
        return Err(ParseError(
            "expressions cannot end with an operator".to_string(),
        ));
    }
    reduce_and_expression(current_and_expression, &mut output);
    Ok(output)
}

fn reduce_or_ops<'a>(
    mut input: VecDeque<PartialOrCommaOnly>,
) -> Result<VecDeque<PartialCommaOnly>, ParseError> {
    let mut output: VecDeque<PartialCommaOnly> = VecDeque::with_capacity(input.len());
    let mut current_or_expression: Vec<Expression> = Vec::with_capacity(input.len());
    let mut at_operator = false;

    fn shift_or_expression(
        mut or_expression: Vec<Expression>,
        output: &mut VecDeque<PartialCommaOnly>,
    ) -> Vec<Expression> {
        match or_expression.len() {
            0 | 1 => {
                if let Some(expr) = or_expression.pop() {
                    output.push_back(PartialCommaOnly::Expr(expr));
                }
                or_expression
            }
            _ => {
                output.push_back(PartialCommaOnly::Expr(Expression::BinaryOp(
                    BinaryOperation::new(BinaryOperationKind::Or, or_expression),
                )));
                Vec::new()
            }
        }
    }

    while let Some(item) = input.pop_front() {
        match item {
            PartialOrCommaOnly::Expr(expression) => {
                current_or_expression.push(expression);
                at_operator = false;
            }
            PartialOrCommaOnly::Comma | PartialOrCommaOnly::Or if at_operator => {
                return Err(ParseError(
                    "operators must have an expression in between them".to_string(),
                ));
            }
            PartialOrCommaOnly::Or => {
                if current_or_expression.is_empty() {
                    return Err(ParseError("expressions cannot start with -or".to_string()));
                } else {
                    at_operator = true;
                }
            }
            PartialOrCommaOnly::Comma => {
                if !current_or_expression.is_empty() {
                    current_or_expression = shift_or_expression(current_or_expression, &mut output);
                }
                output.push_back(PartialCommaOnly::Comma);
                at_operator = true;
            }
        }
    }
    if at_operator {
        return Err(ParseError(
            "expressions cannot end with an operator".to_string(),
        ));
    }
    shift_or_expression(current_or_expression, &mut output);
    Ok(output)
}

fn reduce_comma_ops<'a>(
    mut input: VecDeque<PartialCommaOnly>,
) -> Result<Option<Expression>, ParseError> {
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
    Ok(build_comma_or_direct_expression(expressions))
}

#[derive(Debug)]
enum PartialNoPredicates {
    Expr(Expression),
    Op(OperatorToken), // binary operator or a not operator
    Lparen,
    Rparen,
}

// parse expression parses the expression as a whole and, recursively,
// the contents of parenthesised expressions.
fn parse_expression(
    input: VecDeque<PartialNoPredicates>,
) -> Result<Option<Expression>, ParseError> {
    reduce_paren_expressions(input).and_then(parse_noparen_expression)
}

// parse expression parses the expression as a whole and, recursively,
// the contents of parenthesised expressions.
fn parse_noparen_expression(
    input: VecDeque<PartialNoParens>,
) -> Result<Option<Expression>, ParseError> {
    reduce_not_ops(input)
        .and_then(reduce_and_ops)
        .and_then(reduce_or_ops)
        .and_then(reduce_comma_ops)
}

fn reduce_predicates<'a>(
    input: &[PredOrSyntax<'a>],
) -> Result<Vec<PartialNoPredicates>, ParseError> {
    fn mapper(pred: &PredOrSyntax) -> Result<PartialNoPredicates, ParseError> {
        match pred {
            PredOrSyntax::Op(op) => Ok(PartialNoPredicates::Op(*op)),
            PredOrSyntax::Paren(Parenthesis::Left) => Ok(PartialNoPredicates::Lparen),
            PredOrSyntax::Paren(Parenthesis::Right) => Ok(PartialNoPredicates::Rparen),
            PredOrSyntax::Predicate { token_type, arg } => {
                Ok(PartialNoPredicates::Expr(just(*token_type, *arg)?))
            }
            PredOrSyntax::GlobalOpt(gopt) => {
                Ok(PartialNoPredicates::Expr(global_option_placeholder(*gopt)))
            }
        }
    }

    input
        .iter()
        .try_fold(Vec::with_capacity(input.len()), |mut acc, item| {
            acc.push(mapper(item)?);
            Ok(acc)
        })
}

pub fn parse_program<'a, 'b>(
    input: &'a [&'a str],
    options: &'b mut Options,
) -> Result<(&'a [&'a str], Expression), ParseError> {
    fn tokenize_program<'a, 'b>(
        mut input: &'a [&'a str],
        options: &'b mut Options,
    ) -> Result<(&'a [&'a str], Vec<PredOrSyntax<'a>>), ParseError> {
        let orig_input = input;
        let mut predicates: Vec<PredOrSyntax> = Vec::with_capacity(input.len());
        let mut end_of_starting_points: usize = 0;
        while let Some(item) = tokenize_next_item(&mut input)? {
            match item {
                CommandLineItem::StartingPoint(arg) => {
                    if predicates.is_empty() {
                        end_of_starting_points += 1;
                    } else {
                        return Err(ParseError(format!("{arg} is not a valid predicate")));
                    }
                }
                CommandLineItem::GlobalOption(option) => {
                    options.apply(&option);
                    predicates.push(PredOrSyntax::GlobalOpt(option));
                }
                CommandLineItem::Code(pred) => {
                    predicates.push(pred);
                }
            }
        }
        let starting_points = &orig_input[0..end_of_starting_points];
        Ok((starting_points, predicates))
    }

    // Separate out the initial arguments representing the starting
    // points, and turn the remainder of the input words into tokens.
    // The tokenization process will group words together where they
    // are arguments belonging to a leading token.
    let (starting_points, predicates) = tokenize_program(input, options)?;

    // Convert the tokens-with-arguments into a sequence of predicates
    // (representing things like "-type f") and punctuation
    // (parentheses and unary or binary operators).
    let no_predicates = reduce_predicates(&predicates)?;

    // Convert no_predicates from a Vec (required by ParenMap::new) to
    // a VecDeque (required by the remaining parse steps).
    let no_predicates: VecDeque<PartialNoPredicates> = no_predicates.into_iter().collect();

    // Parse the program into a single expression and return it, with
    // the starting points.
    let expr: Expression = match parse_expression(no_predicates)? {
        None => make_default_print(),
        Some(expr) => {
            if expr.inhibits_default_print() {
                expr
            } else {
                Expression::BinaryOp(BinaryOperation::new(
                    BinaryOperationKind::And,
                    vec![expr, make_default_print()],
                ))
            }
        }
    };

    if options.get_debug_option(&DebugOption::Tree) {
        eprintln!("Parsed expression:\n{expr:?}");
    }
    Ok((starting_points, expr))
}
