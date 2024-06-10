#[cfg(test)]
mod tests;

use std::collections::VecDeque;
use std::fmt::Display;
use std::num::NonZeroUsize;

#[cfg(test)]
use super::ast::BoxedPredicate;
use super::ast::{BinaryOperation, BinaryOperationKind, Expression, Predicate};
use super::errors::ParseError;
use super::predicate::*;

use enum_iterator::Sequence;

#[derive(Debug, Eq, PartialEq, Clone, Copy, PartialOrd, Ord)]
enum Precedence {
    Comma, // lowest
    Or,
    And,
    Not, // highest
}

fn binary_operation_precedence(op: BinaryOperationKind) -> Precedence {
    match op {
        BinaryOperationKind::And => Precedence::And,
        BinaryOperationKind::Or => Precedence::Or,
        BinaryOperationKind::KeepLast => Precedence::Comma,
    }
}

fn operator_precedence(tok: &OperatorToken) -> Precedence {
    match tok {
        OperatorToken::And => Precedence::And,
        OperatorToken::Or => Precedence::Or,
        OperatorToken::Not => Precedence::Not,
        OperatorToken::Comma => Precedence::Comma,
    }
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

#[derive(Debug, Eq, PartialEq, Clone, Copy, Sequence, Hash)]
enum OperatorToken {
    And,
    Or,
    Not,
    Comma,
}

impl Display for OperatorToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use OperatorToken::*;
        f.write_str(
            // While we accept both -and and -a, we use the POSIX
            // compliant representation for output.  Similarly for
            // -o/-or, !/-not.
            match self {
                And => "-a",
                Or => "-o",
                Not => "!",
                Comma => ",",
            },
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Sequence, Hash)]
enum Parenthesis {
    Left,
    Right,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Sequence, Hash)]
enum TokenType {
    Pred(PredicateToken),
    Op(OperatorToken),
    Paren(Parenthesis),
}

fn tokenize_word(s: &str) -> Result<TokenType, ParseError> {
    match s {
        "-print" => Ok(TokenType::Pred(PredicateToken::Print)),
        "-true" => Ok(TokenType::Pred(PredicateToken::True)),
        "-false" => Ok(TokenType::Pred(PredicateToken::False)),
        "-type" => Ok(TokenType::Pred(PredicateToken::Type)),
        "(" => Ok(TokenType::Paren(Parenthesis::Left)),
        ")" => Ok(TokenType::Paren(Parenthesis::Right)),
        "!" | "-not" => Ok(TokenType::Op(OperatorToken::Not)),
        "-and" | "-a" => Ok(TokenType::Op(OperatorToken::And)),
        "-or" | "-o" => Ok(TokenType::Op(OperatorToken::Or)),
        "," => Ok(TokenType::Op(OperatorToken::Comma)),
        _ => Err(ParseError(format!("unknown token {s}"))),
    }
}

fn build_zero_arg_predicate(
    pred_token_type: PredicateToken,
    orig_token: &str,
) -> Result<Box<dyn Predicate + Send + Sync>, ParseError> {
    match pred_token_type {
	PredicateToken::Print => Ok(Box::new(PrintPredicate::new())),
	PredicateToken::True => Ok(Box::new(TruePredicate{})),
	PredicateToken::False => Ok(Box::new(FalsePredicate{})),
	PredicateToken::Type => unreachable!(
            "build_zero_arg_predicate called on {pred_token_type:?} token {orig_token} having non-zero arity"
        ),
    }
}

fn build_one_arg_predicate(
    pred_token_type: PredicateToken,
    orig_token: &str,
    arg: &str,
) -> Result<Box<dyn Predicate + Send + Sync>, ParseError> {
    match pred_token_type {
        PredicateToken::Type => Ok(Box::new(TypePredicate::new(arg)?)),
        PredicateToken::Print | PredicateToken::False | PredicateToken::True => unreachable!(
	    "build_one_arg_predicate called on {pred_token_type:?} token {orig_token} having arity other than 1"
	),
    }
}

#[derive(Debug)]
enum PredOrSyntax {
    Predicate {
        token_type: PredicateToken, // used only in tests
        predicate: Box<dyn Predicate + Send + Sync>,
    },
    Op(OperatorToken),
    Paren(Parenthesis),
}

impl PredOrSyntax {
    fn maybe_get_paren(&self) -> Option<&Parenthesis> {
        if let PredOrSyntax::Paren(paren_type) = self {
            Some(paren_type)
        } else {
            None
        }
    }
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
            Ok(TokenType::Pred(pred_tok)) => match pred_tok.arity() {
                Arity::Zero => Ok(Some(ParsedItem::Program(PredOrSyntax::Predicate {
                    token_type: pred_tok,
                    predicate: build_zero_arg_predicate(pred_tok, orig_token)?,
                }))),
                Arity::One => match input.pop_front() {
                    Some(arg) => Ok(Some(ParsedItem::Program(PredOrSyntax::Predicate {
                        token_type: pred_tok,
                        predicate: build_one_arg_predicate(pred_tok, orig_token, arg)?,
                    }))),
                    None => Err(ParseError(format!(
                        "predicate {orig_token} takes an argument but one was not specified"
                    ))),
                },
            },
            Ok(TokenType::Op(op)) => Ok(Some(ParsedItem::Program(PredOrSyntax::Op(op)))),
            Ok(TokenType::Paren(side)) => Ok(Some(ParsedItem::Program(PredOrSyntax::Paren(side)))),
        }
    } else {
        Ok(None) // end of input
    }
}

#[cfg(test)]
fn parse_single_predicate(
    input: &[&str],
) -> Result<Option<(PredicateToken, Box<dyn Predicate + Send + Sync>)>, ParseError> {
    let mut queue: VecDeque<&str> = input.into_iter().copied().collect();
    match parse_next_item(&mut queue, true) {
        Err(e) => Err(e),
        Ok(Some(ParsedItem::Program(PredOrSyntax::Predicate {
            token_type,
            predicate,
        }))) => {
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
        other => {
            panic!("input {input:?} should have been parsed as predicates only, but appears to contain {other:?}");
        }
    }
}

#[derive(Debug)]
struct ParseInputItem {
    terminal: PredOrSyntax,
    balancing_paren_dist: Option<NonZeroUsize>,
}

impl ParseInputItem {
    fn terminal(&self) -> &PredOrSyntax {
        &self.terminal
    }

    fn take_terminal(self) -> PredOrSyntax {
        self.terminal
    }

    fn balancing_paren_dist(&self) -> Option<NonZeroUsize> {
        self.balancing_paren_dist
    }
}

#[derive(Debug)]
struct ParseInput {
    items: VecDeque<ParseInputItem>,
}

impl ParseInput {
    pub fn new(terminals: VecDeque<PredOrSyntax>) -> Result<ParseInput, ParseError> {
        let paren_offsets = ParseInput::find_matching_parens(&terminals)?;
        Ok(ParseInput {
            items: terminals
                .into_iter()
                .zip(paren_offsets.into_iter())
                .map(|(terminal, offset)| ParseInputItem {
                    terminal,
                    balancing_paren_dist: offset,
                })
                .collect(),
        })
    }

    pub fn find_matching_parens(
        input: &VecDeque<PredOrSyntax>,
    ) -> Result<Vec<Option<NonZeroUsize>>, ParseError> {
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

    pub fn iter(&self) -> impl Iterator<Item = &ParseInputItem> {
        self.items.iter()
    }

    pub fn shift(&mut self) -> Option<ParseInputItem> {
        self.items.pop_front()
    }

    pub fn shift_many(&mut self, n: usize) -> ParseInput {
        ParseInput {
            items: self.items.drain(0..n).collect(),
        }
    }

    pub fn unshift(&mut self, item: ParseInputItem) {
        self.items.push_front(item)
    }

    pub fn shift_binary_operation(
        &mut self,
    ) -> Option<(bool, ParseInputItem, BinaryOperationKind)> {
        self.shift().map(|item| match item.terminal() {
            PredOrSyntax::Op(OperatorToken::And) => (true, item, BinaryOperationKind::And),
            PredOrSyntax::Op(OperatorToken::Or) => (true, item, BinaryOperationKind::Or),
            PredOrSyntax::Op(OperatorToken::Comma) => (true, item, BinaryOperationKind::KeepLast),
            _ => {
                // The next token wasn't an operator (e.g. it was (,
                // -print, etc.).  Unshift the next token and return
                // an implicit "and" so that (as POSIX requires)
                // "-type f -name foo" means the same as "-type f -a
                // -name foo".
                self.unshift(item);
                let implicit = ParseInputItem {
                    terminal: PredOrSyntax::Op(OperatorToken::And),
                    // Since this is not a paren, the distance to the
                    // balancing paren is None.
                    balancing_paren_dist: None,
                };
                (false, implicit, BinaryOperationKind::And)
            }
        })
    }
}

/// Returns a parenthesis expression.  The left paren has already been consumed.
fn get_paren_expression(
    input: &mut ParseInput,
    rparen_dist: NonZeroUsize,
) -> Result<Expression, ParseError> {
    match rparen_dist.get() {
        0 => unreachable!("get_paren_expression should be called only on parens which balance"),
        1 => Err(ParseError("empty parentheses".to_string())),
        dist => {
            // Extract the whole interior of the parenthesised
            // expression.  We already consumed the left parenthesis,
            // so for "( -print )" dist is 2 but we only want to shift
            // 1 token to form the inner expression.
            let mut head: ParseInput = input.shift_many(dist - 1);

            // The next unshifted token must be the balancing
            // parenthesis.  Remove it.
            match input.shift().take().map(|item| item.take_terminal()) {
                Some(PredOrSyntax::Paren(Parenthesis::Right)) => (),
                None => {
                    unreachable!("right parenthesis is unexpectedly missing");
                }
                other => {
                    unreachable!("the end of the parenthesised expression isn't a right parenthesis: {other:?}");
                }
            }

            match get_expression(&mut head, None) {
                Ok(Some(expr)) => Ok(expr),
                Ok(None) => Err(ParseError("empty parentheses".to_string())),
                Err(e) => Err(e),
            }
        }
    }
}

fn get_expression(
    input: &mut ParseInput,
    pred_bound: Option<Precedence>,
) -> Result<Option<Expression>, ParseError> {
    // Shift a token from the input and form an expression (which may
    // end up as the left-hand-side of a binary expressino, or may be
    // the only expression).
    let left: Expression = match input.shift() {
        None => {
            return Ok(None);
        }
        Some(item) => {
            let paren_dist = item.balancing_paren_dist();
            match item.take_terminal() {
                PredOrSyntax::Op(op) => {
                    match op {
                        OperatorToken::And | OperatorToken::Or | OperatorToken::Comma => {
                            return Err(ParseError(format!(
                                "unexpected operator token {op}, expected a predicate"
                            )));
                        }
                        OperatorToken::Not => {
                            // This could be "! -type d" or "! ( ... "
                            // so we need to parse the next thing as an
                            // expression, not simply a token.
                            match get_expression(input, Some(Precedence::Not))? {
                                Some(inverted) => Expression::Not(Box::new(inverted)),
                                None => {
                                    return Err(ParseError(
                                        "incomplete expression: '!' at end of expression"
                                            .to_string(),
                                    ));
                                }
                            }
                        }
                    }
                }
                PredOrSyntax::Paren(Parenthesis::Left) => {
                    match paren_dist {
                        None => {
                            unreachable!("ParseInput should contain only expressions with balanced parentheses");
                        }
                        Some(dist) => {
                            return get_paren_expression(input, dist).map(|expr| Some(expr));
                        }
                    }
                }
                PredOrSyntax::Paren(Parenthesis::Right) => {
                    unreachable!(
                        "ParseInput should contain only expressions with balanced parentheses"
                    );
                }
                PredOrSyntax::Predicate {
                    token_type: _,
                    predicate,
                } => Expression::Just(predicate),
            }
        }
    };

    // Shift a token to identify whether we're looking at a binary
    // expression.   Use the precdence bound to ensure that
    // "x -a y -o b" is parsed as "( x -a y ) -o b".
    let (explicit, kind) = match input.shift_binary_operation() {
        None => {
            // We reached end-of-input, or the end of a parenthesised
            // expression.
            return Ok(Some(left));
        }
        Some((explicit, item, kind)) => {
            if let Some(bound) = pred_bound {
                if binary_operation_precedence(kind) < bound {
                    if !explicit {
                        input.unshift(item);
                    }
                    // The next operator has lower precedence, so this
                    // expression ends here.
                    return Ok(Some(left));
                } else {
                    (explicit, kind)
                }
            } else {
                (explicit, kind)
            }
        }
    };

    // We have a binary expression, parse the expression forming the
    // right-hand-side.
    if let Some(right) = get_expression(input, Some(binary_operation_precedence(kind)))? {
        Ok(Some(Expression::BinaryOp(BinaryOperation::new(
            kind,
            vec![left, right],
        ))))
    } else {
        // We had a binary operation but no expression on its
        // right-hand-side.  This is a syntax error.
        assert!(explicit);
        let symbol = match kind {
            BinaryOperationKind::And => "-a",
            BinaryOperationKind::Or => "-o",
            BinaryOperationKind::KeepLast => ",",
        };
        Err(ParseError(format!(
            "binary operator '{symbol}' has no right-hand operand"
        )))
    }
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
                let mut unparsed_program = ParseInput::new(predicates)?;
                // TODO: make get_expression take ownership of the input.
                let program: Expression =
                    get_expression(&mut unparsed_program, None)?.unwrap_or_else(make_default_print);
                return Ok((starting_points, program));
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
