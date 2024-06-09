#[cfg(test)]
use std::collections::HashSet;
use std::collections::VecDeque;
use std::fmt::Display;

#[cfg(test)]
use super::ast::BoxedPredicate;
use super::ast::{BinaryOperation, BinaryOperationKind, Expression, Predicate};
use super::errors::ParseError;
use super::predicate::*;

#[cfg(test)]
use enum_iterator::all;
use enum_iterator::Sequence;

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

fn binary_operation_precedence(op: BinaryOperationKind) -> Precedence {
    match op {
        BinaryOperationKind::And => Precedence::And,
        BinaryOperationKind::Or => Precedence::Or,
        BinaryOperationKind::KeepLast => Precedence::Comma,
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
    Print,
    Type,
}

impl PredicateToken {
    pub fn arity(&self) -> Arity {
        use Arity::*;
        use PredicateToken::*;
        match self {
            Print => Zero,
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

#[test]
fn test_tokenize_word() {
    let toks: Result<Vec<TokenType>, ParseError> = [].into_iter().map(tokenize_word).collect();
    assert_eq!(toks.expect("tokens are valid"), []);
}

fn build_zero_arg_predicate(
    pred_token_type: PredicateToken,
    orig_token: &str,
) -> Result<Box<dyn Predicate + Send + Sync>, ParseError> {
    match pred_token_type {
	PredicateToken::Print => Ok(Box::new(PrintPredicate::new())),
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
        PredicateToken::Print => unreachable!(
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

#[test]
fn test_parse_next_item_exhaustive_for_predicates() {
    fn do_valid_parse(input: &[&str]) -> Option<(PredicateToken, BoxedPredicate)> {
        match parse_single_predicate(input) {
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
            Some((pred_token_type, _predicate)) => {
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

#[test]
fn test_parse_type_invalid() {
    let test_inputs: Vec<Vec<&str>> = vec![
        vec!["-type"],       // invalid because missing argument
        vec!["-typeZ", "f"], // invalid because not actually -type
        vec!["-type", "q"],  // invalid argument
    ];
    for input in test_inputs {
        match parse_single_predicate(input.as_slice()) {
            Err(_) => (), // as excpted
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
    for (letter, _expected_indicator) in test_inputs {
        let input = &["-type", letter];
        match parse_single_predicate(input) {
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

struct ParseInput {
    terminals: VecDeque<PredOrSyntax>,
}

impl ParseInput {
    pub fn new(terminals: VecDeque<PredOrSyntax>) -> ParseInput {
        ParseInput { terminals }
    }

    pub fn shift(&mut self) -> Option<PredOrSyntax> {
        self.terminals.pop_front()
    }

    pub fn unshift(&mut self, item: PredOrSyntax) {
        self.terminals.push_front(item)
    }

    pub fn shift_binary_operation(&mut self) -> Option<(bool, BinaryOperationKind)> {
        self.shift().map(|token| match token {
            PredOrSyntax::Op(OperatorToken::And) => (true, BinaryOperationKind::And),
            PredOrSyntax::Op(OperatorToken::Or) => (true, BinaryOperationKind::Or),
            PredOrSyntax::Op(OperatorToken::Comma) => (true, BinaryOperationKind::KeepLast),
            other => {
                // The next token wasn't an operator (e.g. it was (,
                // -print, etc.).  Unshift the next token and return
                // an implicit "and" so that (as POSIX requires)
                // "-type f -name foo" means the same as "-type f -a
                // -name foo".
                self.unshift(other);
                (false, BinaryOperationKind::And)
            }
        })
    }

    pub fn unshift_binary_operation(&mut self, op: BinaryOperationKind) {
        self.unshift(match op {
            BinaryOperationKind::And => PredOrSyntax::Op(OperatorToken::And),
            BinaryOperationKind::Or => PredOrSyntax::Op(OperatorToken::Or),
            BinaryOperationKind::KeepLast => PredOrSyntax::Op(OperatorToken::Comma),
        });
    }
}

fn get_paren_expression(_input: &mut ParseInput) -> Result<Option<Expression>, ParseError> {
    todo!("implement get_paren_expression")
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
        Some(PredOrSyntax::Op(op)) => {
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
                                "incomplete expression: '!' at end of expression".to_string(),
                            ));
                        }
                    }
                }
            }
        }
        Some(PredOrSyntax::Paren(Parenthesis::Left)) => return get_paren_expression(input),
        Some(PredOrSyntax::Paren(Parenthesis::Right)) => {
            return Err(ParseError(
                "found unexpected closing parenthesis at the beginning of an expression"
                    .to_string(),
            ))
        }
        Some(PredOrSyntax::Predicate {
            token_type: _,
            predicate,
        }) => Expression::Just(predicate),
    };

    // Shift a token to identify whether we're looking at a binary
    // expression.   Use the precdence bound to ensure that
    // "x -a y -o b" is parsed as "( x -a y ) -o b".
    let (explicit, op) = match input.shift_binary_operation() {
        Some((explicit, op)) => {
            if let Some(bound) = pred_bound {
                if binary_operation_precedence(op) < bound {
                    if !explicit {
                        input.unshift_binary_operation(op);
                    }
                    (explicit, None)
                } else {
                    (explicit, Some(op))
                }
            } else {
                (explicit, Some(op))
            }
        }
        None => (false, None),
    };

    if let Some(op) = op {
        // If we have a binary expression, parse the expression
        // forming the right-hand-side.
        if let Some(right) = get_expression(input, Some(binary_operation_precedence(op)))? {
            Ok(Some(Expression::BinaryOp(BinaryOperation::new(
                op,
                vec![left, right],
            ))))
        } else {
            // We had a binary operation but no expression on its
            // right-hand-side.  This is a syntax error.
            assert!(explicit);
            let symbol = match op {
                BinaryOperationKind::And => "-a",
                BinaryOperationKind::Or => "-o",
                BinaryOperationKind::KeepLast => ",",
            };
            Err(ParseError(format!(
                "binary operator '{symbol}' has no right-hand operand"
            )))
        }
    } else {
        // There was no explicit or implicit operator and thus the
        // "left" expression is in fact the only expression.
        Ok(Some(left))
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
                let mut unparsed_program = ParseInput::new(predicates);
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
