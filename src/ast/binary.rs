use crate::lexer::{Token, TokenStream};

use super::Expression;

#[derive(Debug, PartialEq)]
pub struct Binary {
    pub left: Box<Expression>,
    pub op: BinaryOp,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
}

pub fn parse_binary(stream: &mut TokenStream) -> Expression {
    parse_assignment(stream)
}

fn parse_assignment(stream: &mut TokenStream) -> Expression {
    let left = parse_or(stream);
    let Token::Eq = stream.peek() else {
        return left;
    };
    let right = parse_or(stream);

    let Expression::Identifier(left) = left else {
        panic!("expected identifier on left side of assignment")
    };

    Expression::Assign {
        var: left,
        value: Box::new(right),
    }
}

fn parse_or(stream: &mut TokenStream) -> Expression {
    let mut left = parse_and(stream);
    while let Token::Or = stream.peek() {
        stream.advance();
        let right = parse_and(stream);
        left = Expression::Binary(Binary {
            left: Box::new(left),
            op: BinaryOp::Or,
            right: Box::new(right),
        });
    }
    left
}

fn parse_and(stream: &mut TokenStream) -> Expression {
    let mut left = parse_comparison(stream);
    while let Token::And = stream.peek() {
        stream.advance();
        let right = parse_comparison(stream);
        left = Expression::Binary(Binary {
            left: Box::new(left),
            op: BinaryOp::And,
            right: Box::new(right),
        });
    }
    left
}

fn parse_comparison(stream: &mut TokenStream) -> Expression {
    let mut left = parse_term(stream);
    loop {
        let op = match stream.peek() {
            Token::Lt => BinaryOp::Lt,
            Token::Gt => BinaryOp::Gt,
            Token::DoubleEq => BinaryOp::Eq,
            _ => break,
        };
        stream.advance();
        let right = parse_term(stream);
        left = Expression::Binary(Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
        });
    }
    left
}

fn parse_term(stream: &mut TokenStream) -> Expression {
    let mut left = parse_factor(stream);
    loop {
        let op = match stream.peek() {
            Token::Plus => BinaryOp::Add,
            Token::Minus => BinaryOp::Sub,
            _ => break,
        };
        stream.advance();
        let right = parse_factor(stream);
        left = Expression::Binary(Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
        });
    }
    left
}

fn parse_factor(stream: &mut TokenStream) -> Expression {
    let mut left = parse_unary(stream);
    loop {
        let op = match stream.peek() {
            Token::Star => BinaryOp::Mul,
            Token::Slash => BinaryOp::Div,
            _ => break,
        };
        stream.advance();
        let right = parse_unary(stream);
        left = Expression::Binary(Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
        });
    }
    left
}

fn parse_unary(stream: &mut TokenStream) -> Expression {
    match stream.advance() {
        Token::Integer(i) => Expression::LitInt(i),
        Token::Boolean(b) => Expression::LitBool(b),
        Token::Identifier(ident) => {
            if let Token::LParen = stream.peek() {
                stream.advance();
                let mut args = Vec::new();
                loop {
                    if let Token::RParen = stream.peek() {
                        stream.advance();
                        break;
                    }
                    args.push(parse_binary(stream));
                    if let Token::Comma = stream.peek() {
                        stream.advance();
                    }
                }
                Expression::Call(ident, args)
            } else {
                Expression::Identifier(ident)
            }
        }
        other => panic!("unexpected token {other:?}"),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::lex;

    #[test]
    fn test_parse_binary() {
        let source = "1 + 2 * 3";
        let mut stream = lex(source.chars().collect());
        let expr = parse_binary(&mut stream);
        assert_eq!(
            expr,
            Expression::Binary(Binary {
                left: Box::new(Expression::LitInt(1)),
                op: BinaryOp::Add,
                right: Box::new(Expression::Binary(Binary {
                    left: Box::new(Expression::LitInt(2)),
                    op: BinaryOp::Mul,
                    right: Box::new(Expression::LitInt(3)),
                })),
            })
        );
    }
}
