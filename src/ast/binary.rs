use crate::{
    expect,
    lexer::{Payload, TokenStream},
};

use super::{Error, Expression, Result};

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

pub fn parse_binary(stream: &mut TokenStream) -> Result<Expression> {
    parse_assignment(stream)
}

fn parse_assignment(stream: &mut TokenStream) -> Result<Expression> {
    println!("parse_assignment");
    let left = parse_or(stream)?;
    let Payload::Eq = stream.peek().payload else {
        return Ok(left);
    };
    let right = parse_or(stream)?;

    expect!(
        stream,
        Payload::Identifier(left),
        "a variable as an assign target"
    );

    Ok(Expression::Assign {
        var: left,
        value: Box::new(right),
    })
}

fn parse_or(stream: &mut TokenStream) -> Result<Expression> {
    println!("parse_or");
    let mut left = parse_and(stream)?;
    while let Payload::Or = stream.peek().payload {
        stream.advance();
        let right = parse_and(stream)?;
        left = Expression::Binary(Binary {
            left: Box::new(left),
            op: BinaryOp::Or,
            right: Box::new(right),
        });
    }
    Ok(left)
}

fn parse_and(stream: &mut TokenStream) -> Result<Expression> {
    println!("parse_and");
    let mut left = parse_comparison(stream)?;
    while let Payload::And = stream.peek().payload {
        stream.advance();
        let right = parse_comparison(stream)?;
        left = Expression::Binary(Binary {
            left: Box::new(left),
            op: BinaryOp::And,
            right: Box::new(right),
        });
    }
    Ok(left)
}

fn parse_comparison(stream: &mut TokenStream) -> Result<Expression> {
    println!("parse_comparison");
    let mut left = parse_term(stream)?;
    loop {
        let op = match stream.peek().payload {
            Payload::Lt => BinaryOp::Lt,
            Payload::Gt => BinaryOp::Gt,
            Payload::DoubleEq => BinaryOp::Eq,
            _ => break,
        };
        stream.advance();
        let right = parse_term(stream)?;
        left = Expression::Binary(Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
        });
    }
    Ok(left)
}

fn parse_term(stream: &mut TokenStream) -> Result<Expression> {
    println!("parse_term");
    let mut left = parse_factor(stream)?;
    loop {
        let op = match stream.peek().payload {
            Payload::Plus => BinaryOp::Add,
            Payload::Minus => BinaryOp::Sub,
            _ => break,
        };
        stream.advance();
        let right = parse_factor(stream)?;
        left = Expression::Binary(Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
        });
    }
    Ok(left)
}

fn parse_factor(stream: &mut TokenStream) -> Result<Expression> {
    println!("parse_factor");
    let mut left = parse_unary(stream)?;
    loop {
        let op = match stream.peek().payload {
            Payload::Star => BinaryOp::Mul,
            Payload::Slash => BinaryOp::Div,
            _ => break,
        };
        stream.advance();
        let right = parse_unary(stream)?;
        left = Expression::Binary(Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
        });
    }
    Ok(left)
}

fn parse_unary(stream: &mut TokenStream) -> Result<Expression> {
    println!("parse_unary");
    match stream.advance().payload {
        Payload::Integer(i) => Ok(Expression::LitInt(i)),
        Payload::Boolean(b) => Ok(Expression::LitBool(b)),
        Payload::Identifier(ident) => {
            if let Payload::LParen = stream.peek().payload {
                stream.advance();
                let mut args = Vec::new();
                loop {
                    if let Payload::RParen = stream.peek().payload {
                        stream.advance();
                        break;
                    }
                    args.push(parse_binary(stream)?);
                    if let Payload::Comma = stream.peek().payload {
                        stream.advance();
                    }
                }
                Ok(Expression::Call(ident, args))
            } else {
                Ok(Expression::Identifier(ident))
            }
        }
        other => Err(Error::UnexpectedToken {
            expected: "Literal or identifier",
            found: other,
        }),
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::lexer::{lex, InMemoryFile};

    #[test]
    fn test_parse_binary() {
        let source = "1 + 2 * 3";
        let mut stream = lex(source.chars().collect::<InMemoryFile>()).unwrap();
        let expr = parse_binary(&mut stream).unwrap();
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
