mod binary;
mod flow;
mod ident;
use binary::parse_binary;
pub use binary::{Binary, BinaryOp};
use flow::{parse_block, parse_if, parse_while};
pub use flow::{Block, Else, If, While};
use snafu::Snafu;

use crate::{
    errors::SourceLocation,
    lexer::{Payload, Token, TokenStream},
};

pub struct Ast {
    items: Vec<Item>,
}

impl Ast {
    #[allow(clippy::should_implement_trait)]
    pub fn into_iter(self) -> impl Iterator<Item = Item> {
        self.items.into_iter()
    }

    pub fn items(&self) -> &[Item] {
        &self.items
    }
}

#[cfg(test)]
impl<I> From<I> for Ast
where
    I: IntoIterator<Item = Item>,
{
    fn from(items: I) -> Self {
        Ast {
            items: items.into_iter().collect(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Item {
    Print(Expression),
    Declaration {
        name: Identifier,
        typename: Option<Identifier>,
        value: Expression,
    },
    Expression(Expression),
}

pub use ident::Identifier;

#[derive(Debug, PartialEq)]
pub enum Expression {
    LitInt(i64),
    LitBool(bool),
    Identifier(Identifier),
    Call(Identifier, Vec<Expression>),
    Function {
        name: Option<Identifier>,
        args: Vec<FnArg>,
        return_type: Option<Identifier>,
        body: Block,
    },
    If(If),
    While(While),
    Binary(binary::Binary),
    Assign {
        var: Identifier,
        value: Box<Expression>,
    },
}

#[derive(Debug, PartialEq)]
pub struct FnArg {
    pub name: Identifier,
    pub typename: Identifier,
}

#[macro_export]
macro_rules! expect {
    ($stream:ident, $pat:pat, $expected:expr) => {
        let temp = $stream.peek();
        let $pat = (match &temp.payload {
            #[allow(unused_variables)]
            $pat => $stream.advance().payload,
            other => Err(Error::UnexpectedToken {
                expected: $expected,
                found: other.clone(),
                at: temp.location.clone(),
            })?,
        }) else {
            unreachable!()
        };
    };
}

pub fn parse(stream: &mut TokenStream) -> Result<Ast> {
    let mut items = Vec::new();
    while let Some(item) = parse_item(stream)? {
        items.push(item);
        let next = stream.advance();
        match next.payload {
            Payload::Semicolon | Payload::Eof => {}
            other => {
                return Err(Error::UnexpectedToken {
                    expected: "A semicolon to terminate a statement",
                    found: other,
                    at: next.location,
                })
            }
        }
    }
    Ok(Ast { items })
}

fn parse_item(stream: &mut TokenStream) -> Result<Option<Item>> {
    let item = match stream.peek().payload {
        Payload::Eof => return Ok(None),
        Payload::Print => {
            stream.advance();
            let expression = parse_expression(stream)?;
            Item::Print(expression)
        }
        Payload::Let => {
            stream.advance();
            println!("Parsing decl ");
            expect!(stream, Payload::Identifier(name), "identifier");
            let Token { payload, location } = stream.peek();
            let typename = match payload {
                Payload::Colon => {
                    stream.advance();
                    expect!(stream, Payload::Identifier(ident), "identifier");
                    Some(ident)
                }
                Payload::Eq => None,
                other => {
                    return Err(Error::UnexpectedToken {
                        expected: ": or =",
                        found: other.clone(),
                        at: location.clone(),
                    })
                }
            };

            println!("Type: {typename:?}");

            expect!(stream, Payload::Eq, "an assignment");

            println!("next: {:?}", stream.peek());

            let value = parse_expression(stream)?;

            Item::Declaration {
                name,
                typename,
                value,
            }
        }
        _ => Item::Expression(parse_expression(stream)?),
    };

    Ok(Some(item))
}

#[track_caller]
fn parse_expression(stream: &mut TokenStream) -> Result<Expression> {
    match stream.peek().payload {
        Payload::Fn => {
            stream.advance();
            let name = if let Payload::Identifier(_) = &stream.peek().payload {
                let Payload::Identifier(name) = stream.advance().payload else {
                    unreachable!()
                };
                Some(name)
            } else {
                None
            };
            let mut args = Vec::new();
            if let Payload::LParen = stream.peek().payload {
                stream.advance();
                while let Payload::Identifier(_) = stream.peek().payload {
                    let Payload::Identifier(name) = stream.advance().payload else {
                        unreachable!()
                    };
                    expect!(stream, Payload::Colon, "a colon");
                    expect!(stream, Payload::Identifier(typename), "a typename");
                    args.push(FnArg { name, typename });
                    if let Payload::Comma = stream.peek().payload {
                        stream.advance();
                    }
                }
                expect!(stream, Payload::RParen, "a closing parenthesis");
            };

            let return_type = if let Payload::Colon = stream.peek().payload {
                stream.advance();
                expect!(stream, Payload::Identifier(ident), "a typename");
                Some(ident)
            } else {
                None
            };

            Ok(Expression::Function {
                name,
                body: parse_block(stream)?,
                args,
                return_type,
            })
        }
        Payload::If => {
            stream.advance();
            let if_expr = parse_if(stream)?;
            Ok(Expression::If(if_expr))
        }
        Payload::While => {
            stream.advance();
            let while_expr = parse_while(stream)?;
            Ok(Expression::While(while_expr))
        }
        _ => Ok(parse_binary(stream)?),
    }
}

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("{:?}: unexpected token: expected {}, found {:?}", at, expected, found))]
    UnexpectedToken {
        expected: &'static str,
        found: Payload,
        at: SourceLocation,
    },
}

pub type Result<T> = std::result::Result<T, Error>;
