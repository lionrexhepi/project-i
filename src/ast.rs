mod binary;
mod flow;
mod ident;
use binary::parse_binary;
pub use binary::{Binary, BinaryOp};
use flow::{parse_block, parse_if, parse_while};
pub use flow::{Block, Else, If, While};
use snafu::Snafu;

use crate::lexer::{Payload, TokenStream};

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

#[macro_export]
macro_rules! expect {
    ($stream:ident, $pat:pat, $expected:expr) => {
        let $pat = (match $stream.peek() {
            #[allow(unused_variables)]
            $pat => $stream.advance(),
            other => Err(Error::UnexpectedToken {
                expected: $expected,
                found: other.clone(),
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

        let (Payload::Semicolon | Payload::Eof) = stream.advance() else {
            panic!("Expected semicolon to terminate statement")
        };
    }
    Ok(Ast { items })
}

fn parse_item(stream: &mut TokenStream) -> Result<Option<Item>> {
    let item = match stream.peek() {
        Payload::Eof => return Ok(None),
        Payload::Print => {
            stream.advance();
            let expression = parse_expression(stream)?;
            Item::Print(expression)
        }
        Payload::Let => {
            stream.advance();
            expect!(stream, Payload::Identifier(name), "identifier");

            let typename = match stream.peek() {
                Payload::Colon => {
                    stream.advance();
                    expect!(stream, Payload::Identifier(ident), "identifier");
                    Some(ident)
                }
                Payload::Eq => None,
                _ => panic!("unexpected token"),
            };

            expect!(stream, Payload::Eq, "an assignment");

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

fn parse_expression(stream: &mut TokenStream) -> Result<Expression> {
    match stream.peek() {
        Payload::Fn => {
            stream.advance();
            Ok(Expression::Function {
                body: parse_block(stream)?,
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
    #[snafu(display("unexpected token: expected {}, found {:?}", expected, found))]
    UnexpectedToken {
        expected: &'static str,
        found: Payload,
    },
}

pub type Result<T> = std::result::Result<T, Error>;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_print_int() {
        let mut stream = TokenStream::from([Payload::Print, Payload::Integer(42)]);
        let items = parse(&mut stream).unwrap();
        assert_eq!(items.items.len(), 1);
        assert!(matches!(
            items.items[0],
            Item::Print(Expression::LitInt(42))
        ))
    }

    #[test]
    fn test_print_bool() {
        let mut stream = TokenStream::from([Payload::Print, Payload::Boolean(true)]);
        let items = parse(&mut stream).unwrap();
        assert_eq!(items.items.len(), 1);
        assert!(matches!(
            items.items[0],
            Item::Print(Expression::LitBool(true))
        ))
    }

    #[test]
    fn test_print_identifier() {
        let mut stream = TokenStream::from([Payload::Print, Payload::Identifier("foo".into())]);
        let items = parse(&mut stream).unwrap();
        assert_eq!(items.items.len(), 1);
        assert_eq!(
            items.items[0],
            Item::Print(Expression::Identifier("foo".into()))
        );
    }

    #[test]
    fn test_let_int() {
        use Payload::*;
        let mut stream = TokenStream::from([
            Let,
            Identifier("foo".into()),
            Colon,
            Identifier("int".into()),
            Eq,
            Integer(42),
        ]);
        let items = parse(&mut stream).unwrap();
        assert_eq!(items.items.len(), 1);
        assert_eq!(
            items.items[0],
            Item::Declaration {
                name: "foo".into(),
                typename: Some("int".into()),
                value: Expression::LitInt(42)
            }
        );
    }

    #[test]
    fn test_fn_expr() {
        use Payload::*;
        let mut stream = TokenStream::from([
            Let,
            Identifier("func".into()),
            Colon,
            Identifier("fn".into()),
            Eq,
            Fn,
            LBrace,
            Print,
            Integer(42),
            RBrace,
        ]);
        let items = parse(&mut stream).unwrap();
        assert_eq!(items.items.len(), 1);
        assert_eq!(
            items.items[0],
            Item::Declaration {
                name: "func".into(),
                typename: Some("fn".into()),
                value: Expression::Function {
                    body: Block {
                        statements: vec![Item::Print(Expression::LitInt(42))],
                        semicolon_terminated: false
                    }
                }
            }
        );
    }
}
