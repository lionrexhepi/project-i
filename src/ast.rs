mod binary;
mod flow;
use binary::parse_binary;
pub use binary::{Binary, BinaryOp};
use flow::{parse_block, parse_if, parse_while};
pub use flow::{Block, Else, If, While};
use smol_str::SmolStr;

use crate::lexer::{Token, TokenStream};

pub struct Ast {
    items: Vec<Item>,
}

impl Ast {
    #[allow(clippy::should_implement_trait)]
    pub fn into_iter(self) -> impl Iterator<Item = Item> {
        self.items.into_iter()
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
        name: SmolStr,
        typename: Option<SmolStr>,
        value: Expression,
    },
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    LitInt(i64),
    LitBool(bool),
    Identifier(SmolStr),
    Call(SmolStr, Vec<Expression>),
    Function { body: Block },
    If(If),
    While(While),
    Binary(binary::Binary),
}

#[macro_export]
macro_rules! expect {
    ( $stream:ident, $pat:pat) => {
        let $pat = $stream.advance() else {
            panic!("unexpected token")
        };
    };
}

pub fn parse(stream: &mut TokenStream) -> Ast {
    let mut items = Vec::new();
    loop {
        if let Some(item) = parse_item(stream) {
            items.push(item);
        } else {
            break;
        }
        let (Token::Semicolon | Token::Eof) = stream.advance() else {
            panic!("Expected semicolon to terminate statement")
        };
    }
    Ast { items }
}

fn parse_item(stream: &mut TokenStream) -> Option<Item> {
    let item = match stream.peek() {
        Token::Eof => return None,
        Token::Print => {
            stream.advance();
            let expression = parse_expression(stream);
            Item::Print(expression)
        }
        Token::Let => {
            stream.advance();
            expect!(stream, Token::Identifier(name));

            let typename = match stream.peek() {
                Token::Colon => {
                    stream.advance();
                    expect!(stream, Token::Identifier(ident));
                    Some(ident)
                }
                Token::Eq => None,
                _ => panic!("unexpected token"),
            };

            expect!(stream, Token::Eq);

            let value = parse_expression(stream);

            Item::Declaration {
                name,
                typename,
                value,
            }
        }
        _ => Item::Expression(parse_expression(stream)),
    };

    Some(item)
}

fn parse_expression(stream: &mut TokenStream) -> Expression {
    match stream.peek() {
        Token::Fn => {
            stream.advance();
            Expression::Function {
                body: parse_block(stream),
            }
        }
        Token::If => {
            stream.advance();
            let if_expr = parse_if(stream);
            Expression::If(if_expr)
        }
        Token::While => {
            stream.advance();
            let while_expr = parse_while(stream);
            Expression::While(while_expr)
        }
        _ => parse_binary(stream),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_print_int() {
        let mut stream = TokenStream::from([Token::Print, Token::Integer(42)]);
        let items = parse(&mut stream);
        assert_eq!(items.items.len(), 1);
        assert!(matches!(
            items.items[0],
            Item::Print(Expression::LitInt(42))
        ))
    }

    #[test]
    fn test_print_bool() {
        let mut stream = TokenStream::from([Token::Print, Token::Boolean(true)]);
        let items = parse(&mut stream);
        assert_eq!(items.items.len(), 1);
        assert!(matches!(
            items.items[0],
            Item::Print(Expression::LitBool(true))
        ))
    }

    #[test]
    fn test_print_identifier() {
        let mut stream = TokenStream::from([Token::Print, Token::Identifier("foo".into())]);
        let items = parse(&mut stream);
        assert_eq!(items.items.len(), 1);
        assert_eq!(
            items.items[0],
            Item::Print(Expression::Identifier("foo".into()))
        );
    }

    #[test]
    fn test_let_int() {
        use Token::*;
        let mut stream = TokenStream::from([
            Let,
            Identifier("foo".into()),
            Colon,
            Identifier("int".into()),
            Eq,
            Integer(42),
        ]);
        let items = parse(&mut stream);
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
        use Token::*;
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
        let items = parse(&mut stream);
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
