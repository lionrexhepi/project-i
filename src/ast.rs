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
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    LitInt(i64),
    LitBool(bool),
    Identifier(SmolStr),
    Function { body: Vec<Item> },
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
    let item = match stream.advance() {
        Token::Eof => return None,
        Token::Print => {
            let expression = parse_expression(stream);
            Item::Print(expression)
        }
        Token::Let => {
            let name = match stream.advance() {
                Token::Identifier(ident) => ident,
                _ => panic!("unexpected token"),
            };

            let typename = match stream.peek() {
                Token::Colon => {
                    stream.advance();
                    match stream.advance() {
                        Token::Identifier(ident) => Some(ident),
                        other => panic!("unexpected token {other:?}"),
                    }
                }
                Token::Eq => None,
                _ => panic!("unexpected token"),
            };

            let value = match stream.advance() {
                Token::Eq => parse_expression(stream),
                _ => panic!("unexpected token"),
            };
            Item::Declaration {
                name,
                typename,
                value,
            }
        }
        other => panic!("Unexpected token: {other:?}"),
    };
    Some(item)
}

fn parse_expression(stream: &mut TokenStream) -> Expression {
    match stream.advance() {
        Token::Integer(i) => Expression::LitInt(i),
        Token::Boolean(b) => Expression::LitBool(b),
        Token::Identifier(ident) => Expression::Identifier(ident),
        Token::Fn => {
            let mut body = Vec::new();
            loop {
                match stream.peek() {
                    Token::End => {
                        stream.advance();
                        break;
                    }
                    _ => {
                        let Some(item) = parse_item(stream) else {
                            break;
                        };
                        body.push(item);
                    }
                }
            }
            Expression::Function { body }
        }
        _ => panic!("unexpected token"),
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
        let mut stream = TokenStream::from([
            Token::Let,
            Token::Identifier("foo".into()),
            Token::Colon,
            Token::Identifier("int".into()),
            Token::Eq,
            Token::Integer(42),
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
        let mut stream = TokenStream::from([
            Token::Let,
            Token::Identifier("func".into()),
            Token::Colon,
            Token::Identifier("fn".into()),
            Token::Eq,
            Token::Fn,
            Token::Print,
            Token::Integer(42),
            Token::End,
        ]);
        let items = parse(&mut stream);
        assert_eq!(items.items.len(), 1);
        assert_eq!(
            items.items[0],
            Item::Declaration {
                name: "func".into(),
                typename: Some("fn".into()),
                value: Expression::Function {
                    body: vec![Item::Print(Expression::LitInt(42))]
                }
            }
        );
    }
}
