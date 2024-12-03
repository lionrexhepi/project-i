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
        typename: SmolStr,
        value: Expression,
    },
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    LitInt(i64),
    LitBool(bool),
    Identifier(SmolStr),
}

pub fn parse(stream: &mut TokenStream) -> Ast {
    let mut items = Vec::new();
    loop {
        match stream.advance() {
            Token::Eof => break,
            Token::Print => {
                let expression = match stream.advance() {
                    Token::Integer(i) => Expression::LitInt(i),
                    Token::Boolean(b) => Expression::LitBool(b),
                    Token::Identifier(ident) => Expression::Identifier(ident),
                    _ => panic!("unexpected token"),
                };
                items.push(Item::Print(expression));
            }
            Token::Let => {
                let name = match stream.advance() {
                    Token::Identifier(ident) => ident,
                    _ => panic!("unexpected token"),
                };

                let typename = match stream.advance() {
                    Token::Colon => match stream.advance() {
                        Token::Identifier(ident) => ident,
                        _ => panic!("unexpected token"),
                    },
                    _ => panic!("unexpected token"),
                };

                let value = match stream.advance() {
                    Token::Eq => match stream.advance() {
                        Token::Integer(i) => Expression::LitInt(i),
                        Token::Boolean(b) => Expression::LitBool(b),
                        Token::Identifier(ident) => Expression::Identifier(ident),
                        _ => panic!("unexpected token"),
                    },
                    _ => panic!("unexpected token"),
                };
                items.push(Item::Declaration {
                    name,
                    typename,
                    value,
                });
            }
            other => panic!("Unexpected token: {other:?}"),
        }
        let (Token::Semicolon | Token::Eof) = stream.advance() else {
            panic!("Expected semicolon to terminate statement")
        };
    }
    Ast { items }
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
                typename: "int".into(),
                value: Expression::LitInt(42)
            }
        );
    }
}
