use crate::lexer::{Token, TokenStream};

pub struct Ast {
    items: Vec<Item>,
}

impl Ast {
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

pub enum Item {
    Print(Expression),
}

pub enum Expression {
    LitInt(i64),
    LitBool(bool),
}

pub fn parse(stream: &mut TokenStream) -> Ast {
    let mut items = Vec::new();
    loop {
        match stream.next() {
            Token::Eof => break,
            Token::Print => {
                let expression = match stream.next() {
                    Token::Integer(i) => Expression::LitInt(i),
                    Token::Boolean(b) => Expression::LitBool(b),
                    _ => panic!("unexpected token"),
                };
                items.push(Item::Print(expression));
            }
            Token::Identifier(_) => todo!(),
            Token::Integer(_) => todo!(),
            Token::Boolean(_) => todo!(),
        }
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
}
