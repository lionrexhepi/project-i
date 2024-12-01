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
    Literal(i64),
}

macro_rules! expect {
    ($stream:ident, $pat:pat) => {
        let $pat = $stream.next() else {
            panic!("unexpected token");
        };
    };
}

pub fn parse(stream: &mut TokenStream) -> Ast {
    let mut items = Vec::new();
    loop {
        match stream.next() {
            Token::Eof => break,
            Token::Print => {
                expect!(stream, Token::Literal(lit));
                println!("{}", stream.inner.len());
                items.push(Item::Print(Expression::Literal(lit)));
            }
            other => todo!("Strange token: {other:?}"),
        }
    }
    Ast { items }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse() {
        let mut stream = TokenStream::from([Token::Print, Token::Literal(42)]);
        let items = parse(&mut stream);
        assert_eq!(items.items.len(), 1);
        assert!(matches!(
            items.items[0],
            Item::Print(Expression::Literal(42))
        ))
    }
}
