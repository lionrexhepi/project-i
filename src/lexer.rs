use std::collections::VecDeque;

use smol_str::{SmolStr, SmolStrBuilder};

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(SmolStr),
    Integer(i64),
    Boolean(bool),
    Print,
    Let,
    Eq,
    Colon,
    Eof,
}

#[derive(Debug)]
pub struct TokenStream {
    pub inner: VecDeque<Token>,
}

impl TokenStream {
    fn new() -> Self {
        TokenStream {
            inner: VecDeque::new(),
        }
    }

    fn push(&mut self, token: Token) {
        self.inner.push_back(token);
    }

    pub fn advance(&mut self) -> Token {
        self.inner.pop_front().unwrap_or(Token::Eof)
    }
}

pub fn lex(source: Vec<char>) -> TokenStream {
    let mut stream = TokenStream::new();
    let mut index = 0;
    loop {
        let c = source.get(index).copied();

        match c {
            Some(' ') => {}
            Some(':') => {
                stream.push(Token::Colon);
            }
            Some('=') => {
                stream.push(Token::Eq);
            }
            Some(other) if other.is_ascii_digit() => {
                let mut lit = c.unwrap().to_digit(10).unwrap() as i64;
                index += 1;
                loop {
                    let c = source.get(index).copied();
                    match c {
                        Some(digit) if digit.is_ascii_digit() => {
                            lit = lit * 10 + digit.to_digit(10).unwrap() as i64;
                            index += 1;
                        }
                        _ => {
                            stream.push(Token::Integer(lit));
                            break;
                        }
                    }
                }
                continue;
            }
            Some(other) if other.is_ascii_alphabetic() => {
                let mut ident = SmolStrBuilder::new();
                ident.push(other);
                index += 1;
                loop {
                    let c = source.get(index).copied();
                    match c {
                        Some(letter) if letter.is_ascii_alphabetic() => {
                            ident.push(letter);
                            index += 1;
                        }
                        _ => {
                            let ident = ident.finish();
                            stream.push(match ident.as_str() {
                                "print" => Token::Print,
                                "true" => Token::Boolean(true),
                                "false" => Token::Boolean(false),
                                "let" => Token::Let,
                                _ => Token::Identifier(ident),
                            });

                            break;
                        }
                    }
                }
                continue;
            }
            None => {
                break;
            }
            Some(other) => todo!("Cannot handle char {other}"),
        }
        index += 1;
    }

    stream
}

#[cfg(test)]
impl<I> From<I> for TokenStream
where
    I: IntoIterator<Item = Token>,
{
    fn from(tokens: I) -> Self {
        TokenStream {
            inner: tokens.into_iter().collect(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_number() {
        let source = "print 42";
        let stream = lex(source.chars().collect());
        assert_eq!(stream.inner, vec![Token::Print, Token::Integer(42)]);
    }

    #[test]
    fn test_bool() {
        let source = "print true";
        let stream = lex(source.chars().collect());
        assert_eq!(stream.inner, vec![Token::Print, Token::Boolean(true)]);
    }

    #[test]
    fn test_identifier() {
        let source = "print foo";
        let stream = lex(source.chars().collect());
        assert_eq!(
            stream.inner,
            vec![Token::Print, Token::Identifier("foo".into())]
        );
    }

    #[test]
    fn test_let() {
        let source = "let x = 42";
        let stream = lex(source.chars().collect());
        assert_eq!(
            stream.inner,
            vec![
                Token::Let,
                Token::Identifier("x".into()),
                Token::Eq,
                Token::Integer(42)
            ]
        );
    }
}
