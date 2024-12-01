use std::collections::VecDeque;

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Print,
    Identifier(String),
    Integer(i64),
    Boolean(bool),
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

    pub fn next(&mut self) -> Token {
        self.inner.pop_front().unwrap_or(Token::Eof)
    }
}

pub fn lex(source: Vec<char>) -> TokenStream {
    let mut stream = TokenStream::new();
    let mut index = 0;
    loop {
        let c = source.get(index).copied();
        dbg!(c);
        match c {
            Some(' ') => {}
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
            }
            Some(other) if other.is_ascii_alphabetic() => {
                let mut ident = c.unwrap().to_string();
                index += 1;
                loop {
                    let c = source.get(index).copied();
                    match c {
                        Some(letter) if letter.is_ascii_alphabetic() => {
                            ident.push(letter);
                            index += 1;
                        }
                        _ => {
                            stream.push(match ident.as_str() {
                                "print" => Token::Print,
                                "true" => Token::Boolean(true),
                                "false" => Token::Boolean(false),
                                _ => Token::Identifier(ident),
                            });

                            break;
                        }
                    }
                }
            }
            None => {
                break;
            }
            _ => todo!(),
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
}
