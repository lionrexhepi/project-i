use std::collections::VecDeque;

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Print,
    Identifier(String),
    Literal(i64),
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
                            stream.push(Token::Literal(lit));
                            break;
                        }
                    }
                }
            }
            Some(other) if other.is_ascii_alphabetic() => {
                let mut ident = c.unwrap().to_string();
                for c in source.iter().skip(index + 1) {
                    match c {
                        'a'..='z' | 'A'..='Z' | '0'..='9' => {
                            ident.push(*c);
                            index += 1;
                        }
                        _ => {
                            if ident == "print" {
                                stream.push(Token::Print);
                            } else {
                                stream.push(Token::Identifier(ident));
                            }
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
    fn test_lex() {
        let source = "print 42";
        let stream = lex(source.chars().collect());
        assert_eq!(stream.inner, vec![Token::Print, Token::Literal(42)]);
    }
}
