use std::collections::VecDeque;

use smol_str::{SmolStr, SmolStrBuilder};
use snafu::Snafu;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    // Literals
    Integer(i64),
    Boolean(bool),
    //Identifiers
    Identifier(SmolStr),
    // Keywords
    Print,
    Fn,
    If,
    While,
    Else,
    Let,
    // Punctuation
    Colon,
    LParen,
    RParen,
    Comma,
    Semicolon,
    LBrace,
    RBrace,
    // Operators
    Eq,
    DoubleEq,
    Plus,
    Minus,
    Star,
    Slash,
    Gt,
    Lt,
    Or,
    And,
    /// EOF.
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

    pub fn peek(&self) -> &Token {
        self.inner.front().unwrap_or(&Token::Eof)
    }
}

pub fn lex(source: Vec<char>) -> Result<TokenStream> {
    let mut stream = TokenStream::new();
    let mut index = 0;
    loop {
        let Some(c) = source.get(index).copied() else {
            break;
        };

        if c.is_ascii_whitespace() {
            index += 1;
        } else if c.is_ascii_alphabetic() {
            let mut ident = SmolStrBuilder::new();
            ident.push(c);
            index += 1;
            loop {
                let c = source.get(index).copied();
                match c {
                    Some(letter) if letter.is_ascii_alphanumeric() => {
                        ident.push(letter);
                        index += 1;
                    }
                    _ => {
                        let ident = ident.finish();
                        stream.push(special_ident(ident));
                        break;
                    }
                }
            }
            continue;
        } else if c.is_ascii_digit() {
            let mut lit = c.to_digit(10).unwrap() as i64;
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
        } else {
            index += 1;
            stream.push(match c {
                ',' => Token::Comma,
                ';' => Token::Semicolon,
                '(' => Token::LParen,
                ')' => Token::RParen,
                ':' => Token::Colon,
                '{' => Token::LBrace,
                '}' => Token::RBrace,
                '=' => {
                    if source[index + 1] == '=' {
                        index += 1;
                        Token::DoubleEq
                    } else {
                        Token::Eq
                    }
                }
                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Star,
                '/' => Token::Slash,
                '>' => Token::Gt,
                '<' => Token::Lt,
                '|' => Token::Or,
                '&' => Token::And,
                other => return Err(Error::UnexpectedCharacter { token: other }),
            });
        }
    }

    Ok(stream)
}

fn special_ident(ident: SmolStr) -> Token {
    match ident.as_str() {
        "print" => Token::Print,
        "true" => Token::Boolean(true),
        "false" => Token::Boolean(false),
        "let" => Token::Let,
        "fn" => Token::Fn,
        "while" => Token::While,
        "if" => Token::If,
        "else" => Token::Else,
        _ => Token::Identifier(ident),
    }
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

#[derive(Snafu, Debug)]
pub enum Error {
    #[snafu(display("Invalid character: {}", token))]
    UnexpectedCharacter { token: char },
    #[snafu(display("Invalid number literal. Only base 10 ASCII digits are supported."))]
    InvalidNumberLiteral,
}

type Result<T> = std::result::Result<T, Error>;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_number() {
        let source = "print 42";
        let stream = lex(source.chars().collect()).unwrap();
        assert_eq!(stream.inner, vec![Token::Print, Token::Integer(42)]);
    }

    #[test]
    fn test_bool() {
        let source = "print true";
        let stream = lex(source.chars().collect()).unwrap();
        assert_eq!(stream.inner, vec![Token::Print, Token::Boolean(true)]);
    }

    #[test]
    fn test_identifier() {
        let source = "print foo";
        let stream = lex(source.chars().collect()).unwrap();
        assert_eq!(
            stream.inner,
            vec![Token::Print, Token::Identifier("foo".into())]
        );
    }

    #[test]
    fn test_let() {
        let source = "let x = 42";
        let stream = lex(source.chars().collect()).unwrap();
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
