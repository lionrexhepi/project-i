use std::collections::VecDeque;

use smol_str::{SmolStr, SmolStrBuilder};
use snafu::Snafu;

use crate::errors::SourceLocation;

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

pub fn lex(mut source: impl File) -> Result<TokenStream> {
    let mut stream = TokenStream::new();

    loop {
        let Some(c) = source.peek() else {
            break;
        };

        if c.is_ascii_whitespace() {
            source.advance();
        } else if c.is_ascii_alphabetic() {
            let mut ident = SmolStrBuilder::new();
            ident.push(c);
            source.advance();
            loop {
                let c = source.peek();
                match c {
                    Some(letter) if letter.is_ascii_alphanumeric() => {
                        ident.push(letter);
                        source.advance();
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
            source.advance();
            loop {
                let c = source.peek();
                match c {
                    Some(digit) if digit.is_ascii_digit() => {
                        lit = lit * 10 + digit.to_digit(10).unwrap() as i64;
                        source.advance();
                    }
                    _ => {
                        stream.push(Token::Integer(lit));
                        break;
                    }
                }
            }
        } else {
            source.advance();
            stream.push(match c {
                ',' => Token::Comma,
                ';' => Token::Semicolon,
                '(' => Token::LParen,
                ')' => Token::RParen,
                ':' => Token::Colon,
                '{' => Token::LBrace,
                '}' => Token::RBrace,
                '=' => {
                    if let Some('=') = source.peek_n(1) {
                        source.advance();
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

pub trait File {
    /// File name for error reporting.
    fn name(&self) -> &str;

    fn advance(&mut self) -> Option<char>;
    fn peek(&self) -> Option<char>;
    fn peek_n(&self, n: usize) -> Option<char>;

    fn location(&self) -> SourceLocation;
}

pub struct InMemoryFile {
    name: String,
    data: VecDeque<char>,
    line: usize,
    column: usize,
}

impl File for InMemoryFile {
    fn name(&self) -> &str {
        &self.name
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.data.remove(0);
        match c {
            Some('\n') => {
                self.line += 1;
                self.column = 0;
            }
            Some(_) => self.column += 1,
            None => {}
        }
        c
    }

    fn peek(&self) -> Option<char> {
        self.data.front().copied()
    }

    fn peek_n(&self, n: usize) -> Option<char> {
        self.data.get(n).copied()
    }

    fn location(&self) -> SourceLocation {
        SourceLocation {
            line: self.line,
            column: self.column,
            file: self.name().into(),
        }
    }
}

impl FromIterator<char> for InMemoryFile {
    fn from_iter<I: IntoIterator<Item = char>>(iter: I) -> Self {
        InMemoryFile {
            name: "<string literal>".into(),
            data: iter.into_iter().collect(),
            line: 0,
            column: 0,
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
        let stream = lex(source.chars().collect::<InMemoryFile>()).unwrap();
        assert_eq!(stream.inner, vec![Token::Print, Token::Integer(42)]);
    }

    #[test]
    fn test_bool() {
        let source = "print true";
        let stream = lex(source.chars().collect::<InMemoryFile>()).unwrap();
        assert_eq!(stream.inner, vec![Token::Print, Token::Boolean(true)]);
    }

    #[test]
    fn test_identifier() {
        let source = "print foo";
        let stream = lex(source.chars().collect::<InMemoryFile>()).unwrap();
        assert_eq!(
            stream.inner,
            vec![Token::Print, Token::Identifier("foo".into())]
        );
    }

    #[test]
    fn test_let() {
        let source = "let x = 42";
        let stream = lex(source.chars().collect::<InMemoryFile>()).unwrap();
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
