pub mod files;

use std::{collections::VecDeque, str::Utf8Error};

use smol_str::{SmolStr, SmolStrBuilder};
use snafu::Snafu;

use crate::{ast::Identifier, errors::SourceLocation};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Payload {
    // Literals
    Integer(i64),
    Boolean(bool),
    //Identifiers
    Identifier(Identifier),
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
        self.inner.pop_front().unwrap_or(Token::EOF)
    }

    pub fn peek(&self) -> &Token {
        const EOF_REF: &Token = &Token::EOF;
        self.inner.front().unwrap_or(EOF_REF)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub payload: Payload,
    pub location: SourceLocation,
}

impl Token {
    const EOF: Token = Token {
        payload: Payload::Eof,
        location: SourceLocation {
            line: 0,
            column: 0,
            file: SmolStr::new_static("<EOF>"),
        },
    };

    pub fn eof() -> Self {
        Token {
            payload: Payload::Eof,
            location: SourceLocation {
                line: 0,
                column: 0,
                file: "<EOF>".into(),
            },
        }
    }
}

pub fn lex(mut source: impl File) -> Result<TokenStream> {
    let mut stream = TokenStream::new();

    loop {
        let Some(c) = source.peek()? else {
            break;
        };

        let location = source.location().clone();

        if c.is_ascii_whitespace() {
            source.advance()?;
        } else if c.is_ascii_alphabetic() {
            let mut ident = SmolStrBuilder::new();
            ident.push(c);
            source.advance()?;
            loop {
                let c = source.peek()?;
                match c {
                    Some(letter) if letter.is_ascii_alphanumeric() => {
                        ident.push(letter);
                        source.advance()?;
                    }
                    _ => {
                        let ident = ident.finish();
                        stream.push(Token {
                            payload: special_ident(ident),
                            location,
                        });
                        break;
                    }
                }
            }
            continue;
        } else if c.is_ascii_digit() {
            let mut lit = c.to_digit(10).ok_or(Error::InvalidNumberLiteral)? as i64;
            source.advance()?;
            loop {
                let c = source.peek()?;
                match c {
                    Some(digit) if digit.is_ascii_digit() => {
                        lit = lit * 10
                            + digit.to_digit(10).ok_or(Error::InvalidNumberLiteral)? as i64;
                        source.advance()?;
                    }
                    _ => {
                        stream.push(Token {
                            payload: Payload::Integer(lit),
                            location,
                        });
                        break;
                    }
                }
            }
        } else {
            source.advance()?;
            let payload = match c {
                ',' => Payload::Comma,
                ';' => Payload::Semicolon,
                '(' => Payload::LParen,
                ')' => Payload::RParen,
                ':' => Payload::Colon,
                '{' => Payload::LBrace,
                '}' => Payload::RBrace,
                '=' => {
                    if let Some('=') = source.peek()? {
                        source.advance()?;
                        Payload::DoubleEq
                    } else {
                        Payload::Eq
                    }
                }
                '+' => Payload::Plus,
                '-' => Payload::Minus,
                '*' => Payload::Star,
                '/' => Payload::Slash,
                '>' => Payload::Gt,
                '<' => Payload::Lt,
                '|' => Payload::Or,
                '&' => Payload::And,
                other => return Err(Error::UnexpectedCharacter { token: other }),
            };
            stream.push(Token { payload, location });
        }
    }

    Ok(stream)
}

fn special_ident(ident: SmolStr) -> Payload {
    match ident.as_str() {
        "print" => Payload::Print,
        "true" => Payload::Boolean(true),
        "false" => Payload::Boolean(false),
        "let" => Payload::Let,
        "fn" => Payload::Fn,
        "while" => Payload::While,
        "if" => Payload::If,
        "else" => Payload::Else,
        _ => Payload::Identifier(ident.into()),
    }
}

pub trait File {
    /// File name for error reporting.
    fn name(&self) -> &str {
        &self.location().file
    }

    #[must_use = "May fail on I/O"]
    fn advance(&mut self) -> ReadCharResult;
    #[must_use = "May fail on I/O"]
    fn peek(&self) -> ReadCharResult;
    #[must_use = "May fail on I/O"]
    fn peek_n(&self, n: usize) -> ReadCharResult;

    fn location(&self) -> &SourceLocation;
}

#[derive(Snafu, Debug)]
pub enum Error {
    #[snafu(display("Invalid character: {}", token))]
    UnexpectedCharacter { token: char },
    #[snafu(display("Invalid number literal. Only base 10 ASCII digits are supported."))]
    InvalidNumberLiteral,
    #[snafu(display("IO Error while dealing with file {}", file.clone().unwrap_or_else(String::new)))]
    Io {
        file: Option<String>,
        source: std::io::Error,
    },
    #[snafu(display("Invalid Unicode byte sequence: {:x}", seq))]
    InvalidUtf8 { source: Utf8Error, seq: u32 },
    #[snafu(display("Unexpected end of file within Unicode byte sequence"))]
    UnexpectedEof { source: Utf8Error },
}

type Result<T> = std::result::Result<T, Error>;
type ReadCharResult = Result<Option<char>>;
