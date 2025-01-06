//! Flow control structures like `if` and `while`.
//! All parser functions assume that the relevant keywords (if, while, else, ...) have already been consumed.

use crate::{
    ast::{parse_expression, parse_item},
    expect,
    lexer::{Payload, TokenStream},
};

use super::{Error, Expression, Item, Result};

#[derive(Debug, PartialEq)]
pub struct While {
    pub condition: Box<Expression>,
    pub body: Block,
}

#[derive(Debug, PartialEq)]
pub struct If {
    pub branches: Vec<(Expression, Block)>,
    pub otherwise: Option<Block>,
}

#[derive(Debug, PartialEq)]
pub enum Else {
    Block(Block),
    If(Box<If>),
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub statements: Vec<Item>,
    pub semicolon_terminated: bool,
}

impl IntoIterator for Block {
    type Item = Item;
    type IntoIter = std::vec::IntoIter<Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.statements.into_iter()
    }
}

pub fn parse_while(stream: &mut TokenStream) -> Result<While> {
    let condition = Box::new(parse_expression(stream)?);
    let body = parse_block(stream)?;

    Ok(While { condition, body })
}

pub fn parse_if(stream: &mut TokenStream) -> Result<If> {
    let mut branches = Vec::new();
    let otherwise = loop {
        let condition = parse_expression(stream)?;
        let body = parse_block(stream)?;
        branches.push((condition, body));
        let Payload::Else = stream.peek().payload else {
            break None;
        };
        stream.advance();

        if let Payload::If = stream.peek().payload {
            stream.advance();
        } else {
            break Some(parse_block(stream)?);
        }
    };

    Ok(If {
        branches,
        otherwise,
    })
}

pub fn parse_block(stream: &mut TokenStream) -> Result<Block> {
    expect!(stream, Payload::LBrace, "An opening brace");
    let mut block = Vec::new();
    let mut had_semicolon = true;
    let semicolon_terminated = loop {
        let peek = stream.peek();
        match &peek.payload {
            Payload::RBrace => {
                stream.advance();
                break had_semicolon;
            }
            other if !had_semicolon => {
                return Err(Error::UnexpectedToken {
                    expected: "}",
                    found: other.clone(),
                    at: peek.location.clone(),
                })
            }
            _ => {
                let Some(item) = parse_item(stream)? else {
                    let found = stream.peek().clone();
                    return Err(Error::UnexpectedToken {
                        expected: "A statement",
                        found: found.payload,
                        at: found.location,
                    });
                };
                block.push(item);
                if let Payload::Semicolon = stream.peek().payload {
                    stream.advance();
                } else {
                    had_semicolon = false;
                };
            }
        }
    };
    Ok(Block {
        statements: block,
        semicolon_terminated,
    })
}
