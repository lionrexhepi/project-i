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
    pub condition: Box<Expression>,
    pub then: Block,
    pub otherwise: Option<Else>,
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
    let condition = Box::new(parse_expression(stream)?);
    let then = parse_block(stream)?;
    let otherwise = if let Payload::Else = stream.peek().payload {
        stream.advance();
        Some(parse_else(stream)?)
    } else {
        None
    };

    Ok(If {
        condition,
        then,
        otherwise,
    })
}

fn parse_else(stream: &mut TokenStream) -> Result<Else> {
    if let Payload::If = stream.peek().payload {
        stream.advance();
        Ok(Else::If(Box::new(parse_if(stream)?)))
    } else {
        Ok(Else::Block(parse_block(stream)?))
    }
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
