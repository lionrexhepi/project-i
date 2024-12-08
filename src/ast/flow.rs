//! Flow control structures like `if` and `while`.
//! All parser functions assume that the relevant keywords (if, while, else, ...) have already been consumed.

use crate::{
    ast::{parse_expression, parse_item},
    expect,
    lexer::{Token, TokenStream},
};

use super::{Expression, Item};

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
pub struct Block(pub Vec<Item>);

impl IntoIterator for Block {
    type Item = Item;
    type IntoIter = std::vec::IntoIter<Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

pub fn parse_while(stream: &mut TokenStream) -> While {
    let condition = Box::new(parse_expression(stream));
    let body = parse_block(stream);

    While { condition, body }
}

pub fn parse_if(stream: &mut TokenStream) -> If {
    let condition = Box::new(parse_expression(stream));
    let then = parse_block(stream);
    let otherwise = if let Token::Else = stream.peek() {
        stream.advance();
        Some(parse_else(stream))
    } else {
        None
    };

    If {
        condition,
        then,
        otherwise,
    }
}

fn parse_else(stream: &mut TokenStream) -> Else {
    if let Token::If = stream.peek() {
        stream.advance();
        Else::If(Box::new(parse_if(stream)))
    } else {
        Else::Block(parse_block(stream))
    }
}

pub fn parse_block(stream: &mut TokenStream) -> Block {
    expect!(stream, Token::LBrace);
    let mut block = Vec::new();
    let mut must_close = false;
    loop {
        match stream.peek() {
            Token::RBrace => {
                stream.advance();
                break;
            }
            _ if must_close => panic!("Expected Semicolon or end of block"),
            _ => {
                let Some(item) = parse_item(stream) else {
                    panic!("Unclosed block");
                };
                block.push(item);
                if let Token::Semicolon = stream.peek() {
                    stream.advance();
                } else {
                    must_close = true;
                };
            }
        }
    }
    Block(block)
}
