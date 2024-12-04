use crate::{
    ast::{parse_expression, parse_item},
    expect,
    lexer::{Token, TokenStream},
};

use super::{Expression, Item};

#[derive(Debug, PartialEq)]
pub struct If {
    pub condition: Box<Expression>,
    pub then: Vec<Item>,
    pub otherwise: Option<Else>,
}

#[derive(Debug, PartialEq)]
pub enum Else {
    Block(Vec<Item>),
    If(Box<If>),
}

pub fn parse_if(stream: &mut TokenStream) -> If {
    let condition = Box::new(parse_expression(stream));
    expect!(stream, Token::Do);
    let mut then = Vec::new();
    let otherwise = loop {
        match stream.peek() {
            Token::End => {
                stream.advance();
                break None;
            }
            Token::Else => {
                stream.advance();
                break Some(parse_else(stream));
            }
            _ => {
                let Some(item) = parse_item(stream) else {
                    panic!("Unclosed block");
                };
                then.push(item);
            }
        }
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
        let mut block = Vec::new();
        loop {
            match stream.peek() {
                Token::End => {
                    stream.advance();
                    break;
                }
                _ => {
                    let Some(item) = parse_item(stream) else {
                        panic!("Unclosed block");
                    };
                    block.push(item);
                }
            }
        }
        Else::Block(block)
    }
}
