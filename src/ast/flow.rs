use crate::{
    ast::parse_expression,
    lexer::{Token, TokenStream},
};

use super::{parse_block, Expression, Item};

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
