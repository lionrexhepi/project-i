use crate::lexer::TokenStream;

use super::Expression;

#[derive(Debug, PartialEq)]
pub struct Binary {
    pub left: Box<Expression>,
    pub op: BinaryOp,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
}

pub fn parse_binary(stream: TokenStream) -> Binary {
    todo!()
}
