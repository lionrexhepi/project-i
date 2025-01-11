pub mod symbols;
pub mod transformer;
mod types;
use snafu::Snafu;

use smol_str::SmolStr;

use crate::ast::{self, Identifier};

pub struct Ir {
    pub items: Vec<IrItem>,
}

#[cfg(test)]
impl<I> From<I> for Ir
where
    I: IntoIterator<Item = IrItem>,
{
    fn from(items: I) -> Self {
        Ir {
            items: items.into_iter().collect(),
        }
    }
}

/// Stringly-typed intermediate representation
#[derive(Debug, PartialEq, Clone)]
pub enum IrItem {
    Print(Box<IrItem>),
    Declaration {
        typename: SmolStr,
        var: SmolStr,
        value: Option<Box<IrItem>>,
    },
    LitInt(i64),
    LitBool(bool),
    Variable(SmolStr),
    Function {
        args: Vec<(SmolStr, SmolStr)>,
        return_type: SmolStr,
        body: IrBlock,
    },
    If {
        branches: Vec<IfBranch>,
        otherwise: Option<IrBlock>,
    },
    Loop {
        condition: Box<IrItem>,
        body: IrBlock,
    },
    Block(IrBlock),
    Assign {
        var: SmolStr,
        value: Box<IrItem>,
    },
    Op {
        lhs: Box<IrItem>,
        rhs: Box<IrItem>,
        op: Operator,
    },
    Call {
        name: SmolStr,
        args: Vec<IrItem>,
    },
    Multiple(Vec<IrItem>),
    Return(Option<Box<IrItem>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfBranch {
    pub condition: IrItem,
    pub then: IrBlock,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IrBlock {
    statements: Vec<IrItem>,
}

impl IntoIterator for IrBlock {
    type Item = IrItem;
    // the full beauty of rust
    type IntoIter = std::vec::IntoIter<IrItem>;
    fn into_iter(self) -> Self::IntoIter {
        self.statements.into_iter()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
}

impl From<ast::BinaryOp> for Operator {
    fn from(value: ast::BinaryOp) -> Self {
        match value {
            ast::BinaryOp::Add => Self::Add,
            ast::BinaryOp::Sub => Self::Sub,
            ast::BinaryOp::Mul => Self::Mul,
            ast::BinaryOp::Div => Self::Div,
            ast::BinaryOp::Eq => Self::Eq,
            ast::BinaryOp::Lt => Self::Lt,
            ast::BinaryOp::Gt => Self::Gt,
            ast::BinaryOp::Le => Self::Lte,
            ast::BinaryOp::Ge => Self::Gte,
            ast::BinaryOp::And => Self::And,
            ast::BinaryOp::Or => Self::Or,
        }
    }
}

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("Type mismatch: expected {}, found {}", expected, found))]
    TypeMismatch {
        expected: Identifier,
        found: Identifier,
    },
    #[snafu(display("Variable not found: {}", var))]
    UndeclaredVariable { var: Identifier },
    #[snafu(display("Function not found: {}", name))]
    FunctionNotFound { name: Identifier },
    #[snafu(display("Undeclared type: {}", name))]
    UndeclaredType { name: Identifier },
    #[snafu(display("Not a function: {}", name))]
    NotAFunction { name: Identifier },
}

pub type Result<T> = std::result::Result<T, Error>;
