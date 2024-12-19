pub mod symbols;
mod transform;
mod types;
pub use transform::transform;

use smol_str::SmolStr;

use crate::ast;

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
#[derive(Debug, PartialEq)]
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
        body: IrBlock,
    },
    If {
        condition: Box<IrItem>,
        then: IrBlock,
        otherwise: Option<Box<IrItem>>,
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
}

#[derive(Debug, PartialEq)]
pub struct IrBlock {
    temporaries: Vec<(SmolStr, SmolStr)>,
    statements: Vec<IrItem>,
    pub(crate) return_var: Option<SmolStr>,
}

impl IntoIterator for IrBlock {
    type Item = IrItem;
    // the full beauty of rust
    type IntoIter = std::vec::IntoIter<IrItem>;
    fn into_iter(self) -> Self::IntoIter {
        let mut tmp_results = self
            .temporaries
            .into_iter()
            .map(|(name, ty)| IrItem::Declaration {
                typename: ty,
                var: name,
                value: None,
            })
            .collect::<Vec<_>>();
        tmp_results.extend(self.statements);

        tmp_results.into_iter()
    }
}

#[derive(Debug, PartialEq)]
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
            ast::BinaryOp::Assign => todo!(),
        }
    }
}
