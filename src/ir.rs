mod analyse;
mod inference;
pub mod symbols;
mod types;

use smol_str::SmolStr;

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
    tmp_results: Vec<(SmolStr, SmolStr)>,
    statements: Vec<IrItem>,
}

impl IntoIterator for IrBlock {
    type Item = IrItem;
    // the full beauty of rust
    type IntoIter = std::iter::Chain<std::vec::IntoIter<IrItem>, std::vec::IntoIter<IrItem>>;
    fn into_iter(self) -> Self::IntoIter {
        let tmp_results = self
            .tmp_results
            .into_iter()
            .map(|(name, ty)| IrItem::Declaration {
                typename: ty,
                var: name,
                value: None,
            })
            .collect::<Vec<_>>();

        tmp_results.into_iter().chain(self.statements)
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

pub use analyse::analyse;
