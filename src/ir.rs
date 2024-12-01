use crate::ast::{Ast, Expression, Item};

pub struct MangledProgram {
    pub items: Vec<MangledItem>,
}

#[cfg(test)]
impl<I> From<I> for MangledProgram
where
    I: IntoIterator<Item = MangledItem>,
{
    fn from(items: I) -> Self {
        MangledProgram {
            items: items.into_iter().collect(),
        }
    }
}

pub enum MangledItem {
    Print(MangledExpression),
}

pub enum MangledExpression {
    Literal(i64),
}

pub fn mangle(ast: Ast) -> MangledProgram {
    MangledProgram {
        items: ast.into_iter().flat_map(mangle_item).collect(),
    }
}

fn mangle_item(item: Item) -> Vec<MangledItem> {
    match item {
        Item::Print(expr) => vec![MangledItem::Print(mangle_expression(expr))],
    }
}

fn mangle_expression(expr: Expression) -> MangledExpression {
    match expr {
        Expression::Literal(lit) => MangledExpression::Literal(lit),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_mangle() {
        let ast = Ast::from([Item::Print(Expression::Literal(42))]);
        let program = mangle(ast);
        assert_eq!(program.items.len(), 1);
        assert!(matches!(
            program.items[0],
            MangledItem::Print(MangledExpression::Literal(42))
        ))
    }
}
