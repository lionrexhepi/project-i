pub mod symbols;

use smol_str::SmolStr;
use symbols::{Symbol, SymbolTable};

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

#[derive(Debug, PartialEq)]
pub enum MangledItem {
    Print(MangledExpression),
    Declaration {
        ty: SmolStr,
        var: SmolStr,
        value: MangledExpression,
    },
}

#[derive(Debug, PartialEq)]
pub enum MangledExpression {
    LitInt(i64),
    LitBool(bool),
    Variable(SmolStr),
}

pub fn mangle(ast: Ast) -> MangledProgram {
    let mut symbols = SymbolTable::default();
    MangledProgram {
        items: ast
            .into_iter()
            .flat_map(|item| mangle_item(item, &mut symbols))
            .collect(),
    }
}

fn mangle_item(item: Item, symbols: &mut SymbolTable) -> Vec<MangledItem> {
    match item {
        Item::Print(expr) => vec![MangledItem::Print(mangle_expression(expr, symbols))],
        Item::Declaration { name, value } => {
            symbols.insert(&name, Symbol);
            vec![MangledItem::Declaration {
                ty: "int".into(),
                var: name,
                value: mangle_expression(value, symbols),
            }]
        }
    }
}

fn mangle_expression(expr: Expression, symbols: &mut SymbolTable) -> MangledExpression {
    match expr {
        Expression::LitInt(int) => MangledExpression::LitInt(int),
        Expression::LitBool(bool) => MangledExpression::LitBool(bool),
        Expression::Identifier(name) => {
            if symbols.get(&name).is_none() {
                panic!("undeclared variable: {}", name)
            }
            MangledExpression::Variable(name)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_mangle() {
        let ast = Ast::from([Item::Print(Expression::LitInt(42))]);
        let program = mangle(ast);
        assert_eq!(program.items.len(), 1);
        assert!(matches!(
            program.items[0],
            MangledItem::Print(MangledExpression::LitInt(42))
        ))
    }

    #[test]
    fn test_declare() {
        let mut symbols = SymbolTable::default();
        let declaration = Item::Declaration {
            name: "foo".into(),
            value: Expression::LitInt(42),
        };

        let items = mangle_item(declaration, &mut symbols);

        assert_eq!(items.len(), 1);
        assert_eq!(
            items[0],
            MangledItem::Declaration {
                ty: "int".into(),
                var: "foo".into(),
                value: MangledExpression::LitInt(42)
            }
        );
        assert_eq!(symbols.get("foo"), Some(&Symbol))
    }

    #[test]
    #[should_panic(expected = "undeclared variable: foo")]
    fn test_undeclared() {
        let mut symbols = SymbolTable::default();
        let expression = Expression::Identifier("foo".into());
        mangle_expression(expression, &mut symbols);
    }
}
