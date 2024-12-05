pub mod symbols;

use smol_str::SmolStr;
use symbols::{Symbol, SymbolTable, Type};

use crate::ast::{Ast, Block, Else, Expression, If, Item, While};

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
    Print(Box<MangledItem>),
    Declaration {
        typename: SmolStr,
        var: SmolStr,
        value: Box<MangledItem>,
    },
    LitInt(i64),
    LitBool(bool),
    Variable(SmolStr),
    Function {
        body: Vec<MangledItem>,
    },
    If {
        condition: Box<MangledItem>,
        then: Vec<MangledItem>,
        otherwise: Option<Box<MangledItem>>,
    },
    Loop {
        condition: Box<MangledItem>,
        body: Vec<MangledItem>,
    },
    Block(Vec<MangledItem>),
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
        Item::Print(expr) => vec![MangledItem::Print(Box::new(mangle_expression(
            expr, symbols,
        )))],
        Item::Declaration {
            name,
            typename,
            value,
        } => {
            let named_type = match typename {
                Some(typename) => match symbols.get(&typename) {
                    Some(Symbol::Type(ty)) => Some(*ty),
                    _ => panic!("undeclared type: {}", typename),
                },
                None => None,
            };

            let expr_ty = infer_type(&value, symbols);

            if let Some(named_type) = named_type {
                if named_type != expr_ty {
                    panic!(
                        "type mismatch: expected {:?}, got {:?}",
                        named_type, expr_ty
                    )
                }
            }

            symbols.insert(&name, Symbol::Variable(expr_ty));
            vec![MangledItem::Declaration {
                typename: expr_ty.name().into(),
                var: name,
                value: Box::new(mangle_expression(value, symbols)),
            }]
        }
        Item::Expression(expr) => vec![mangle_expression(expr, symbols)],
    }
}

fn infer_type(expr: &Expression, symbols: &SymbolTable) -> Type {
    match expr {
        Expression::LitInt(_) => Type::Int,
        Expression::LitBool(_) => Type::Bool,
        Expression::Identifier(name) => match symbols.get(name) {
            Some(Symbol::Variable(ty)) => *ty,
            _ => panic!("undeclared variable: {}", name),
        },
        Expression::Function { body: _ } => Type::Function,
        Expression::If(_) => todo!(),
        Expression::While(_) => todo!(),
    }
}

fn mangle_expression(expr: Expression, symbols: &mut SymbolTable) -> MangledItem {
    match expr {
        Expression::LitInt(int) => MangledItem::LitInt(int),
        Expression::LitBool(bool) => MangledItem::LitBool(bool),
        Expression::Identifier(name) => {
            if symbols.get(&name).is_none() {
                panic!("undeclared variable: {}", name)
            }
            MangledItem::Variable(name)
        }
        Expression::Function { body } => MangledItem::Function {
            body: mangle_block(body, symbols),
        },
        Expression::If(r#if) => mangle_if(r#if, symbols),
        Expression::While(While { condition, body }) => {
            if infer_type(&condition, symbols) != Type::Bool {
                panic!("expected boolean expression")
            }

            let condition = Box::new(mangle_expression(*condition, symbols));

            let body = mangle_block(body, symbols);

            MangledItem::Loop { condition, body }
        }
    }
}

fn mangle_if(r#if: If, symbols: &mut SymbolTable) -> MangledItem {
    if infer_type(&r#if.condition, symbols) != Type::Bool {
        panic!("expected boolean expression")
    }

    let condition = Box::new(mangle_expression(*r#if.condition, symbols));

    let then = mangle_block(r#if.then, symbols);

    let otherwise = r#if
        .otherwise
        .map(|expr| match expr {
            Else::Block(block) => MangledItem::Block(mangle_block(block, symbols)),
            Else::If(r#if) => mangle_if(*r#if, symbols),
        })
        .map(Box::new);

    MangledItem::If {
        condition,
        then,
        otherwise,
    }
}

fn mangle_block(block: Block, symbols: &mut SymbolTable) -> Vec<MangledItem> {
    block
        .into_iter()
        .flat_map(|item| mangle_item(item, symbols))
        .collect()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_mangle() {
        let ast = Ast::from([Item::Print(Expression::LitInt(42))]);
        let program = mangle(ast);
        assert_eq!(program.items.len(), 1);
        assert_eq!(
            program.items[0],
            MangledItem::Print(Box::new(MangledItem::LitInt(42)))
        );
    }

    #[test]
    fn test_declare() {
        let mut symbols = SymbolTable::default();
        let declaration = Item::Declaration {
            name: "foo".into(),
            typename: Some("int".into()),
            value: Expression::LitInt(42),
        };

        let items = mangle_item(declaration, &mut symbols);

        assert_eq!(items.len(), 1);
        assert_eq!(
            items[0],
            MangledItem::Declaration {
                typename: "int".into(),
                var: "foo".into(),
                value: Box::new(MangledItem::LitInt(42))
            }
        );
        assert_eq!(
            symbols.get("foo"),
            Some(&Symbol::Variable(symbols::Type::Int))
        )
    }

    #[test]
    #[should_panic(expected = "undeclared variable: foo")]
    fn test_undeclared() {
        let mut symbols = SymbolTable::default();
        let expression = Expression::Identifier("foo".into());
        mangle_expression(expression, &mut symbols);
    }
}
