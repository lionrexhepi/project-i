//! Transform AST nodes into IR nodes.
use crate::ast::{Ast, BinaryOp, Block, Else, Expression, If, Item, While};

use super::{
    symbols::{Symbol, SymbolTable, TempId},
    types::{Type, TypeId},
    Ir, IrBlock, IrItem,
};

type AnnotatedItem = (IrItem, TypeId);
type AnnotatedBlock = (IrBlock, TypeId);

pub fn transform(ast: Ast) -> Ir {
    let mut symbols = SymbolTable::default();
    let mut items = Vec::with_capacity(ast.items().len());

    for item in ast.into_iter() {
        let (item, _) = transform_statement(item, &mut symbols);
        items.push(item);
    }
    Ir { items }
}

fn transform_statement(item: Item, symbols: &mut SymbolTable) -> AnnotatedItem {
    let mut prepend = vec![];
    let (item, ty) = match item {
        Item::Print(expr) => {
            let (expr, _) = transform_expression(expr, symbols, &mut prepend);

            (IrItem::Print(Box::new(expr)), TypeId::VOID)
        }
        Item::Declaration {
            name,
            value,
            typename,
        } => {
            let named_type = match typename {
                Some(typename) => match symbols.get(&typename) {
                    Some(Symbol::Type(ty)) => Some(*ty),
                    _ => panic!("undeclared type: {}", typename),
                },
                None => None,
            };

            let (value, expr_ty) = transform_expression(value, symbols, &mut prepend);

            if let Some(named_ty) = named_type {
                assert_eq!(named_ty, expr_ty);
            }

            symbols.insert(&name, Symbol::Variable(expr_ty));

            let typename = symbols.resolve_type(expr_ty).name().into();

            (
                IrItem::Declaration {
                    typename,
                    var: name,
                    value: Some(Box::new(value)),
                },
                TypeId::VOID,
            )
        }
        Item::Expression(expr) => transform_expression(expr, symbols, &mut prepend),
    };
    prepend.push(item);
    (IrItem::Multiple(prepend), ty)
}

fn transform_expression(
    expr: Expression,
    symbols: &mut SymbolTable,
    prepend: &mut Vec<IrItem>,
) -> AnnotatedItem {
    match expr {
        Expression::LitInt(int) => (IrItem::LitInt(int), TypeId::INT),
        Expression::LitBool(bool) => (IrItem::LitBool(bool), TypeId::BOOL),
        Expression::Identifier(name) => {
            let Some(Symbol::Variable(ty)) = symbols.get(&name) else {
                panic!("undeclared variable: {}", name)
            };
            (IrItem::Variable(name), *ty)
        }
        Expression::Function { body } => {
            let (body, _) = transform_block(body, symbols, None);
            (IrItem::Function { body }, TypeId::FUNCTION)
        }
        Expression::If(r#if) => transform_if(r#if, symbols, prepend, None),
        Expression::While(While { condition, body }) => {
            let (condition, cond_ty) = transform_expression(*condition, symbols, prepend);

            assert_eq!(cond_ty, TypeId::BOOL);

            let (body, _) = transform_block(body, symbols, None);

            (
                IrItem::Loop {
                    condition: Box::new(condition),
                    body,
                },
                TypeId::VOID,
            )
        }
        Expression::Assign { var, value } => {
            let (rhs, rhs_ty) = transform_expression(*value, symbols, prepend);

            let Some(Symbol::Variable(ty)) = symbols.get(&var) else {
                panic!("Undeclared variable")
            };

            assert_eq!(rhs_ty, *ty);

            (
                IrItem::Assign {
                    var,
                    value: Box::new(rhs),
                },
                TypeId::VOID,
            )
        }
        Expression::Binary(binary) => {
            let (lhs, lhs_ty) = transform_expression(*binary.left, symbols, prepend);
            let (rhs, rhs_ty) = transform_expression(*binary.right, symbols, prepend);

            let out = match binary.op {
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                    assert_eq!(lhs_ty, TypeId::INT);
                    assert_eq!(rhs_ty, TypeId::INT);
                    TypeId::INT
                }
                BinaryOp::Eq | BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
                    assert_eq!(lhs_ty, rhs_ty);
                    TypeId::BOOL
                }
                BinaryOp::And | BinaryOp::Or => {
                    assert_eq!(lhs_ty, TypeId::BOOL);
                    assert_eq!(rhs_ty, TypeId::BOOL);
                    TypeId::BOOL
                }
            };
            (
                IrItem::Op {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op: binary.op.into(),
                },
                out,
            )
        }
        Expression::Call(name, args) => {
            let Some(Symbol::Variable(id)) = symbols.get(&name) else {
                panic!("undeclared function")
            };
            let Type::Function {
                args: expected,
                ret,
            } = symbols.resolve_type(*id).clone()
            else {
                panic!("expected function")
            };

            let args = args
                .into_iter()
                .zip(expected)
                .map(|(expr, expected_ty)| {
                    let (expr, ty) = transform_expression(expr, symbols, prepend);
                    assert_eq!(ty, expected_ty);
                    expr
                })
                .collect();
            (IrItem::Call { name, args }, ret)
        }
    }
}

fn transform_if(
    r#if: If,
    symbols: &mut SymbolTable,
    prepend: &mut Vec<IrItem>,
    save_to: Option<TempId>,
) -> AnnotatedItem {
    let (condition, cond_ty) = transform_expression(*r#if.condition, symbols, prepend);

    assert_eq!(cond_ty, TypeId::BOOL);

    let temp = save_to.unwrap_or_else(|| symbols.create_temporary());

    let (then, then_ty) = transform_block(r#if.then, symbols, Some(temp));

    let otherwise = r#if
        .otherwise
        .map(|expr| match expr {
            Else::Block(block) => {
                let (mut block, ty) = transform_block(block, symbols, Some(temp));
                block.return_var = Some(temp.get_name());
                (IrItem::Block(block), ty)
            }
            Else::If(r#if) => transform_if(*r#if, symbols, prepend, Some(temp)),
        })
        .map(|(item, otherwise_ty)| {
            if then_ty != otherwise_ty {
                panic!(
                    "type mismatch: expected {:?}, got {:?}",
                    then_ty, otherwise_ty
                )
            }
            Box::new(item)
        });

    let r#if = IrItem::If {
        condition: Box::new(condition),
        then,
        otherwise,
    };
    if then_ty != TypeId::VOID {
        prepend.push(r#if);
        (IrItem::Variable(temp.get_name()), then_ty)
    } else {
        (r#if, then_ty)
    }
}

fn transform_block(
    block: Block,
    symbols: &mut SymbolTable,
    save_to: Option<TempId>,
) -> AnnotatedBlock {
    symbols.push_scope();
    let len = block.statements.len();
    let mut statements = Vec::with_capacity(if save_to.is_some() { len + 1 } else { len });
    let mut ty = TypeId::VOID;
    for (i, item) in block.statements.into_iter().enumerate() {
        let (statement, statement_ty) = transform_statement(item, symbols);

        if i == len - 1 {
            if let Some(temp) = save_to {
                statements.push(IrItem::Assign {
                    var: temp.get_name(),
                    value: Box::new(statement),
                });

                ty = statement_ty;
                break;
            }
        }
        statements.push(statement);
    }

    let tmp_decls = symbols
        .pop_scope()
        .into_iter()
        .map(|(id, ty)| (id.get_name(), symbols.resolve_type(ty).name().into()))
        .collect();

    if let Some(save_to) = save_to {
        symbols.set_temporary_type(save_to, ty);
    }

    (
        IrBlock {
            temporaries: tmp_decls,
            statements,
            return_var: save_to.map(|id| id.get_name()),
        },
        ty,
    )
}

#[cfg(test)]
mod test {
    use crate::ir::types::TypeId;

    use super::*;

    #[test]
    fn test_mangle() {
        let ast = Ast::from([Item::Print(Expression::LitInt(42))]);
        let program = transform(ast);
        assert_eq!(program.items.len(), 1);
        assert_eq!(
            program.items[0],
            IrItem::Print(Box::new(IrItem::LitInt(42)))
        );
    }

    #[test]
    fn test_declare() {
        let mut symbols = SymbolTable::default();
        let declaration = Item::Declaration {
            name: "foo".into(),
            typename: Some("i32".into()),
            value: Expression::LitInt(42),
        };

        let transformed = transform_statement(declaration, &mut symbols);

        assert_eq!(
            transformed.0,
            IrItem::Declaration {
                typename: "int".into(),
                var: "foo".into(),
                value: Some(Box::new(IrItem::LitInt(42)))
            }
        );
        assert_eq!(symbols.get("foo"), Some(&Symbol::Variable(TypeId::INT)))
    }

    #[test]
    #[should_panic(expected = "undeclared variable: foo")]
    fn test_undeclared() {
        let mut symbols = SymbolTable::default();
        let expression = Expression::Identifier("foo".into());
        transform_expression(expression, &mut symbols, &mut vec![]);
    }
}
