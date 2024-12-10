use crate::{
    ast::{Ast, BinaryOp, Block, Else, Expression, If, Item, While},
    ir::Operator,
};

use super::{
    inference::infer_expr_type,
    symbols::{Symbol, SymbolTable},
    types::{Type, TypeId},
    Ir, IrBlock, IrItem,
};

pub fn analyse(ast: Ast) -> Ir {
    let mut symbols = SymbolTable::default();
    Ir {
        items: ast
            .into_iter()
            .flat_map(|item| analyze_statement(item, &mut symbols))
            .collect(),
    }
}

fn analyze_statement(item: Item, symbols: &mut SymbolTable) -> Vec<IrItem> {
    match item {
        Item::Print(expr) => vec![IrItem::Print(Box::new(analyse_expression(expr, symbols)))],
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

            let expr_ty = infer_expr_type(&value, symbols);

            if let Some(named_type) = named_type {
                if named_type != expr_ty {
                    panic!(
                        "type mismatch: expected {:?}, got {:?}",
                        named_type, expr_ty
                    )
                }
            }

            symbols.insert(&name, Symbol::Variable(expr_ty));

            let typename = symbols.resolve_type(expr_ty).name();

            vec![IrItem::Declaration {
                typename: typename.into(),
                var: name,
                value: Some(Box::new(analyse_expression(value, symbols))),
            }]
        }
        Item::Expression(expr) => vec![analyse_expression(expr, symbols)],
    }
}

fn analyse_expression(expr: Expression, symbols: &mut SymbolTable) -> IrItem {
    match expr {
        Expression::LitInt(int) => IrItem::LitInt(int),
        Expression::LitBool(bool) => IrItem::LitBool(bool),
        Expression::Identifier(name) => {
            if symbols.get(&name).is_none() {
                panic!("undeclared variable: {}", name)
            }
            IrItem::Variable(name)
        }
        Expression::Function { body } => IrItem::Function {
            body: analyze_block(body, symbols),
        },
        Expression::If(r#if) => analyse_if(r#if, symbols),
        Expression::While(While { condition, body }) => {
            if infer_expr_type(&condition, symbols) != TypeId::BOOL {
                panic!("expected boolean expression")
            }

            let condition = Box::new(analyse_expression(*condition, symbols));

            let body = analyze_block(body, symbols);

            IrItem::Loop { condition, body }
        }
        Expression::Binary(assignment) if assignment.op == BinaryOp::Assign => {
            let Expression::Identifier(variable) = *assignment.left else {
                panic!("Cannot assign to a value")
            };

            let Some(Symbol::Variable(ty)) = symbols.get(&variable) else {
                panic!("Undeclared variable")
            };

            if infer_expr_type(&assignment.right, symbols) != *ty {
                panic!("Invalid type")
            }

            let rhs = analyse_expression(*assignment.right, symbols);

            IrItem::Assign {
                var: variable,
                value: Box::new(rhs),
            }
        }
        Expression::Binary(binary) => {
            let lhs_ty = infer_expr_type(&binary.left, symbols);
            let rhs_ty = infer_expr_type(&binary.right, symbols);

            let op = match binary.op {
                BinaryOp::Add => {
                    assert!(lhs_ty == rhs_ty && lhs_ty == TypeId::INT);
                    Operator::Add
                }
                BinaryOp::Sub => {
                    assert!(lhs_ty == rhs_ty && lhs_ty == TypeId::INT);
                    Operator::Sub
                }
                BinaryOp::Mul => {
                    assert!(lhs_ty == rhs_ty && lhs_ty == TypeId::INT);
                    Operator::Mul
                }
                BinaryOp::Div => {
                    assert!(lhs_ty == rhs_ty && lhs_ty == TypeId::INT);
                    Operator::Div
                }
                BinaryOp::Eq => {
                    assert!(lhs_ty == rhs_ty);
                    Operator::Eq
                }
                BinaryOp::Lt => {
                    assert!(lhs_ty == rhs_ty && lhs_ty == TypeId::INT);
                    Operator::Lt
                }
                BinaryOp::Gt => {
                    assert!(lhs_ty == rhs_ty && lhs_ty == TypeId::INT);
                    Operator::Gt
                }
                BinaryOp::Le => {
                    assert!(lhs_ty == rhs_ty && lhs_ty == TypeId::INT);
                    Operator::Lte
                }
                BinaryOp::Ge => {
                    assert!(lhs_ty == rhs_ty && lhs_ty == TypeId::INT);
                    Operator::Gte
                }
                BinaryOp::And => {
                    assert!(lhs_ty == rhs_ty && lhs_ty == TypeId::BOOL);
                    Operator::And
                }
                BinaryOp::Or => {
                    assert!(lhs_ty == rhs_ty && lhs_ty == TypeId::BOOL);
                    Operator::Or
                }
                BinaryOp::Assign => unreachable!(),
            };
            IrItem::Op {
                lhs: Box::new(analyse_expression(*binary.left, symbols)),
                rhs: Box::new(analyse_expression(*binary.right, symbols)),
                op,
            }
        }
        Expression::Call(name, args) => {
            let Some(Symbol::Variable(id)) = symbols.get(&name) else {
                panic!("undeclared function")
            };
            let expected = {
                let Type::Function { args, .. } = symbols.resolve_type(*id) else {
                    panic!("expected function")
                };
                args.clone() // TODO figure out a way to do this without a clone
            };

            let args = args
                .into_iter()
                .zip(expected)
                .map(|(expr, expected_ty)| {
                    if infer_expr_type(&expr, symbols) != expected_ty {
                        panic!("type mismatch")
                    }
                    analyse_expression(expr, symbols)
                })
                .collect();
            IrItem::Call { name, args }
        }
    }
}

fn analyse_if(r#if: If, symbols: &mut SymbolTable) -> IrItem {
    if infer_expr_type(&r#if.condition, symbols) != TypeId::BOOL {
        panic!("expected boolean expression")
    }

    let condition = Box::new(analyse_expression(*r#if.condition, symbols));

    let then = analyze_block(r#if.then, symbols);

    let otherwise = r#if
        .otherwise
        .map(|expr| match expr {
            Else::Block(block) => IrItem::Block(analyze_block(block, symbols)),
            Else::If(r#if) => analyse_if(*r#if, symbols),
        })
        .map(Box::new);
    IrItem::If {
        condition,
        then,
        otherwise,
    }
}

fn analyze_block(block: Block, symbols: &mut SymbolTable) -> IrBlock {
    symbols.push_scope();
    let statements = block
        .into_iter()
        .flat_map(|item| analyze_statement(item, symbols))
        .collect();

    let tmp_results = symbols
        .pop_scope()
        .map(|(id, ty)| {
            let name = id.get_name();
            let ty = symbols.resolve_type(ty).name().into();
            (name, ty)
        })
        .collect();
    IrBlock {
        tmp_results,
        statements,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_mangle() {
        let ast = Ast::from([Item::Print(Expression::LitInt(42))]);
        let program = analyse(ast);
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

        let items = analyze_statement(declaration, &mut symbols);

        assert_eq!(items.len(), 1);
        assert_eq!(
            items[0],
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
        analyse_expression(expression, &mut symbols);
    }
}
