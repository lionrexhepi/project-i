//! Transform AST nodes into IR nodes.
use crate::ast::{Ast, BinaryOp, Block, Else, Expression, FnArg, If, Item, While};

use super::{
    symbols::{Symbol, SymbolTable, TempId},
    types::{Signature, Type, TypeId},
    Error, Ir, IrBlock, IrItem, Result,
};

type AnnotatedItem = (IrItem, TypeId);
type AnnotatedBlock = (IrBlock, TypeId);

macro_rules! assert_types {
    ($symbol_table:ident, $ty1:expr,$ty2:expr) => {
        if $ty1 != $ty2 {
            return Err(Error::TypeMismatch {
                expected: $symbol_table.resolve_type($ty1).name().into(),
                found: $symbol_table.resolve_type($ty2).name().into(),
            });
        }
    };
}

pub fn transform(ast: Ast) -> Result<Ir> {
    let mut symbols = SymbolTable::default();
    let mut items = Vec::with_capacity(ast.items().len());

    for item in ast.into_iter() {
        let (item, _) = transform_statement(item, &mut symbols)?;
        items.push(item);
    }
    Ok(Ir { items })
}

fn transform_statement(item: Item, symbols: &mut SymbolTable) -> Result<AnnotatedItem> {
    let mut prepend = vec![];
    let (item, ty) = match item {
        Item::Print(expr) => {
            let (expr, _) = transform_expression(expr, symbols, &mut prepend)?;

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
                    _ => return Err(Error::UndeclaredType { name: typename }),
                },
                None => None,
            };

            let (value, expr_ty) = transform_expression(value, symbols, &mut prepend)?;

            if let Some(named_ty) = named_type {
                assert_types!(symbols, named_ty, expr_ty);
            }

            symbols.insert(&name, Symbol::Variable(expr_ty));

            let typename = symbols.resolve_type(expr_ty).c_name().into();

            (
                IrItem::Declaration {
                    typename,
                    var: name.into(),
                    value: Some(Box::new(value)),
                },
                TypeId::VOID,
            )
        }
        Item::Expression(expr) => transform_expression(expr, symbols, &mut prepend)?,
    };
    prepend.push(item);
    Ok((IrItem::Multiple(prepend), ty))
}

fn transform_expression(
    expr: Expression,
    symbols: &mut SymbolTable,
    prepend: &mut Vec<IrItem>,
) -> Result<AnnotatedItem> {
    match expr {
        Expression::LitInt(int) => Ok((IrItem::LitInt(int), TypeId::INT)),
        Expression::LitBool(bool) => Ok((IrItem::LitBool(bool), TypeId::BOOL)),
        Expression::Identifier(name) => {
            let Some(Symbol::Variable(ty)) = symbols.get(&name) else {
                return Err(Error::UndeclaredVariable { var: name });
            };
            Ok((IrItem::Variable(name.into()), *ty))
        }
        Expression::Function {
            name,
            body,
            args,
            return_type,
        } => {
            let return_type = return_type
                .map(|name| {
                    let Symbol::Type(ty) = symbols
                        .get(&name)
                        .ok_or_else(|| Error::UndeclaredType { name: name.clone() })?
                    else {
                        return Err(Error::UndeclaredType { name });
                    };
                    Ok(*ty)
                })
                .transpose()?
                .unwrap_or(TypeId::VOID);

            let args = args
                .into_iter()
                .map(|FnArg { name, typename }| {
                    let Symbol::Type(ty) =
                        symbols
                            .get(&typename)
                            .ok_or_else(|| Error::UndeclaredType {
                                name: typename.clone(),
                            })?
                    else {
                        return Err(Error::UndeclaredType { name: typename });
                    };
                    Ok((name, *ty))
                })
                .collect::<Result<Vec<_>>>()?;

            let arg_types = args.iter().map(|(_, ty)| *ty).collect();

            let id = symbols.function_pointer(arg_types, return_type);

            if let Some(name) = name {
                symbols.insert(&name, Symbol::Variable(id));
            }

            symbols.push_scope();

            for (name, ty) in args.iter() {
                symbols.insert(name, Symbol::Variable(*ty));
            }

            let ret_temp = symbols.create_temporary();

            println!("Transform body");
            let (body, actual_return_type) = transform_block(body, symbols, Some(ret_temp))?;
            println!("Body transformed. actual_return_type: {actual_return_type:?}");

            symbols.pop_scope();

            assert_types!(symbols, return_type, actual_return_type);

            let args = args
                .into_iter()
                .map(|(name, ty)| (name.into(), symbols.resolve_type(ty).c_name().into()))
                .collect();

            Ok((
                IrItem::Function {
                    body,
                    args,
                    return_type: symbols.resolve_type(return_type).c_name().into(),
                },
                id,
            ))
        }
        Expression::If(r#if) => transform_if(r#if, symbols, prepend, None),
        Expression::While(While { condition, body }) => {
            let (condition, cond_ty) = transform_expression(*condition, symbols, prepend)?;

            assert_types!(symbols, cond_ty, TypeId::BOOL);

            let (body, _) = transform_block(body, symbols, None)?;

            Ok((
                IrItem::Loop {
                    condition: Box::new(condition),
                    body,
                },
                TypeId::VOID,
            ))
        }
        Expression::Assign { var, value } => {
            let (rhs, rhs_ty) = transform_expression(*value, symbols, prepend)?;

            let Some(Symbol::Variable(ty)) = symbols.get(&var) else {
                return Err(Error::UndeclaredVariable { var });
            };

            assert_types!(symbols, rhs_ty, *ty);

            Ok((
                IrItem::Assign {
                    var: var.into(),
                    value: Box::new(rhs),
                },
                TypeId::VOID,
            ))
        }
        Expression::Binary(binary) => {
            let (lhs, lhs_ty) = transform_expression(*binary.left, symbols, prepend)?;
            let (rhs, rhs_ty) = transform_expression(*binary.right, symbols, prepend)?;

            let out = match binary.op {
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                    assert_types!(symbols, lhs_ty, TypeId::INT);
                    assert_types!(symbols, rhs_ty, TypeId::INT);
                    TypeId::INT
                }
                BinaryOp::Eq | BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
                    assert_types!(symbols, lhs_ty, rhs_ty);
                    TypeId::BOOL
                }
                BinaryOp::And | BinaryOp::Or => {
                    assert_types!(symbols, lhs_ty, TypeId::BOOL);
                    assert_types!(symbols, rhs_ty, TypeId::BOOL);
                    TypeId::BOOL
                }
            };
            Ok((
                IrItem::Op {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op: binary.op.into(),
                },
                out,
            ))
        }
        Expression::Call(name, args) => {
            let Some(Symbol::Variable(id)) = symbols.get(&name) else {
                return Err(Error::UndeclaredVariable { var: name });
            };
            let Type::Function(Signature {
                args: expected,
                ret,
            }) = symbols.resolve_type(*id).clone()
            else {
                return Err(Error::FunctionNotFound { name });
            };

            let args = args
                .into_iter()
                .zip(expected)
                .map(|(expr, expected_ty)| {
                    let (expr, ty) = transform_expression(expr, symbols, prepend)?;
                    assert_types!(symbols, ty, expected_ty);
                    Ok(expr)
                })
                .collect::<Result<Vec<_>>>()?;
            Ok((
                IrItem::Call {
                    name: name.into(),
                    args,
                },
                ret,
            ))
        }
    }
}

fn transform_if(
    r#if: If,
    symbols: &mut SymbolTable,
    prepend: &mut Vec<IrItem>,
    save_to: Option<TempId>,
) -> Result<AnnotatedItem> {
    let (condition, cond_ty) = transform_expression(*r#if.condition, symbols, prepend)?;

    assert_types!(symbols, cond_ty, TypeId::BOOL);

    let temp = save_to.unwrap_or_else(|| symbols.create_temporary());

    let (then, then_ty) = transform_block(r#if.then, symbols, Some(temp))?;

    let otherwise = r#if
        .otherwise
        .map(|expr| match expr {
            Else::Block(block) => {
                let (mut block, ty) = transform_block(block, symbols, Some(temp))?;
                block.return_var = Some(temp.get_name());
                Ok((IrItem::Block(block), ty))
            }
            Else::If(r#if) => transform_if(*r#if, symbols, prepend, Some(temp)),
        })
        .transpose()?
        .map(|(item, otherwise_ty)| {
            assert_types!(symbols, then_ty, otherwise_ty);
            Ok(Box::new(item))
        })
        .transpose()?;

    let r#if = IrItem::If {
        condition: Box::new(condition),
        then,
        otherwise,
    };
    if then_ty != TypeId::VOID {
        prepend.push(r#if);
        Ok((IrItem::Variable(temp.get_name()), then_ty))
    } else {
        Ok((r#if, then_ty))
    }
}

fn transform_block(
    block: Block,
    symbols: &mut SymbolTable,
    save_to: Option<TempId>,
) -> Result<AnnotatedBlock> {
    symbols.push_scope();
    println!("Beginning block transform");
    let len = block.statements.len();
    let mut statements = Vec::with_capacity(if save_to.is_some() { len + 1 } else { len });
    let mut ty = TypeId::VOID;
    for (i, item) in block.statements.into_iter().enumerate() {
        println!("Beginning statement #{i}");
        let (statement, statement_ty) = transform_statement(item, symbols)?;
        println!("Ending statement #{i}. statement_ty: {statement_ty:?}");

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
        .map(|(id, ty)| (id.get_name(), symbols.resolve_type(ty).c_name().into()))
        .collect();

    if let Some(save_to) = save_to {
        symbols.set_temporary_type(save_to, ty);
    }
    println!("Block transform finished. ty:{ty:?}");

    Ok((
        IrBlock {
            temporaries: tmp_decls,
            statements,
            return_var: save_to.map(|id| id.get_name()),
        },
        ty,
    ))
}

#[cfg(test)]
mod test {
    use crate::ir::types::TypeId;

    use super::*;

    #[test]
    fn test_mangle() {
        let ast = Ast::from([Item::Print(Expression::LitInt(42))]);
        let program = transform(ast).unwrap();
        assert_eq!(program.items.len(), 1);
        assert_eq!(
            program.items[0],
            IrItem::Multiple(vec![IrItem::Print(Box::new(IrItem::LitInt(42)))])
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

        let transformed = transform_statement(declaration, &mut symbols).unwrap();

        assert_eq!(
            transformed.0,
            IrItem::Multiple(vec![IrItem::Declaration {
                typename: "int".into(),
                var: "foo".into(),
                value: Some(Box::new(IrItem::LitInt(42)))
            }])
        );
        assert_eq!(symbols.get("foo"), Some(&Symbol::Variable(TypeId::INT)))
    }

    #[test]
    #[should_panic(expected = "UndeclaredVariable")]
    fn test_undeclared() {
        let mut symbols = SymbolTable::default();
        let expression = Expression::Identifier("foo".into());
        transform_expression(expression, &mut symbols, &mut vec![]).unwrap();
    }
}
