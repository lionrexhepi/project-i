use crate::ast::{BinaryOp, Block, Else, Expression, If, Item};

use super::{
    symbols::{Symbol, SymbolTable},
    types::{Type, TypeId},
};

pub fn infer_statement_type(item: &Item, symbols: &SymbolTable) -> TypeId {
    match item {
        Item::Expression(expr) => infer_expr_type(expr, symbols),
        _ => TypeId::VOID,
    }
}

pub fn infer_expr_type(expr: &Expression, symbols: &SymbolTable) -> TypeId {
    match expr {
        Expression::LitInt(_) => TypeId::INT,
        Expression::LitBool(_) => TypeId::BOOL,
        Expression::Identifier(name) => match symbols.get(name) {
            Some(Symbol::Variable(ty)) => *ty,
            _ => panic!("undeclared variable: {}", name),
        },
        Expression::Function { body: _ } => TypeId::FUNCTION,
        Expression::If(r#if) => infer_if_type(r#if, symbols),
        Expression::While(_) => TypeId::VOID,
        Expression::Binary(binary) => {
            let left_ty = infer_expr_type(&binary.left, symbols);
            let right_ty = infer_expr_type(&binary.right, symbols);

            assert_eq!(left_ty, right_ty);

            match binary.op {
                BinaryOp::Eq | BinaryOp::Lt | BinaryOp::Gt | BinaryOp::Le | BinaryOp::Ge => {
                    TypeId::BOOL
                }
                BinaryOp::Assign => unreachable!(),
                _ => left_ty,
            }
        }
        Expression::Call(name, _) => {
            let Some(Symbol::Variable(id)) = symbols.get(name) else {
                panic!("undeclared function")
            };
            let Type::Function { ret: ty, .. } = symbols.resolve_type(*id) else {
                panic!("expected function")
            };
            *ty
        }
    }
}

fn infer_if_type(r#if: &If, symbols: &SymbolTable) -> TypeId {
    let then_ty = infer_block_type(&r#if.then, symbols);
    let otherwise_ty = r#if
        .otherwise
        .as_ref()
        .map(|expr| match expr {
            Else::Block(block) => infer_block_type(block, symbols),
            Else::If(r#if) => infer_if_type(r#if, symbols),
        })
        .unwrap_or(TypeId::VOID);

    assert_eq!(then_ty, otherwise_ty);

    then_ty
}

pub fn infer_block_type(block: &Block, symbols: &SymbolTable) -> TypeId {
    if block.semicolon_terminated {
        TypeId::VOID
    } else {
        block
            .statements
            .last()
            .map_or(TypeId::VOID, |item| infer_statement_type(item, symbols))
    }
}
