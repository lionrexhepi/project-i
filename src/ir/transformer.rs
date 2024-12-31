use crate::ast::{Ast, BinaryOp, Expression, FnArg, Item, While};

use super::{
    symbols::{Symbol, SymbolTable, TempId},
    types::{Signature, Type, TypeId},
    Error, IrBlock, IrItem, Result,
};

#[derive(Default)]
pub struct FileTransformer {
    ir_stack: Vec<Vec<IrItem>>,
    symbols: SymbolTable,
    errors: Vec<Error>,
}

impl FileTransformer {
    pub fn transform_file(mut self, file: Ast) -> std::result::Result<Vec<IrItem>, Vec<Error>> {
        for item in file.into_iter() {
            match self.process_item(item) {
                Ok((stmt, _)) => {
                    self.push_item(stmt);
                }
                Err(err) => {
                    self.errors.push(err);
                }
            };
        }

        assert_eq!(self.ir_stack.len(), 1);
        if self.errors.is_empty() {
            Ok(self.ir_stack.pop().unwrap())
        } else {
            Err(self.errors)
        }
    }

    #[inline(always)]
    fn assert_types(&self, expected: TypeId, found: TypeId) -> Result<()> {
        if expected != found {
            Err(Error::TypeMismatch {
                expected: self.symbols.resolve_type(expected).name().into(),
                found: self.symbols.resolve_type(found).name().into(),
            })
        } else {
            Ok(())
        }
    }

    #[inline(always)]
    fn push_item(&mut self, item: IrItem) {
        self.ir_stack
            .last_mut()
            .expect("Cannot pop top IR block")
            .push(item);
    }

    fn scoped<T>(&mut self, with: impl FnOnce(&mut Self) -> Result<T>) -> Result<(T, Vec<IrItem>)> {
        self.symbols.push_scope();
        self.ir_stack.push(Vec::new());
        let result = with(self)?;
        self.symbols.pop_scope();
        Ok((
            result,
            self.ir_stack.pop().expect("Cannot pop top IR block"),
        ))
    }
}

impl FileTransformer {
    fn process_item(&mut self, item: Item) -> Result<(IrItem, TypeId)> {
        match item {
            Item::Print(expr) => {
                let (expr, ty) = self.transform_expression(expr)?;
                Ok((IrItem::Print(Box::new(expr)), ty))
            }
            Item::Declaration {
                name,
                value,
                typename,
            } => {
                let named_type = match typename {
                    Some(typename) => match self.symbols.get(&typename) {
                        Some(Symbol::Type(ty)) => Some(*ty),
                        _ => return Err(Error::UndeclaredType { name: typename }),
                    },
                    None => None,
                };

                let (value, expr_ty) = self.transform_expression(value)?;

                if let Some(named_ty) = named_type {
                    self.assert_types(named_ty, expr_ty)?;
                }

                self.symbols.insert(&name, Symbol::Variable(expr_ty));

                let typename = self.symbols.resolve_type(expr_ty).c_name().into();

                Ok((
                    IrItem::Declaration {
                        typename,
                        var: name.into(),
                        value: Some(Box::new(value)),
                    },
                    TypeId::VOID,
                ))
            }
            Item::Expression(expr) => self.transform_expression(expr),
        }
    }

    fn transform_expression(&mut self, expr: Expression) -> Result<(IrItem, TypeId)> {
        match expr {
            Expression::LitInt(int) => Ok((IrItem::LitInt(int), TypeId::INT)),
            Expression::LitBool(bool) => Ok((IrItem::LitBool(bool), TypeId::BOOL)),
            Expression::Identifier(name) => {
                let Some(Symbol::Variable(ty)) = self.symbols.get(&name) else {
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
                        let Symbol::Type(ty) = self
                            .symbols
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
                            self.symbols
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

                let id = self.symbols.function_pointer(arg_types, return_type);

                if let Some(name) = name {
                    self.symbols.insert(&name, Symbol::Variable(id));
                }

                self.symbols.push_scope();

                for (name, ty) in args.iter() {
                    self.symbols.insert(name, Symbol::Variable(*ty));
                }

                let ret_temp = self.symbols.create_temporary();

                println!("Transform body");
                let (body, actual_return_type) = self.transform_block(body, true)?;

                let temporaries = self
                    .symbols
                    .pop_scope()
                    .into_iter()
                    .map(|(id, ty)| (id.get_name(), self.symbols.resolve_type(ty).c_name().into()))
                    .collect();
                self.assert_types(return_type, actual_return_type)?;

                let args = args
                    .into_iter()
                    .map(|(name, ty)| (name.into(), self.symbols.resolve_type(ty).c_name().into()))
                    .collect();

                Ok((
                    IrItem::Function {
                        body: IrBlock {
                            temporaries,
                            statements: vec![IrItem::Block(body)],
                            return_var: Some(ret_temp.get_name()),
                        },
                        args,
                        return_type: self.symbols.resolve_type(return_type).c_name().into(),
                    },
                    id,
                ))
                /* */
            }
            Expression::If(r#if) => self.transform_if(r#if),
            Expression::While(While { condition, body }) => {
                let (condition, cond_ty) = self.transform_expression(*condition)?;

                self.assert_types(TypeId::BOOL, cond_ty)?;

                let (body, _) = self.transform_block(body, false)?;

                Ok((
                    IrItem::Loop {
                        condition: Box::new(condition),
                        body,
                    },
                    TypeId::VOID,
                ))
            }
            Expression::Assign { var, value } => {
                let (rhs, rhs_ty) = self.transform_expression(*value)?;

                let Some(Symbol::Variable(ty)) = self.symbols.get(&var) else {
                    return Err(Error::UndeclaredVariable { var });
                };

                self.assert_types(*ty, rhs_ty)?;

                Ok((
                    IrItem::Assign {
                        var: var.into(),
                        value: Box::new(rhs),
                    },
                    TypeId::VOID,
                ))
            }
            Expression::Binary(binary) => {
                let (lhs, lhs_ty) = self.transform_expression(*binary.left)?;
                let (rhs, rhs_ty) = self.transform_expression(*binary.right)?;

                let out = match binary.op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                        self.assert_types(TypeId::INT, lhs_ty)?;
                        self.assert_types(TypeId::INT, rhs_ty)?;
                        TypeId::INT
                    }
                    BinaryOp::Eq | BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
                        self.assert_types(lhs_ty, rhs_ty)?;
                        TypeId::BOOL
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        self.assert_types(TypeId::BOOL, lhs_ty)?;
                        self.assert_types(TypeId::BOOL, rhs_ty)?;

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
                let Some(Symbol::Variable(id)) = self.symbols.get(&name) else {
                    return Err(Error::UndeclaredVariable { var: name });
                };
                let Type::Function(Signature {
                    args: expected,
                    ret,
                }) = self.symbols.resolve_type(*id).clone()
                else {
                    return Err(Error::FunctionNotFound { name });
                };

                let args = args
                    .into_iter()
                    .zip(expected)
                    .map(|(expr, expected_ty)| {
                        let (expr, ty) = self.transform_expression(expr)?;
                        self.assert_types(expected_ty, ty).map(|_| expr)
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

    fn transform_if(&self, r#if: crate::ast::If) -> Result<(IrItem, TypeId)> {
        todo!()
    }

    fn transform_block(
        &mut self,
        block: crate::ast::Block,
        store: bool,
    ) -> Result<(IrBlock, TypeId)> {
        let temp = if store {
            Some(self.symbols.create_temporary())
        } else {
            None
        };

        let mut ty = TypeId::VOID;

        let (_, statements) = self.scoped(|this| {
            let len = block.statements.len();
            for (i, item) in block.into_iter().enumerate() {
                match this.process_item(item) {
                    Ok((stmt, stmt_ty)) => {
                        if store && i == len - 1 {
                            ty = stmt_ty;
                            this.symbols.set_temporary_type(temp.unwrap(), ty);
                            this.push_item(IrItem::Assign {
                                var: temp.unwrap().get_name(),
                                value: Box::new(stmt),
                            });
                        } else {
                            this.push_item(stmt);
                        }
                    }
                    Err(err) => {
                        this.errors.push(err);
                    }
                }
            }
            Ok(())
        })?;

        let return_var = temp.map(TempId::get_name);

        Ok((
            IrBlock {
                temporaries: vec![],
                statements,
                return_var,
            },
            ty,
        ))
    }
}
