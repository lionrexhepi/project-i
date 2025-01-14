#![deny(clippy::unwrap_used, unused_results)]
use std::io::{self, Write};

use crate::ir::{Ir, IrItem, Operator};

pub fn write_c(program: Ir, to: &mut impl Write) -> io::Result<()> {
    for item in program.items {
        write_item(item, to)?;
        to.write_all(b";")?;
    }

    Ok(())
}

fn write_item(item: IrItem, to: &mut impl Write) -> io::Result<()> {
    match item {
        IrItem::Print(expr) => {
            to.write_all(b"printf(\"%ld\\n\", ")?;
            write_item(*expr, to)?;
            to.write_all(b");")
        }
        IrItem::Declaration {
            typename: _,
            var,
            value: Some(value),
        } if matches!(&*value, IrItem::Function { .. }) => {
            let IrItem::Function {
                body,
                return_type,
                args,
            } = *value
            else {
                unreachable!()
            };

            let args = args
                .into_iter()
                .map(|(name, ty)| format!("{} {}", ty, name))
                .collect::<Vec<_>>()
                .join(", ");

            write!(to, "{} {}({}){{", return_type, var, args)?;
            for item in body {
                write_item(item, to)?;
                to.write_all(b";")?;
            }
            to.write_all(b"}")
        }
        IrItem::Declaration {
            typename: ty,
            var,
            value,
        } => {
            write!(to, "{} {}", ty, var)?;
            if let Some(value) = value {
                to.write_all(b" = ")?;
                write_item(*value, to)?;
            }
            to.write_all(b";")
        }
        IrItem::LitInt(i) => write!(to, "{}", i),
        IrItem::LitBool(b) => write!(to, "{}", b as u8),
        IrItem::Variable(smol_str) => write!(to, "{}", smol_str),
        IrItem::Block(block) => {
            to.write_all(b"{")?;
            for item in block {
                write_item(item, to)?;

                to.write_all(b";")?;
            }
            to.write_all(b"}")
        }
        IrItem::If {
            branches,
            otherwise,
        } => {
            let len = branches.len();
            for (i, branch) in branches.into_iter().enumerate() {
                to.write_all(b"if (").unwrap();
                write_item(branch.condition, to)?;
                to.write_all(b")")?;
                write_item(IrItem::Block(branch.then), to)?;

                if i < len - 1 {
                    to.write_all(b"else ")?;
                }
            }

            if let Some(otherwise) = otherwise {
                to.write_all(b"else").unwrap();
                write_item(IrItem::Block(otherwise), to)?;
            }
            Ok(())
        }
        IrItem::Loop { condition, body } => {
            to.write_all(b"while (")?;
            write_item(*condition, to)?;
            to.write_all(b")")?;
            write_item(IrItem::Block(body), to)
        }
        IrItem::Function { .. } => unreachable!(),
        IrItem::Op { lhs, rhs, op } => {
            let op = match op {
                Operator::Add => "+",
                Operator::Sub => "-",
                Operator::Mul => "*",
                Operator::Div => "/",
                Operator::Eq => "==",
                Operator::Neq => "!=",
                Operator::Lt => "<",
                Operator::Lte => "<=",
                Operator::Gt => ">",
                Operator::Gte => ">=",
                Operator::And => "&&",
                Operator::Or => "||",
            };
            write_item(*lhs, to)?;
            to.write_all(op.as_bytes()).unwrap();
            write_item(*rhs, to)
        }
        IrItem::Assign { var, value } => {
            write!(to, "{var} = ").unwrap();
            write_item(*value, to)?;
            to.write_all(b";")
        }
        IrItem::Call { name, args } => {
            write!(to, "{}(", name).unwrap();
            let len = args.len();
            for (i, arg) in args.into_iter().enumerate() {
                write_item(arg, to)?;
                if i < len - 1 {
                    to.write_all(b", ").unwrap();
                }
            }
            to.write_all(b")")
        }
        IrItem::Multiple(items) => {
            for item in items {
                write_item(item, to)?;
            }
            Ok(())
        }
        IrItem::Return(item) => {
            to.write_all(b"return ").unwrap();
            if let Some(val) = item {
                write_item(*val, to)?;
            }
            Ok(())
        }
    }
}

#[cfg(test)]
mod test {
    #![allow(clippy::unwrap_used)]
    use core::str;

    use super::*;

    #[test]
    fn test_print_int() {
        let program = Ir::from([IrItem::Print(Box::new(IrItem::LitInt(42)))]);
        let mut buf = Vec::new();
        write_c(program, &mut buf).unwrap();
        assert_eq!(buf, b"printf(\"%ld\\n\", 42);");
    }

    #[test]
    fn test_print_bool() {
        let program = Ir::from([IrItem::Print(Box::new(IrItem::LitBool(true)))]);
        let mut buf = Vec::new();
        write_c(program, &mut buf).unwrap();
        assert_eq!(str::from_utf8(&buf).unwrap(), "printf(\"%ld\\n\", 1);");
    }

    #[test]
    fn test_declare_int() {
        let program = Ir::from([IrItem::Declaration {
            typename: "int".into(),
            var: "foo".into(),
            value: Some(Box::new(IrItem::LitInt(42))),
        }]);
        let mut buf = Vec::new();
        write_c(program, &mut buf).unwrap();
        assert_eq!(buf, b"int foo = 42;")
    }
}
