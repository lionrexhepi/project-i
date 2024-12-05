use std::io::Write;

use crate::ir::{MangledItem, MangledProgram};

pub fn write_c(program: MangledProgram, to: &mut impl Write) {
    for item in program.items {
        write_item(item, to);
    }
}

fn write_item(item: MangledItem, to: &mut impl Write) {
    match item {
        MangledItem::Print(expr) => {
            to.write_all(b"printf(\"%ld\\n\", ").unwrap();
            write_item(*expr, to);
            to.write_all(b");").unwrap();
        }
        MangledItem::Declaration {
            typename: _,
            var,
            value,
        } if matches!(&*value, MangledItem::Function { .. }) => {
            let MangledItem::Function { body } = *value else {
                unreachable!()
            };
            write!(to, "int {}(){{", var).unwrap();
            for item in body {
                write_item(item, to);
            }
            to.write_all(b"return 0;}").unwrap();
        }
        MangledItem::Declaration {
            typename: ty,
            var,
            value,
        } => {
            write!(to, "{} {} = ", ty, var).unwrap();
            write_item(*value, to);
            to.write_all(b";").unwrap();
        }
        MangledItem::LitInt(i) => write!(to, "{}", i).unwrap(),
        MangledItem::LitBool(b) => write!(to, "{}", b as u8).unwrap(),
        MangledItem::Variable(smol_str) => write!(to, "{}", smol_str).unwrap(),
        MangledItem::Block(block) => {
            to.write_all(b"{").unwrap();
            for item in block {
                write_item(item, to);
            }
            to.write_all(b"}").unwrap();
        }
        MangledItem::If {
            condition,
            then,
            otherwise,
        } => {
            to.write_all(b"if (").unwrap();
            write_item(*condition, to);
            to.write_all(b")").unwrap();
            write_item(MangledItem::Block(then), to);
            if let Some(otherwise) = otherwise {
                to.write_all(b"else").unwrap();
                write_item(*otherwise, to);
            }
        }
        MangledItem::Loop { condition, body } => {
            to.write_all(b"while (").unwrap();
            write_item(*condition, to);
            to.write_all(b")").unwrap();
            write_item(MangledItem::Block(body), to);
            
        },
        MangledItem::Function { .. } => unreachable!(),
        
    }
}

#[cfg(test)]
mod test {
    use core::str;

    use super::*;

    #[test]
    fn test_print_int() {
        let program = MangledProgram::from([MangledItem::Print(Box::new(MangledItem::LitInt(42)))]);
        let mut buf = Vec::new();
        write_c(program, &mut buf);
        assert_eq!(buf, b"printf(\"%ld\\n\", 42);");
    }

    #[test]
    fn test_print_bool() {
        let program =
            MangledProgram::from([MangledItem::Print(Box::new(MangledItem::LitBool(true)))]);
        let mut buf = Vec::new();
        write_c(program, &mut buf);
        assert_eq!(str::from_utf8(&buf).unwrap(), "printf(\"%ld\\n\", 1);");
    }

    #[test]
    fn test_declare_int() {
        let program = MangledProgram::from([MangledItem::Declaration {
            typename: "int".into(),
            var: "foo".into(),
            value: Box::new(MangledItem::LitInt(42)),
        }]);
        let mut buf = Vec::new();
        write_c(program, &mut buf);
        assert_eq!(buf, b"int foo = 42;")
    }
}
