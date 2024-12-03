use std::io::Write;

use crate::ir::{MangledExpression, MangledItem, MangledProgram};

pub fn write_c(program: MangledProgram, to: &mut impl Write) {
    for item in program.items {
        match item {
            MangledItem::Print(expr) => {
                to.write_all(b"printf(\"%ld\\n\", ").unwrap();
                write_expression(expr, to);
                to.write_all(b");").unwrap();
            }
            MangledItem::Declaration {
                typename: ty,
                var,
                value,
            } => {
                write!(to, "{} {} = ", ty, var).unwrap();
                write_expression(value, to);
                to.write_all(b";").unwrap();
            }
        }
    }
}

fn write_expression(expr: MangledExpression, to: &mut impl Write) {
    match expr {
        MangledExpression::LitInt(i) => write!(to, "{}", i).unwrap(),
        MangledExpression::LitBool(b) => write!(to, "{}", b).unwrap(),
        MangledExpression::Variable(smol_str) => write!(to, "{}", smol_str).unwrap(),
    }
}

#[cfg(test)]
mod test {
    use core::str;

    use super::*;

    #[test]
    fn test_print_int() {
        let program = MangledProgram::from([MangledItem::Print(MangledExpression::LitInt(42))]);
        let mut buf = Vec::new();
        write_c(program, &mut buf);
        assert_eq!(buf, b"printf(\"%ld\\n\", 42);");
    }

    #[test]
    fn test_print_bool() {
        let program = MangledProgram::from([MangledItem::Print(MangledExpression::LitBool(true))]);
        let mut buf = Vec::new();
        write_c(program, &mut buf);
        assert_eq!(str::from_utf8(&buf).unwrap(), "printf(\"%ld\\n\", true);");
    }

    #[test]
    fn test_declare_int() {
        let program = MangledProgram::from([MangledItem::Declaration {
            typename: "int".into(),
            var: "foo".into(),
            value: MangledExpression::LitInt(42),
        }]);
        let mut buf = Vec::new();
        write_c(program, &mut buf);
        assert_eq!(buf, b"int foo = 42;")
    }
}
