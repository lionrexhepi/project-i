use std::io::Write;

use crate::ir::{MangledExpression, MangledItem, MangledProgram};

pub fn write_c(program: MangledProgram, to: &mut impl Write) {
    for item in program.items {
        match item {
            MangledItem::Print(expr) => {
                let str = match expr {
                    MangledExpression::LitInt(i) => i.to_string(),
                    MangledExpression::LitBool(b) => b.to_string(),
                };
                write!(to, "printf(\"%ld\\n\", {});", str).unwrap();
            }
        }
    }
}

#[cfg(test)]
mod test {
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
        assert_eq!(buf, b"printf(\"%ld\\n\", true);");
    }
}
