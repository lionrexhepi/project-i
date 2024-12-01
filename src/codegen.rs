use std::io::Write;

use crate::ir::{MangledExpression, MangledItem, MangledProgram};

pub fn write_c(program: MangledProgram, to: &mut impl Write) {
    for item in program.items {
        match item {
            MangledItem::Print(MangledExpression::Literal(expr)) => {
                write!(to, "printf(\"%ld\\n\", {});", expr).unwrap();
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_write_c() {
        let program = MangledProgram::from([MangledItem::Print(MangledExpression::Literal(42))]);
        let mut buf = Vec::new();
        write_c(program, &mut buf);
        assert_eq!(buf, b"printf(\"%ld\\n\", 42);");
    }
}
