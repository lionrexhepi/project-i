use core::str;
use std::io::Write;

use project_i::{ast::parse, ir::mangle, lexer::lex};

fn main() {
    let source = r#"let main = fn {
        while true {
            print 42
        }
        }"#
    .chars()
    .collect();
    let mut tokens = lex(source);
    let ast = parse(&mut tokens);
    let ir = mangle(ast);
    let mut buf = Vec::new();
    project_i::codegen::write_c(ir, &mut buf);
    println!("{}", str::from_utf8(&buf).unwrap());
    let mut gcc = std::process::Command::new("gcc")
        .arg("-x")
        .arg("c")
        .arg("-o")
        .arg("out2")
        .arg("-")
        .stdin(std::process::Stdio::piped())
        .spawn()
        .unwrap();
    gcc.stdin.as_mut().unwrap().write_all(&buf).unwrap();
    gcc.wait().unwrap();
}
