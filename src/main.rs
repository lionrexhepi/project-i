use core::str;
use std::io::Write;

use project_i::{
    ast::parse,
    lexer::{lex, InMemoryFile},
};

fn main() {
    let source = r#"
        let facrec= fn fac(n: i32): i32 {
            if n == 2{
                1
            } else {
                n * fac(n - 1)
            }
        };"#
    .chars()
    .collect::<InMemoryFile>();
    let mut tokens = lex(source).unwrap();
    let ast = parse(&mut tokens).unwrap();
    let ir = todo!();
    let mut buf = Vec::from(b"#include <stdio.h>\nint add2(int a){return a+2;}\n");
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
