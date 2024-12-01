use project_i::{
    ast::{parse, Ast},
    ir::mangle,
    lexer::lex,
};

fn main() {
    let source = "print 42".chars().collect();
    let mut tokens = lex(source);
    let ast = parse(&mut tokens);
    let ir = mangle(ast);
    let mut buf = Vec::new();
    project_i::codegen::write_c(ir, &mut buf);
    println!("{}", String::from_utf8(buf).unwrap());
}
