use core::str;
use std::{
    io::{Read, Result, Write},
    path::PathBuf,
};

use clap::Parser;
use project_i::{
    ast::parse,
    codegen,
    ir::transformer::FileTransformer,
    lexer::{lex, File, InMemoryFile},
};
use tempdir::TempDir;

#[derive(Parser, Debug)]
struct CompilerArgs {
    #[arg(short, long, num_args = 1.., value_delimiter=' ', required=true )]
    input: Vec<PathBuf>,
    #[arg(short, long, default_value = "i.out")]
    output: PathBuf,
}

static STD_HEADERS: &[u8] = include_bytes!("../HEADERS.h");

fn main() -> Result<()> {
    let args = CompilerArgs::parse();

    let c_dir = TempDir::new("I_OUT")?;

    let sources = args
        .input
        .into_iter()
        .map(|path| -> Result<_> {
            let mut string = String::new();
            std::fs::File::open(path)?.read_to_string(&mut string)?;

            let source = string.chars().collect::<InMemoryFile>();
            let name = source.name().to_owned();
            let mut tokens = lex(source).unwrap();
            let ast = parse(&mut tokens).unwrap();
            let ir = project_i::ir::Ir {
                items: FileTransformer::new().transform_file(ast).unwrap(),
            };

            let mut c_path = c_dir.path().to_owned();
            c_path.push(name);
            c_path = c_path.with_extension("c");
            println!("{}", c_path.display());
            let mut c_file = std::fs::File::options()
                .create(true)
                .write(true)
                .open(&c_path)?;
            c_file.write_all(&STD_HEADERS)?;
            codegen::write_c(ir, &mut c_file)?;

            Ok(c_path)
        })
        .collect::<Result<Vec<_>>>()?;

    let mut gcc = std::process::Command::new("gcc")
        .arg("-x")
        .arg("c")
        .arg("-o")
        .arg(args.output)
        .args(sources)
        .spawn()?;
    gcc.wait()?;

    Ok(())
}
