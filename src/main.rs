use oil::{
    compiler,
    parser::Parser,
    symbol_table::SymbolTable,
    types::{DataType, IntType},
    CompilerResult,
};
use std::{
    env,
    fs::{self, File},
    io::Write,
};

fn compile<'src>(input_file: &'src str) -> CompilerResult<'src, String> {
    let mut symbol_table = SymbolTable::new();

    let mut ast = Parser::new(input_file).parse(&mut symbol_table)?;

    DataType::Int(IntType::U64).infer(&mut ast)?;

    Ok(compiler::compile(ast, symbol_table))
}

fn main() -> CompilerResult<'static, ()> {
    let mut args = env::args();

    assert!(args.next().is_some());

    let Some(input_file_path) = args.next() else {
        return Err("Not enough arguments provided.".into());
    };

    match compile(&fs::read_to_string(input_file_path)?) {
        Ok(code) => {
            if let Some(output_file_path) = args.next() {
                let mut output_file = File::create(output_file_path)?;

                output_file.write_all(code.as_bytes())?;
            } else {
                println!("{code}");
            }
        }
        Err(err) => {
            eprintln!("Error: {err:?}");
            std::process::exit(1);
        }
    };

    Ok(())
}
