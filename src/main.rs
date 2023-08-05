use oil::{
    bytecode::ByteCode,
    compiler::Compiler,
    parser::Parser,
    symbol_table::SymbolTable,
    CompilerResult,
};
use std::{
    env,
    fs::{self, File},
    io::Write,
};

fn compile(file: &str) -> CompilerResult<ByteCode<'_>> {
    let mut symbol_table = SymbolTable::new();

    let ast = Parser::new(file).parse(&mut symbol_table)?;
    
    Ok(Compiler::compile(ast, symbol_table))
}

fn main() -> CompilerResult<'static, ()> {
    let mut args = env::args();

    assert!(args.next().is_some());

    let Some(input_file_path) = args.next() else {
        return Err("Not enough arguments provided.".into());
    };

    let input_file = fs::read_to_string(input_file_path)?;

    match compile(&input_file) {
        Ok(bytecode) => {
            let code = bytecode.compile_nasm();

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
