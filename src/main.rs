use oil::{
    bytecode::CodeGenerator, compiler::Compiler, nasm::Nasm, parser::Parser,
    symbol_table::SymbolTable, CompilerResult,
};
use std::{
    env,
    fs::{self, File},
    io::Write,
};

fn main() -> CompilerResult<'static, ()> {
    let mut args = env::args();

    assert!(args.next().is_some()); // executable

    let Some(input_file_path) = args.next() else {
        return Err("Not enough arguments provided.".into());
    };

    let input_file = fs::read_to_string(input_file_path)?;

    let mut symbol_table = SymbolTable::new();

    let ast = Parser::parse(&input_file, &mut symbol_table).map_err(|e| e.to_string())?;

    let bytecode = Compiler::compile(&ast, symbol_table);

    eprintln!("{bytecode:#?}");

    let code = Nasm::generate(&bytecode)?;

    if let Some(output_file_path) = args.next() {
        let mut output_file = File::create(output_file_path)?;

        output_file.write_all(code.as_bytes())?;
    } else {
        println!("{code}");
    }

    Ok(())
}
