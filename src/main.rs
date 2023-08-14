use oil::{
    compiler::Compiler,
    parser::Parser,
    CompilerResult,
};
use std::{
    env,
    fs::{self, File},
    io::Write,
};

fn main() -> CompilerResult<'static, ()> {
    let mut args = env::args();

    assert!(args.next().is_some());

    let Some(input_file_path) = args.next() else {
        return Err("Not enough arguments provided.".into());
    };

    let input_file = fs::read_to_string(input_file_path)?;

    let (ast, symbol_table) = Parser::parse(&input_file)
        .map_err(|e| e.to_string())?;

    let code = Compiler::compile(ast, symbol_table);

    if let Some(output_file_path) = args.next() {
        let mut output_file = File::create(output_file_path)?;

        output_file.write_all(code.as_bytes())?;
    } else {
        println!("{code}");
    }

    Ok(())
}
