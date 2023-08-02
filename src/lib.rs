pub mod ast;
pub mod bytecode;
pub mod compiler;
pub mod parser;
pub mod symbol_table;
pub mod types;

pub type CompilerResult<'src, T> = Result<T, Box<dyn std::error::Error + 'src>>;
