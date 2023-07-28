pub mod ast;
pub mod bytecode;
pub mod compiler;
pub mod parser;
pub mod types;

pub type CompilerResult<'a, T> = Result<T, Box<dyn std::error::Error + 'a>>;
