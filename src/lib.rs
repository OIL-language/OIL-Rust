use std::error::Error;

pub mod ast;
pub mod bytecode;
pub mod compiler;
pub mod nasm;
pub mod parser;
pub mod symbol_table;
pub mod types;

pub type CompilerResult<'src, T> = Result<T, Box<dyn Error + 'src>>;

#[inline(always)]
pub const fn div_round_up(a: usize, b: usize) -> usize {
    (a + b - 1) / b
}
