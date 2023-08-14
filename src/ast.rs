use crate::{
    parser::{Token, TokenKind},
    symbol_table::SymbolTable,
    types::DataType,
    CompilerResult,
};
use std::cmp::Eq;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum AstKind<'src> {
    Node {
        token: Token<'src>,
    },
    Prefix {
        oper: Token<'src>,
        node: Box<Ast<'src>>,
    },
    Infix {
        oper: Token<'src>,
        lhs: Box<Ast<'src>>,
        rhs: Box<Ast<'src>>,
    },
    Assign {
        lhs: Box<Ast<'src>>,
        rhs: Box<Ast<'src>>,
    },
    Block {
        scope_id: usize,
        statements: Vec<Ast<'src>>,
    },
    Declaration {
        name: &'src str,
        data_type: DataType,
        value: Option<Box<Ast<'src>>>,
    },
    FunctionDeclaration {
        name: &'src str,
        return_type: DataType,
        arguments: Vec<Ast<'src>>,
        body: Box<Ast<'src>>,
    },
    Call {
        lhs: Box<Ast<'src>>,
        arguments: Vec<Ast<'src>>,
    },
    IfStatement {
        condition: Box<Ast<'src>>,
        if_block: Box<Ast<'src>>,
        else_block: Option<Box<Ast<'src>>>,
    },
    WhileLoop {
        condition: Box<Ast<'src>>,
        body: Box<Ast<'src>>,
    },
}

impl<'src> AstKind<'src> {
    pub fn assignable(&self) -> bool {
        matches!(
            self,
            Self::Node {
                token: Token {
                    kind: TokenKind::Ident,
                    ..
                }
            }
        )
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Ast<'src> {
    pub data_type: DataType,
    pub kind: AstKind<'src>,
}

impl<'src> Ast<'src> {
    pub fn new(
        symbol_table: &mut SymbolTable<'src>,
        mut kind: AstKind<'src>,
    ) -> CompilerResult<'src, Self> {
        let data_type = DataType::new(symbol_table, &mut kind)?;

        Ok(Self { kind, data_type })
    }
}
