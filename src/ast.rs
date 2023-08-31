use crate::{
    parser::{Token, TokenKind},
    symbol_table::SymbolTable,
    types::DataType,
    CompilerResult,
};
use std::cmp::Eq;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct VariableDeclaration<'src> {
    pub name: &'src str,
    pub data_type: DataType<'src>,
    pub value: Option<Box<Ast<'src>>>,
}

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
    Index {
        lhs: Box<Ast<'src>>,
        index: Box<Ast<'src>>,
    },
    Assign {
        lhs: Box<Ast<'src>>,
        rhs: Box<Ast<'src>>,
    },
    GetField {
        lhs: Box<Ast<'src>>,
        name: &'src str,
    },
    Block {
        scope_id: usize,
        statements: Vec<Ast<'src>>,
    },
    VariableDeclaration(VariableDeclaration<'src>),
    FunctionDeclaration {
        name: &'src str,
        scope_id: usize,
        return_type: DataType<'src>,
        arguments: Vec<VariableDeclaration<'src>>,
        body: Box<Ast<'src>>,
    },
    StructDeclaration {
        name: &'src str,
        fields: Vec<VariableDeclaration<'src>>,
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
            } | Self::Prefix {
                oper: Token {
                    kind: TokenKind::AtSymbol,
                    ..
                },
                ..
            } | Self::Index { .. }
                | Self::GetField { .. }
        )
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Ast<'src> {
    pub data_type: DataType<'src>,
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
