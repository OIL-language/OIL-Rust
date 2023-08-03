use crate::{parser::Token, types::DataType};
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
        rhs: Box<Ast<'src>>
    },
    Block {
        scope_id: usize,
        statements: Vec<Ast<'src>>,
    },
    FunctionCall {
        lhs: Box<Ast<'src>>,
        arguments: Vec<Ast<'src>>,
    },
    FunctionDefinition {
        name: &'src str,
        arguments: Vec<Ast<'src>>,
        return_type: Box<Ast<'src>>,
        body: Box<Ast<'src>>,
    },
    Declaration {
        name: &'src str,
        data_type: DataType,
        value: Option<Box<Ast<'src>>>,
    },
    IfStatement {
        condition: Box<Ast<'src>>,
        if_block: Box<Ast<'src>>,
        else_block: Option<Box<Ast<'src>>>,
    },
    Return {
        value: Box<Ast<'src>>,
    },
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Ast<'src> {
    pub assignable: bool,
    pub data_type: DataType,
    pub kind: AstKind<'src>,
}
