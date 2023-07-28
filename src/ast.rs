use std::cmp::Eq;
use crate::{
    parser::Token,
    types::DataType
};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum AstKind<'a> {
    Atom {
        token: Token<'a>,
    },
    Prefix {
        oper: Token<'a>,
        node: Box<Ast<'a>>,
    },
    Infix {
        oper: Token<'a>,
        lhs: Box<Ast<'a>>,
        rhs: Box<Ast<'a>>,
    },
    Block {
        statements: Vec<Ast<'a>>,
    },
    FunctionCall {
        name: Token<'a>,
        arguments: Vec<Ast<'a>>,
    },
    FunctionDefinition {
        name: Token<'a>,
        arguments: Vec<Ast<'a>>,
        return_type: Box<Ast<'a>>,
        body: Box<Ast<'a>>,
    },
    Declaration {
        name: Token<'a>,
        data_type: Box<Ast<'a>>,
        value: Option<Box<Ast<'a>>>,
    },
    Return {
        value: Box<Ast<'a>>,
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Ast<'a> {
    pub data_type: DataType,
    pub kind: AstKind<'a>,
}
