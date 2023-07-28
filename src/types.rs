use std::{
    cmp::Eq,
    error::Error,
    fmt
};
use crate::{
    ast::{AstKind, Ast},
    CompilerResult,
    parser::{Token, TokenKind}
};

pub enum TypeError {
    TypeMismatch(DataType, DataType),
    NotANumber(DataType),
}

impl Error for TypeError {}

impl fmt::Debug for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::TypeMismatch(a, b) => write!(f, "Mismatched types: {a:?} and {b:?}"),
            Self::NotANumber(n) => write!(f, "Sorry, but {n:?} is not a number, so you can't do that with it :/"),
        }
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(PartialEq, Eq, Copy, Clone)]
pub enum InferredType {
    Int,
    Any,
}

impl fmt::Debug for InferredType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int => write!(f, "{{inferred integer}}"),
            Self::Any => write!(f, "{{inferred any}}"),
        }
    }
}

#[derive(PartialEq, Eq, Copy, Clone)]
pub enum IntType {
    S8,
    S16,
    S32,
    S64,

    U8,
    U16,
    U32,
    U64,
}

impl IntType {
    pub fn size(&self) -> usize {
        match self {
            Self::S8 | Self::U8 => 1,
            Self::S16 | Self::U16 => 2,
            Self::S32 | Self::U32 => 4,
            Self::S64 | Self::U64 => 8,
        }
    }
}

impl fmt::Debug for IntType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::S8 => write!(f, "s8"),
            Self::S16 => write!(f, "s16"),
            Self::S32 => write!(f, "s32"),
            Self::S64 => write!(f, "s64"),

            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
        }
    }
}

#[derive(PartialEq, Eq, Clone)]
pub enum DataType {
    Void,
    Inferred(InferredType),
    Int(IntType),
    Ref(Box<Self>),
}

impl DataType {
    pub fn size(&self) -> usize {
        match self {
            Self::Void => 0,
            Self::Int(int_type) => int_type.size(),
            _ => unreachable!(),
        }
    }
}

impl fmt::Debug for DataType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Void => write!(f, "void"),
            Self::Inferred(inferred) => write!(f, "{inferred:?}"),
            Self::Int(int) => write!(f, "{int:?}"),
            Self::Ref(ref deref) => write!(f, "#{deref:?}"),
        }
    }
}

impl<'a> Ast<'a> {
    pub fn new(mut kind: AstKind<'a>) -> CompilerResult<'a, Self> {
        let data_type = match kind {
            AstKind::Atom { ref token } => match token.kind {
                TokenKind::Number(_) => DataType::Inferred(InferredType::Int),
                TokenKind::Ident => DataType::Inferred(InferredType::Any),
                TokenKind::Str(_) => DataType::Ref(Box::new(DataType::Int(IntType::U8))),
                _ => unreachable!(),
            },
            AstKind::Prefix {
                oper: Token {
                    kind: TokenKind::Sub,
                    ..
                },
                ref mut node,
            } => {
                let node_data_type = node.data_type.clone();

                let (DataType::Int(_) | DataType::Inferred(InferredType::Int)) = node_data_type else {
                    Err(TypeError::NotANumber(node_data_type))?
                };

                node_data_type
            },
            AstKind::Infix {
                oper: Token {
                    kind: TokenKind::Add
                        | TokenKind::Sub
                        | TokenKind::Mul
                        | TokenKind::Div
                        | TokenKind::Mod,
                    ..
                },
                ref mut lhs,
                ref mut rhs,
            } => {
                lhs.infer(&rhs.data_type)?;
                rhs.infer(&lhs.data_type)?;

                if lhs.data_type != rhs.data_type {
                    Err(TypeError::TypeMismatch(
                        lhs.data_type.clone(),
                        rhs.data_type.clone(),
                    ))?
                }

                let lhs_data_type = lhs.data_type.clone();

                let (DataType::Int(_) | DataType::Inferred(InferredType::Int)) = lhs_data_type else {
                    Err(TypeError::NotANumber(lhs_data_type))?
                };

                lhs_data_type
            },
            _ => unreachable!(),
        };

        Ok(Self { kind, data_type })
    }

    pub fn infer<'b>(&mut self, data_type: &'b DataType) -> CompilerResult<'a, ()> {
        let DataType::Inferred(inferred_type) = self.data_type else {
            if data_type != &self.data_type {
                Err(TypeError::TypeMismatch(data_type.clone(), self.data_type.clone()))?
            }

            return Ok(());
        };

        match &inferred_type {
            InferredType::Int => {
                let (DataType::Int(_) | DataType::Inferred(InferredType::Int)) = data_type else {
                    Err(TypeError::TypeMismatch(DataType::Inferred(inferred_type), self.data_type.clone()))?
                };
            }
            InferredType::Any => {}
        }

        match &mut self.kind {
            AstKind::Prefix { ref mut node, .. } => node.infer(data_type)?,
            AstKind::Infix {
                ref mut lhs,
                ref mut rhs,
                ..
            } => {
                lhs.infer(data_type)?;
                rhs.infer(data_type)?;
            }
            _ => {}
        }

        self.data_type = data_type.clone();

        Ok(())
    }
}
