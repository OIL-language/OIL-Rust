use crate::{
    ast::{Ast, AstKind},
    parser::TokenKind,
    symbol_table::SymbolTable,
    CompilerResult,
};
use std::{cmp::Eq, error::Error, fmt};

pub enum TypeError<'src> {
    TypeMismatch(DataType, DataType),
    ExpectedType(DataType, DataType),
    NotANumber,
    NotSigned,
    NotAssignable,
    NotDefined(&'src str)
}

impl Error for TypeError<'_> {}

impl fmt::Debug for TypeError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::TypeMismatch(a, b) => write!(f, "mismatched types: `{a:?}` and `{b:?}`"),
            Self::ExpectedType(a, b) => write!(f, "expected `{a:?}` but found `{b:?}`"),
            Self::NotANumber => write!(f, "this is not a number, so you can't do that with it :/"),
            Self::NotSigned => write!(f, "this is not a signed number, so you can't do that with it :/"),
            Self::NotAssignable => write!(f, "you can't assign to this expression"),
            Self::NotDefined(var_name) => write!(f, "variable `{var_name}` was not defined")
        }
    }
}

impl fmt::Display for TypeError<'_> {
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
            Self::Int => write!(f, "{{integer}}"),
            Self::Any => write!(f, "{{any}}"),
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

    pub fn signed(&self) -> bool {
        matches!(self, Self::S8 | Self::S16 | Self::S32 | Self::S64)
    }
}

impl fmt::Debug for IntType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::S8  => write!(f, "S8"),
            Self::S16 => write!(f, "S16"),
            Self::S32 => write!(f, "S32"),
            Self::S64 => write!(f, "S64"),

            Self::U8  => write!(f, "U8"),
            Self::U16 => write!(f, "U16"),
            Self::U32 => write!(f, "U32"),
            Self::U64 => write!(f, "U64"),
        }
    }
}

#[derive(PartialEq, Eq, Clone)]
pub enum DataType {
    Void,
    Bool,
    Inferred(InferredType),
    Int(IntType),
    Ref(Box<Self>),
}

impl DataType {
    pub fn size(&self) -> usize {
        match self {
            Self::Void => 0,
            Self::Bool => 1,
            Self::Int(int_type) => int_type.size(),
            _ => unreachable!(),
        }
    }
}

impl fmt::Debug for DataType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Void => write!(f, "Void"),
            Self::Bool => write!(f, "Bool"),
            Self::Inferred(inferred) => write!(f, "{inferred:?}"),
            Self::Int(int) => write!(f, "{int:?}"),
            Self::Ref(deref) => write!(f, "#{deref:?}")
        }
    }
}

impl<'src> Ast<'src> {
    pub fn new(symbol_table: &SymbolTable<'src>, mut kind: AstKind<'src>) -> CompilerResult<'src, Self> {
        let mut assignable = false;

        let data_type = match kind {
            AstKind::Node { ref token } => match token.kind {
                TokenKind::Number(_) => DataType::Inferred(InferredType::Int),
                TokenKind::Ident => {
                    assignable = true;
                    symbol_table.get_variable(token.text)
                                .ok_or(TypeError::NotDefined(token.text))?
                                .data_type
                                .clone()
                },
                TokenKind::Str(_) => DataType::Ref(Box::new(DataType::Int(IntType::U8))),
                TokenKind::True | TokenKind::False => DataType::Bool,
                _ => unreachable!(),
            },
            AstKind::Prefix {
                ref oper,
                ref mut node,
            } => {
                let node_data_type = node.data_type.clone();

                if oper.kind == TokenKind::Sub {
                    match node_data_type {
                        DataType::Int(int_type) => if !int_type.signed() {
                            return Err(TypeError::NotSigned.into());
                        },
                        DataType::Inferred(InferredType::Int) => {},
                        _ => return Err(TypeError::NotANumber.into())
                    }
                }

                node_data_type
            }
            AstKind::Infix {
                ref oper,
                ref mut lhs,
                ref mut rhs,
            } => {
                lhs.infer(&rhs.data_type)?;
                rhs.infer(&lhs.data_type)?;

                if lhs.data_type != rhs.data_type {
                    return Err(TypeError::TypeMismatch(lhs.data_type.clone(), rhs.data_type.clone()).into());
                }

                match oper.kind {
                    | TokenKind::Add | TokenKind::Sub
                    | TokenKind::Mul | TokenKind::Div
                    | TokenKind::Mod => {
                        let (DataType::Int(_) | DataType::Inferred(InferredType::Int)) = lhs.data_type else {
                            return Err(TypeError::NotANumber.into());
                        };

                        lhs.data_type.clone()
                    },
                    | TokenKind::Equals | TokenKind::Greater
                    | TokenKind::Less => {
                        let (DataType::Int(_) | DataType::Inferred(InferredType::Int)) = lhs.data_type else {
                            return Err(TypeError::NotANumber.into());
                        };

                        DataType::Bool
                    },
                    _ => unreachable!()
                }
            }
            AstKind::Assign { ref mut lhs, ref mut rhs } => {
                lhs.infer(&rhs.data_type)?;
                rhs.infer(&lhs.data_type)?;

                if lhs.data_type != rhs.data_type {
                    Err(TypeError::TypeMismatch(
                        lhs.data_type.clone(),
                        rhs.data_type.clone(),
                    ))?
                }

                if !lhs.assignable {
                    return Err(TypeError::NotAssignable.into());
                }

                DataType::Void
            }
            AstKind::Block { ref mut statements, .. } => {
                let length = statements.len();

                for (n, statement) in statements.iter_mut().enumerate() {
                    if n + 1 < length {
                        statement.infer(&DataType::Void)?;
                    }
                }

                statements.last()
                    .map_or(DataType::Void, |statement| statement.data_type.clone())
            }
            AstKind::Declaration { ref data_type, ref mut value, .. } => {
                value.as_mut().map_or(Ok(()), |value| value.infer(data_type))?;

                DataType::Void
            }
            AstKind::IfStatement { ref mut condition, ref mut if_block, ref mut else_block } => {
                condition.infer(&DataType::Bool)?;

                if let Some(ref mut else_block) = else_block {
                    if_block.infer(&else_block.data_type)?;
                    else_block.infer(&if_block.data_type)?;

                    if if_block.data_type != else_block.data_type {
                        return Err(TypeError::TypeMismatch(
                            if_block.data_type.clone(),
                            else_block.data_type.clone()
                        ).into());
                    }
                } else {
                    if_block.infer(&DataType::Void)?;
                }

                if_block.data_type.clone()
            }
            AstKind::WhileLoop { ref mut condition, ref mut body } => {
                condition.infer(&DataType::Bool)?;

                body.infer(&DataType::Void)?;

                body.data_type.clone()
            }
        };

        Ok(Self { assignable, kind, data_type })
    }

    pub fn infer(&mut self, data_type: &DataType) -> CompilerResult<'src, ()> {
        let DataType::Inferred(self_inferred_type) = self.data_type else {
            if let DataType::Inferred(_) = data_type {
                return Ok(());
            }

            if data_type != &self.data_type {
                return Err(TypeError::ExpectedType(data_type.clone(), self.data_type.clone()).into());
            }

            return Ok(());
        };

        match &self_inferred_type {
            InferredType::Int => {
                let (DataType::Int(_) | DataType::Inferred(InferredType::Int)) = data_type else {
                    return Err(TypeError::ExpectedType(data_type.clone(), DataType::Inferred(self_inferred_type)).into());
                };
            }
            _ => {}
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
            AstKind::Block { ref mut statements, .. } => {
                let Some(statement) = statements.last_mut() else {
                    unreachable!("If we got here, it means the type of this AST is inferred.\n\
                                  But, if the list of block statements is empty, the type must not be inferred, but instead set to void.\n\
                                  Therefore, this situation is impossible and this statement unreachable.")
                };

                statement.infer(data_type)?;
            }
            AstKind::IfStatement { ref mut if_block, ref mut else_block, .. } => {
                if let Some(ref mut else_block) = else_block {
                    if_block.infer(data_type)?;
                    else_block.infer(data_type)?;
                }
            },
            _ => {}
        }

        self.data_type = data_type.clone();

        Ok(())
    }
}
