use crate::{
    ast::{Ast, AstKind, VariableDeclaration},
    div_round_up,
    parser::TokenKind,
    symbol_table::{Symbol, SymbolTable},
    CompilerResult,
};
use std::{cmp::Eq, error::Error, fmt};

pub enum TypeError<'src> {
    TypeMismatch {
        first: DataType<'src>,
        second: DataType<'src>,
    },
    ExpectedType {
        expected: DataType<'src>,
        found: DataType<'src>,
    },
    NotANumber,
    NotSigned,
    NotAssignable,
    NotAFunction,
    NotAReference,
    NotAStruct,
    WrongNumberOfArguments,
    NotDefined {
        name: &'src str,
    },
    FieldNotDefined {
        name: &'src str,
    },
    CannotInfer,
}

impl Error for TypeError<'_> {}

impl fmt::Debug for TypeError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::TypeMismatch { first, second } => {
                write!(f, "mismatched types: `{first:?}` and `{second:?}`")
            }
            Self::ExpectedType { expected, found } => {
                write!(f, "expected `{expected:?}` but found `{found:?}`")
            }
            Self::NotANumber => write!(f, "this is not a number, so you can't do that with it :/"),
            Self::NotSigned => write!(f, "this is not a signed number"),
            Self::NotAssignable => write!(f, "you can't assign to this expression"),
            Self::NotAFunction => write!(f, "this expression isn't a function"),
            Self::NotAReference => write!(f, "this expression isn't a reference"),
            Self::NotAStruct => write!(f, "this expression isn't a struct"),
            Self::WrongNumberOfArguments => {
                write!(f, "wrong number of arguments passed into function")
            }
            Self::NotDefined { name } => write!(f, "variable `{name}` was not defined"),
            Self::FieldNotDefined { name } => write!(f, "structure has no defined field `{name}`"),
            Self::CannotInfer => write!(f, "cannot infer type of expression"),
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

    pub fn is_signed(&self) -> bool {
        matches!(self, Self::S8 | Self::S16 | Self::S32 | Self::S64)
    }
}

impl fmt::Debug for IntType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::S8 => write!(f, "S8"),
            Self::S16 => write!(f, "S16"),
            Self::S32 => write!(f, "S32"),
            Self::S64 => write!(f, "S64"),

            Self::U8 => write!(f, "U8"),
            Self::U16 => write!(f, "U16"),
            Self::U32 => write!(f, "U32"),
            Self::U64 => write!(f, "U64"),
        }
    }
}

#[derive(PartialEq, Eq, Clone)]
pub enum DataType<'src> {
    Void,
    Bool,
    Inferred(InferredType),
    Int(IntType),
    Ref(Box<Self>),
    Struct(Vec<(&'src str, Self)>),
    Function {
        return_type: Box<Self>,
        argument_types: Vec<Self>,
    },
}

impl<'src> DataType<'src> {
    pub fn new(
        symbol_table: &mut SymbolTable<'src>,
        kind: &mut AstKind<'src>,
    ) -> CompilerResult<'src, Self> {
        let data_type = match kind {
            AstKind::Node { ref token } => match token.kind {
                TokenKind::Number(_) => Self::Inferred(InferredType::Int),
                TokenKind::Ident => {
                    let Some(Symbol::Variable(ref data_type)) = symbol_table.get_symbol(token.text) else {
                        return Err(TypeError::NotDefined { name: token.text }.into());
                    };

                    data_type.clone()
                }
                TokenKind::Str(_) => Self::Ref(Box::new(Self::Int(IntType::U8))),
                TokenKind::True | TokenKind::False => Self::Bool,
                _ => unreachable!(),
            },
            AstKind::Prefix {
                ref oper,
                ref mut node,
            } => {
                let node_data_type = node.data_type.clone();

                match oper.kind {
                    TokenKind::Sub => match node_data_type {
                        Self::Int(int_type) => {
                            if !int_type.is_signed() {
                                return Err(TypeError::NotSigned.into());
                            }

                            node_data_type
                        }
                        Self::Inferred(InferredType::Int) => node_data_type,
                        _ => return Err(TypeError::NotANumber.into()),
                    },
                    TokenKind::Not => {
                        if node_data_type != Self::Bool {
                            return Err(TypeError::ExpectedType {
                                expected: Self::Bool,
                                found: node_data_type,
                            }
                            .into());
                        }

                        node_data_type
                    }
                    TokenKind::Hash => {
                        if !node.kind.assignable() {
                            return Err(TypeError::NotAssignable.into());
                        }

                        Self::Ref(Box::new(node_data_type))
                    }
                    TokenKind::AtSymbol => {
                        let Self::Ref(ref deref) = node_data_type else {
                            return Err(TypeError::NotAReference.into());
                        };

                        *deref.clone()
                    }
                    _ => unreachable!(),
                }
            }
            AstKind::Infix {
                ref oper,
                ref mut lhs,
                ref mut rhs,
            } => {
                rhs.data_type.infer(lhs)?;
                lhs.data_type.infer(rhs)?;

                if lhs.data_type != rhs.data_type {
                    return Err(TypeError::TypeMismatch {
                        first: lhs.data_type.clone(),
                        second: rhs.data_type.clone(),
                    }
                    .into());
                }

                match oper.kind {
                    TokenKind::Add
                    | TokenKind::Sub
                    | TokenKind::Mul
                    | TokenKind::Div
                    | TokenKind::Mod => {
                        if !lhs.data_type.is_integer() {
                            return Err(TypeError::NotANumber.into());
                        }

                        lhs.data_type.clone()
                    }
                    TokenKind::Equals | TokenKind::NotEquals => Self::Bool,
                    TokenKind::Greater
                    | TokenKind::Less
                    | TokenKind::GreaterOrEqual
                    | TokenKind::LessOrEqual => {
                        if !lhs.data_type.is_integer() {
                            return Err(TypeError::NotANumber.into());
                        };

                        Self::Bool
                    }
                    _ => unreachable!(),
                }
            }
            AstKind::Index {
                ref mut lhs,
                ref mut index,
            } => {
                Self::Int(IntType::U64).infer(index)?;

                let Self::Ref(ref deref) = lhs.data_type else {
                    return Err(TypeError::NotAReference.into());
                };

                *deref.clone()
            }
            AstKind::Assign {
                ref mut lhs,
                ref mut rhs,
            } => {
                rhs.data_type.infer(lhs)?;
                lhs.data_type.infer(rhs)?;

                if lhs.data_type != rhs.data_type {
                    return Err(TypeError::TypeMismatch {
                        first: lhs.data_type.clone(),
                        second: rhs.data_type.clone(),
                    }
                    .into());
                }

                if !lhs.kind.assignable() {
                    return Err(TypeError::NotAssignable.into());
                }

                Self::Void
            }
            AstKind::GetField {
                ref mut lhs,
                ref name,
            } => {
                let Self::Struct(ref fields) = lhs.data_type else {
                    return Err(TypeError::NotAStruct.into());
                };

                let (_, data_type) = fields
                    .iter()
                    .find(|(field_name, _)| field_name == name)
                    .ok_or(TypeError::FieldNotDefined { name })?;

                data_type.clone()
            }
            AstKind::Block {
                ref mut statements, ..
            } => {
                let length = statements.len();

                for (n, statement) in statements.iter_mut().enumerate() {
                    if n + 1 < length {
                        Self::Void.infer(statement)?;
                    }
                }

                statements
                    .last()
                    .map_or(Self::Void, |statement| statement.data_type.clone())
            }
            AstKind::VariableDeclaration(VariableDeclaration {
                ref data_type,
                ref mut value,
                ..
            }) => {
                if let Some(ref mut value) = value {
                    data_type.infer(value)?;
                }

                Self::Void
            }
            AstKind::FunctionDeclaration {
                ref return_type,
                ref mut body,
                ..
            } => {
                return_type.infer(body)?;

                Self::Void
            }
            AstKind::StructDeclaration { .. } => Self::Void,
            AstKind::IfStatement {
                ref mut condition,
                ref mut if_block,
                ref mut else_block,
            } => {
                Self::Bool.infer(condition)?;

                if let Some(ref mut else_block) = else_block {
                    else_block.data_type.infer(if_block)?;
                    if_block.data_type.infer(else_block)?;

                    if if_block.data_type != else_block.data_type {
                        return Err(TypeError::TypeMismatch {
                            first: if_block.data_type.clone(),
                            second: else_block.data_type.clone(),
                        }
                        .into());
                    }
                } else {
                    Self::Void.infer(if_block)?;
                }

                if_block.data_type.clone()
            }
            AstKind::Call {
                ref mut lhs,
                ref mut arguments,
            } => {
                let Self::Function { ref return_type, ref argument_types } = &lhs.data_type else {
                    return Err(TypeError::NotAFunction.into());
                };

                if arguments.len() != argument_types.len() {
                    return Err(TypeError::WrongNumberOfArguments.into());
                }

                for (argument, argument_type) in arguments.iter_mut().zip(argument_types.iter()) {
                    argument_type.infer(argument)?;
                }

                *return_type.clone()
            }
            AstKind::WhileLoop {
                ref mut condition,
                ref mut body,
            } => {
                Self::Bool.infer(condition)?;

                Self::Void.infer(body)?;

                body.data_type.clone()
            }
        };

        Ok(data_type)
    }

    pub fn infer(&self, ast: &mut Ast<'src>) -> CompilerResult<'src, ()> {
        let Self::Inferred(ast_inferred_type) = ast.data_type else {
            if let Self::Inferred(_) = self {
                return Ok(());
            }

            if self != &ast.data_type {
                return Err(
                    TypeError::ExpectedType {
                        expected: self.clone(),
                        found: ast.data_type.clone()
                    }
                    .into()
                );
            }

            return Ok(());
        };

        if ast_inferred_type == InferredType::Int && !self.is_integer() {
            return Err(TypeError::ExpectedType {
                expected: self.clone(),
                found: Self::Inferred(ast_inferred_type),
            }
            .into());
        }

        match ast.kind {
            AstKind::Prefix {
                ref mut node,
                ref oper,
            } => match oper.kind {
                TokenKind::Sub | TokenKind::Not => self.infer(node)?,
                TokenKind::Hash => {
                    let Self::Ref(ref deref) = self else {
                            return Err(TypeError::ExpectedType {
                                expected: self.clone(),
                                found: ast.data_type.clone()
                            }.into());
                        };

                    deref.infer(node)?;
                }
                TokenKind::AtSymbol => Self::Ref(Box::new(self.clone())).infer(node)?,
                _ => unreachable!(),
            },
            AstKind::Infix {
                ref mut lhs,
                ref mut rhs,
                ref oper,
            } => match oper.kind {
                TokenKind::Add
                | TokenKind::Sub
                | TokenKind::Mul
                | TokenKind::Div
                | TokenKind::Mod => {
                    self.infer(lhs)?;
                    self.infer(rhs)?;
                }
                TokenKind::Equals
                | TokenKind::NotEquals
                | TokenKind::Less
                | TokenKind::Greater
                | TokenKind::LessOrEqual
                | TokenKind::GreaterOrEqual => {
                    if *self != Self::Bool {
                        return Err(TypeError::ExpectedType {
                            expected: Self::Bool,
                            found: self.clone(),
                        }
                        .into());
                    }
                }
                _ => unreachable!(),
            },
            AstKind::Block {
                ref mut statements, ..
            } => {
                let Some(statement) = statements.last_mut() else {
                    unreachable!("If we got here, it means the type of this AST is inferred.\n\
                                  But, if the list of block statements is empty, the type must not be inferred, but instead set to void.\n\
                                  Therefore, this situation is impossible and this statement unreachable.")
                };

                self.infer(statement)?;
            }
            AstKind::IfStatement {
                ref mut if_block,
                else_block: Some(ref mut else_block),
                ..
            } => {
                self.infer(if_block)?;
                self.infer(else_block)?;
            }
            _ => {}
        }

        ast.data_type = self.clone();

        Ok(())
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, Self::Int(_) | Self::Inferred(InferredType::Int))
    }

    pub fn is_signed_integer(&self) -> bool {
        match self {
            Self::Int(int_type) if int_type.is_signed() => true,
            Self::Inferred(InferredType::Int) => true,
            _ => false,
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Self::Void => 0,
            Self::Bool => 1,
            Self::Int(int_type) => int_type.size(),
            Self::Ref(_) | Self::Function { .. } => 8,
            Self::Struct(ref fields) => fields
                .iter()
                .map(|(_, data_type)| data_type.size_aligned())
                .sum(),
            _ => unreachable!(),
        }
    }

    #[inline(always)]
    pub fn size_aligned(&self) -> usize {
        div_round_up(self.size(), 8) * 8
    }
}

impl<'src> fmt::Debug for DataType<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Void => write!(f, "Void"),
            Self::Bool => write!(f, "Bool"),
            Self::Inferred(inferred) => write!(f, "{inferred:?}"),
            Self::Int(int) => write!(f, "{int:?}"),
            Self::Ref(deref) => write!(f, "#{deref:?}"),
            Self::Struct(fields) => {
                write!(
                    f,
                    "Struct {{ {}}}",
                    fields
                        .iter()
                        .map(|(name, data_type)| format!("let {name}: {data_type:?}; "))
                        .collect::<String>()
                )
            }
            Self::Function {
                return_type,
                argument_types,
            } => {
                write!(
                    f,
                    "Fn ({}) -> {return_type:?}",
                    argument_types
                        .iter()
                        .map(|x| format!("{x:?}"))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        }
    }
}
