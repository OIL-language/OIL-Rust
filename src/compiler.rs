use crate::{
    ast::{Ast, AstKind},
    bytecode::{Argument, ByteCode, Function, OpCode},
    parser::TokenKind,
    types::DataType
};

pub struct Compiler<'a> {
    pub bytecode: ByteCode<'a>,
}

impl<'a> Compiler<'a> {
    pub fn new() -> Self {
        Self {
            bytecode: ByteCode::new(),
        }
    }

    pub fn compile(&mut self, ast: Ast<'a>) {
        let mut function = Function::new("main", DataType::Void);

        self.compile_ast(&ast, &mut function);

        self.bytecode.add_function(function);
    }

    pub fn compile_ast<'b>(&mut self, ast: &'b Ast<'a>, function: &mut Function) -> Argument {
        match &ast.kind {
            AstKind::Atom { ref token } => match token.kind {
                TokenKind::Number(value) => Argument::Constant { value },
                _ => unreachable!(),
            },
            AstKind::Infix {
                ref oper,
                ref lhs,
                ref rhs,
            } => {
                let dst = function.add_register(lhs.data_type.clone());

                let lhs = self.compile_ast(lhs, function);
                let rhs = self.compile_ast(rhs, function);

                function.add_opcode(OpCode::Mov { dst, src: lhs });

                match oper.kind {
                    TokenKind::Add => function.add_opcode(OpCode::Add { dst, src: rhs }),
                    TokenKind::Sub => function.add_opcode(OpCode::Sub { dst, src: rhs }),
                    TokenKind::Mul => function.add_opcode(OpCode::Mul { dst, src: rhs }),
                    TokenKind::Div => function.add_opcode(OpCode::Div { dst, src: rhs }),
                    TokenKind::Mod => function.add_opcode(OpCode::Mod { dst, src: rhs }),
                    _ => unreachable!(),
                }

                dst
            }
            _ => todo!(),
        }
    }
}

impl Default for Compiler<'_> {
    fn default() -> Self {
        Self::new()
    }
}
