use crate::{
    ast::{Ast, AstKind},
    bytecode::{Argument, ByteCode, Function, OpCode, RegisterID},
    parser::TokenKind,
    symbol_table::{SymbolTable, VariableID},
    types::{DataType, IntType},
};
use std::collections::HashMap;

pub struct Compiler<'src> {
    symbol_table: SymbolTable<'src>,
    variable_registers: HashMap<VariableID<'src>, RegisterID>,
    function: Function<'src>
}

impl<'src> Compiler<'src> {
    pub fn new(symbol_table: SymbolTable<'src>) -> Self {
        Self {
            symbol_table,
            variable_registers: HashMap::new(),
            function: Function::new("main", DataType::Int(IntType::U8))
        }
    }

    pub fn compile(ast: Ast<'src>, symbol_table: SymbolTable<'src>) -> ByteCode<'src> {
        let mut compiler = Compiler::new(symbol_table);

        compiler.compile_ast(&ast, Some(Argument::Register(compiler.function.return_value)));
        
        let mut bytecode = ByteCode::new();

        bytecode.add_function(compiler.function);

        bytecode
    }

    fn compile_ast(&mut self, ast: &Ast<'src>, dst: Option<Argument>) -> Argument {
        match &ast.kind {
            AstKind::Node { ref token } => {
                let node = match token.kind {
                    TokenKind::Number(value) => Argument::Constant { value },
                    TokenKind::Ident => {
                        Argument::Register(
                            self.variable_registers[&self.symbol_table.get_variable_id(token.text).unwrap()]
                        )
                    },
                    TokenKind::True => Argument::Constant { value: 1 },
                    TokenKind::False => Argument::Constant { value: 0 },
                    _ => unreachable!(),
                };

                dst.map_or(node, |dst| {
                    self.function.add_opcode(OpCode::Mov { dst, src: node });
                    dst
                })
            },
            AstKind::Infix {
                ref oper,
                ref lhs,
                ref rhs,
            } => {
                let dst = dst.unwrap_or_else(|| {
                    Argument::Register(self.function.add_register(lhs.data_type.clone()))
                });

                let _lhs = self.compile_ast(lhs, Some(dst));

                let rhs = self.compile_ast(rhs, None);

                match oper.kind {
                    TokenKind::Add => self.function.add_opcode(OpCode::Add { dst, src: rhs }),
                    TokenKind::Sub => self.function.add_opcode(OpCode::Sub { dst, src: rhs }),
                    TokenKind::Mul => self.function.add_opcode(OpCode::Mul { dst, src: rhs }),
                    TokenKind::Div => self.function.add_opcode(OpCode::Div { dst, src: rhs }),
                    TokenKind::Mod => self.function.add_opcode(OpCode::Mod { dst, src: rhs }),
                    _ => unreachable!(),
                }

                dst
            }
            AstKind::Assign {
                ref lhs,
                ref rhs,
            } => {
                let lhs = self.compile_ast(lhs, None);
                let _rhs = self.compile_ast(rhs, Some(lhs));

                Argument::NullRegister
            }
            AstKind::Block { scope_id, ref statements } => {
                let dst = dst.unwrap_or_else(|| {
                    if let DataType::Void = ast.data_type {
                        Argument::NullRegister
                    } else {
                        Argument::Register(self.function.add_register(ast.data_type.clone()))
                    }
                });

                self.symbol_table.enter_scope(*scope_id);

                for (n, statement) in statements.iter().enumerate() {
                    let _statement = if n + 1 == statements.len() && ast.data_type != DataType::Void {
                        self.compile_ast(statement, Some(dst))
                    } else {
                        self.compile_ast(statement, None)
                    };
                }

                self.symbol_table.leave_scope();

                dst
            }
            AstKind::Declaration { name, ref data_type, ref value } => {
                let variable = self.function.add_register(data_type.clone());

                self.variable_registers.insert(self.symbol_table.get_variable_id(name).unwrap(), variable);

                if let Some(value) = value {
                    self.compile_ast(value, Some(Argument::Register(variable)));
                }

                Argument::NullRegister
            }
            AstKind::IfStatement { condition, if_block, else_block } => {
                let dst = dst.unwrap_or_else(|| {
                    if let DataType::Void = ast.data_type {
                        Argument::NullRegister
                    } else {
                        Argument::Register(self.function.add_register(ast.data_type.clone()))
                    }
                });

                let condition = self.compile_ast(condition, None);

                let else_label = self.function.add_label();
                let end_label = self.function.add_label();

                self.function.add_opcode(OpCode::GotoIfZero { condition, label_id: else_label });

                let _if_block = self.compile_ast(if_block, Some(dst));

                self.function.add_opcode(OpCode::Goto { label_id: end_label });
                self.function.add_opcode(OpCode::Label { label_id: else_label });

                if let Some(ref else_block) = else_block {
                    let _else_block = self.compile_ast(else_block, Some(dst));
                }

                self.function.add_opcode(OpCode::Label { label_id: end_label });

                dst
            }
            _ => todo!(),
        }
    }
}
