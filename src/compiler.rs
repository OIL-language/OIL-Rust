use crate::{
    ast::{Ast, AstKind},
    bytecode::{Argument, ByteCode, Function, OpCode, RegisterID},
    parser::TokenKind,
    symbol_table::{SymbolTable, VariableID},
    types::DataType
};
use std::{
    collections::HashMap,
    borrow::Cow
};

pub struct Compiler<'src> {
    symbol_table: SymbolTable<'src>,
    bytecode: ByteCode<'src>,
    variable_registers: HashMap<VariableID<'src>, RegisterID>,
}

impl<'src> Compiler<'src> {
    pub fn compile(ast: Ast<'src>, symbol_table: SymbolTable<'src>) -> String {
        let mut compiler = Self {
            symbol_table,
            bytecode: ByteCode::new(),
            variable_registers: HashMap::new()
        };

        let mut main = Function::new("@main", ast.data_type.clone());

        let dst = Argument::Register(main.return_value);

        compiler.compile_ast(&ast, &mut main, Some(dst));

        compiler.bytecode.add_function(main);

        compiler.bytecode.compile_nasm()
    }

    fn compile_ast(&mut self, ast: &'src Ast<'src>, function: &mut Function<'src>, dst: Option<Argument>) -> Argument {
        match &ast.kind {
            AstKind::Node { ref token } => {
                let node = match &token.kind {
                    TokenKind::Number(value) => Argument::Constant {
                        value: *value,
                        data_type: ast.data_type.clone(),
                    },
                    TokenKind::Ident => {
                        let variable_id = self.symbol_table
                            .get_variable_id(token.text)
                            .expect("Variable not being predefined was not caught in the typechecking phase, this is a compiler bug");

                        self.variable_registers
                            .get(&variable_id)
                            .map_or(
                                Argument::Symbol {
                                    name: token.text.to_string(),
                                    data_type: ast.data_type.clone()
                                },
                                |register| Argument::Register(*register)
                            )
                    },
                    TokenKind::Str(text) => {
                        let text = match &text {
                            Cow::Owned(owned) => owned.as_bytes(),
                            Cow::Borrowed(borrowed) => borrowed.as_bytes()
                        };

                        let name = format!("str_{}", self.bytecode.symbols_len());

                        self.bytecode.add_symbol(name.clone(), text);

                        Argument::Symbol {
                            name,
                            data_type: ast.data_type.clone()
                        }
                    },
                    TokenKind::True => Argument::Constant {
                        value: 1,
                        data_type: DataType::Bool,
                    },
                    TokenKind::False => Argument::Constant {
                        value: 0,
                        data_type: DataType::Bool,
                    },
                    _ => unreachable!(),
                };

                dst.map_or(node.clone(), |dst| {
                    function.add_opcode(OpCode::Mov {
                        dst: dst.clone(),
                        src: node,
                    });
                    dst
                })
            }
            AstKind::Infix {
                ref oper,
                ref lhs,
                ref rhs,
            } => {
                let dst = dst.unwrap_or_else(|| {
                    Argument::Register(function.add_register(ast.data_type.clone()))
                });

                match oper.kind {
                    TokenKind::Add => {
                        let _lhs = self.compile_ast(lhs, function, Some(dst.clone()));
                        let rhs = self.compile_ast(rhs, function, None);

                        function.add_opcode(OpCode::Add {
                            dst: dst.clone(),
                            src: rhs,
                        });
                    }
                    TokenKind::Sub => {
                        let _lhs = self.compile_ast(lhs, function, Some(dst.clone()));
                        let rhs = self.compile_ast(rhs, function, None);

                        function.add_opcode(OpCode::Sub {
                            dst: dst.clone(),
                            src: rhs,
                        });
                    }
                    TokenKind::Mul => {
                        let _lhs = self.compile_ast(lhs, function, Some(dst.clone()));
                        let rhs = self.compile_ast(rhs, function, None);

                        function.add_opcode(OpCode::Mul {
                            dst: dst.clone(),
                            src: rhs,
                        });
                    }
                    TokenKind::Div => {
                        let _lhs = self.compile_ast(lhs, function, Some(dst.clone()));
                        let rhs = self.compile_ast(rhs, function, None);

                        function.add_opcode(OpCode::Div {
                            dst: dst.clone(),
                            src: rhs,
                        });
                    }
                    TokenKind::Mod => {
                        let _lhs = self.compile_ast(lhs, function, Some(dst.clone()));
                        let rhs = self.compile_ast(rhs, function, None);

                        function.add_opcode(OpCode::Mod {
                            dst: dst.clone(),
                            src: rhs,
                        });
                    }
                    TokenKind::Equals => {
                        let lhs = self.compile_ast(lhs, function, None);
                        let rhs = self.compile_ast(rhs, function, None);

                        function.add_opcode(OpCode::SetIfEqual {
                            dst: dst.clone(),
                            lhs,
                            rhs,
                        });
                    }
                    TokenKind::NotEquals => {
                        let lhs = self.compile_ast(lhs, function, None);
                        let rhs = self.compile_ast(rhs, function, None);

                        function.add_opcode(OpCode::SetIfNotEqual {
                            dst: dst.clone(),
                            lhs,
                            rhs,
                        });
                    }
                    TokenKind::Greater => {
                        let lhs = self.compile_ast(lhs, function, None);
                        let rhs = self.compile_ast(rhs, function, None);

                        function.add_opcode(OpCode::SetIfGreater {
                            dst: dst.clone(),
                            lhs,
                            rhs,
                        });
                    }
                    TokenKind::Less => {
                        let lhs = self.compile_ast(lhs, function, None);
                        let rhs = self.compile_ast(rhs, function, None);

                        function.add_opcode(OpCode::SetIfLess {
                            dst: dst.clone(),
                            lhs,
                            rhs,
                        });
                    }
                    TokenKind::GreaterOrEqual => {
                        let lhs = self.compile_ast(lhs, function, None);
                        let rhs = self.compile_ast(rhs, function, None);

                        function.add_opcode(OpCode::SetIfGreaterOrEqual {
                            dst: dst.clone(),
                            lhs,
                            rhs,
                        });
                    }
                    TokenKind::LessOrEqual => {
                        let lhs = self.compile_ast(lhs, function, None);
                        let rhs = self.compile_ast(rhs, function, None);

                        function.add_opcode(OpCode::SetIfLessOrEqual {
                            dst: dst.clone(),
                            lhs,
                            rhs,
                        });
                    }
                    _ => unreachable!(),
                }

                dst
            }
            AstKind::Prefix { ref oper, ref node } => {
                let dst = dst.unwrap_or_else(|| {
                    Argument::Register(function.add_register(ast.data_type.clone()))
                });

                let _node = self.compile_ast(node, function, Some(dst.clone()));

                match oper.kind {
                    TokenKind::Sub => function.add_opcode(OpCode::Negate { dst: dst.clone() }),
                    TokenKind::Not => function.add_opcode(OpCode::Not { dst: dst.clone() }),
                    _ => unreachable!(),
                }

                dst
            }
            AstKind::Assign { ref lhs, ref rhs } => {
                let lhs = self.compile_ast(lhs, function, None);
                let _rhs = self.compile_ast(rhs, function, Some(lhs.clone()));

                Argument::VoidRegister
            }
            AstKind::Block {
                scope_id,
                ref statements,
            } => {
                let dst = dst.unwrap_or_else(|| {
                    if let DataType::Void = ast.data_type {
                        Argument::VoidRegister
                    } else {
                        Argument::Register(function.add_register(ast.data_type.clone()))
                    }
                });

                self.symbol_table.enter_scope(*scope_id);

                for (n, statement) in statements.iter().enumerate() {
                    let _statement = if n + 1 == statements.len() && ast.data_type != DataType::Void
                    {
                        self.compile_ast(statement, function, Some(dst.clone()))
                    } else {
                        self.compile_ast(statement, function, None)
                    };
                }

                self.symbol_table.leave_scope();

                dst
            }
            AstKind::Declaration {
                name,
                ref data_type,
                ref value,
            } => {
                let variable = function.add_register(data_type.clone());

                self.variable_registers.insert(
                    self.symbol_table
                        .get_variable_id(name)
                        .expect("Unreachable: this variable should have been defined in the parsing stage"),
                    variable
                );

                if let Some(value) = value {
                    self.compile_ast(value, function, Some(Argument::Register(variable)));
                }

                Argument::VoidRegister
            }
            AstKind::FunctionDeclaration {
                name,
                ref return_type,
                ref body,
                ..
            } => {
                let mut function = Function::new(name, return_type.clone());

                let dst = Argument::Register(function.return_value); 

                self.compile_ast(body, &mut function, Some(dst));

                self.bytecode.add_function(function);

                Argument::VoidRegister
            }
            AstKind::IfStatement {
                condition,
                if_block,
                else_block,
            } => {
                let dst = dst.unwrap_or_else(|| {
                    if let DataType::Void = ast.data_type {
                        Argument::VoidRegister
                    } else {
                        Argument::Register(function.add_register(ast.data_type.clone()))
                    }
                });

                let condition = self.compile_ast(condition, function, None);

                let else_label = function.add_label();
                let end_label = function.add_label();

                function.add_opcode(OpCode::GotoIfZero {
                    condition,
                    label_id: else_label,
                });

                let _if_block = self.compile_ast(if_block, function, Some(dst.clone()));

                function.add_opcode(OpCode::Goto {
                    label_id: end_label,
                });
                function.add_opcode(OpCode::Label {
                    label_id: else_label,
                });

                if let Some(ref else_block) = else_block {
                    let _else_block = self.compile_ast(else_block, function, Some(dst.clone()));
                }

                function.add_opcode(OpCode::Label {
                    label_id: end_label,
                });

                dst
            }
            AstKind::WhileLoop { condition, body } => {
                let start_label = function.add_label();
                let end_label = function.add_label();

                function.add_opcode(OpCode::Label {
                    label_id: start_label,
                });

                let condition = self.compile_ast(condition, function, None);

                function.add_opcode(OpCode::GotoIfZero {
                    condition,
                    label_id: end_label,
                });

                let _body = self.compile_ast(body, function, None);

                function.add_opcode(OpCode::Goto {
                    label_id: start_label,
                });
                function.add_opcode(OpCode::Label {
                    label_id: end_label,
                });

                Argument::VoidRegister
            }
            AstKind::Call { lhs, arguments } => {
                let dst = dst.unwrap_or_else(|| {
                    if let DataType::Void = ast.data_type {
                        Argument::VoidRegister
                    } else {
                        Argument::Register(function.add_register(ast.data_type.clone()))
                    }
                });

                let lhs = self.compile_ast(lhs, function, None);

                let arguments = arguments
                    .iter()
                    .map(|argument| self.compile_ast(argument, function, None))
                    .collect::<Vec<Argument>>();

                function.add_opcode(OpCode::Call {
                    dst: dst.clone(),
                    lhs,
                    arguments
                });

                dst
            }
        }
    }
}
