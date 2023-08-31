use crate::{
    ast::{Ast, AstKind, VariableDeclaration},
    bytecode::{Argument, ByteCode, Function, OpCode},
    parser::{Token, TokenKind},
    symbol_table::{SymbolID, SymbolTable},
    types::DataType,
};
use std::collections::HashMap;

pub struct Compiler<'src> {
    symbol_table: SymbolTable<'src>,
    variable_registers: HashMap<SymbolID<'src>, Argument<'src>>,
}

impl<'src> Compiler<'src> {
    pub fn compile(ast: &'src Ast<'src>, symbol_table: SymbolTable<'src>) -> ByteCode<'src> {
        let mut bytecode = ByteCode::new();

        let mut compiler = Self {
            symbol_table,
            variable_registers: HashMap::new(),
        };

        let mut main = Function::new("@main", ast.data_type.clone(), Vec::new());

        let data = compiler.compile_ast(ast, &mut bytecode, &mut main);

        main.add_opcode(OpCode::Mov {
            dst: Argument::ReturnValue,
            src: data,
        });

        bytecode.add_function(main);

        bytecode
    }

    pub fn compile_ast(
        &mut self,
        ast: &'src Ast<'src>,
        bytecode: &mut ByteCode<'src>,
        function: &mut Function<'src>,
    ) -> Argument<'src> {
        match &ast.kind {
            AstKind::Node { ref token } => match &token.kind {
                TokenKind::Number(value) => Argument::Constant {
                    value: *value,
                    data_type: ast.data_type.clone(),
                },
                TokenKind::Ident => {
                    let symbol_id = self.symbol_table
                        .get_symbol_id(token.text)
                        .expect("Variable not being predefined was not caught in the typechecking phase, this is a compiler bug");

                    self.variable_registers
                        .get(&symbol_id)
                        .cloned()
                        .unwrap_or(Argument::Symbol {
                            name: token.text.to_string(),
                            data_type: ast.data_type.clone(),
                        })
                }
                TokenKind::Str(text) => Argument::Symbol {
                    name: ByteCode::string_symbol_name(bytecode.add_string(text)),
                    data_type: ast.data_type.clone(),
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
            },
            AstKind::Infix {
                ref oper,
                ref lhs,
                ref rhs,
            } => {
                let dst = Argument::Register(function.add_register(ast.data_type.clone()));

                let lhs = self.compile_ast(lhs, bytecode, function);
                let rhs = self.compile_ast(rhs, bytecode, function);

                match oper.kind {
                    TokenKind::Add => {
                        function.add_opcode(OpCode::Mov {
                            dst: dst.clone(),
                            src: lhs,
                        });

                        function.add_opcode(OpCode::Add {
                            dst: dst.clone(),
                            src: rhs,
                        });
                    }
                    TokenKind::Sub => {
                        function.add_opcode(OpCode::Mov {
                            dst: dst.clone(),
                            src: lhs,
                        });

                        function.add_opcode(OpCode::Sub {
                            dst: dst.clone(),
                            src: rhs,
                        });
                    }
                    TokenKind::Mul => {
                        function.add_opcode(OpCode::Mov {
                            dst: dst.clone(),
                            src: lhs,
                        });

                        function.add_opcode(OpCode::Mul {
                            dst: dst.clone(),
                            src: rhs,
                        });
                    }
                    TokenKind::Div => {
                        function.add_opcode(OpCode::Mov {
                            dst: dst.clone(),
                            src: lhs,
                        });

                        function.add_opcode(OpCode::Div {
                            dst: dst.clone(),
                            src: rhs,
                        });
                    }
                    TokenKind::Mod => {
                        function.add_opcode(OpCode::Mov {
                            dst: dst.clone(),
                            src: lhs,
                        });

                        function.add_opcode(OpCode::Mod {
                            dst: dst.clone(),
                            src: rhs,
                        });
                    }
                    TokenKind::Equals => {
                        function.add_opcode(OpCode::SetIfEqual {
                            dst: dst.clone(),
                            lhs,
                            rhs,
                        });
                    }
                    TokenKind::NotEquals => {
                        function.add_opcode(OpCode::SetIfNotEqual {
                            dst: dst.clone(),
                            lhs,
                            rhs,
                        });
                    }
                    TokenKind::Greater => {
                        function.add_opcode(OpCode::SetIfGreater {
                            dst: dst.clone(),
                            lhs,
                            rhs,
                        });
                    }
                    TokenKind::Less => {
                        function.add_opcode(OpCode::SetIfLess {
                            dst: dst.clone(),
                            lhs,
                            rhs,
                        });
                    }
                    TokenKind::GreaterOrEqual => {
                        function.add_opcode(OpCode::SetIfGreaterOrEqual {
                            dst: dst.clone(),
                            lhs,
                            rhs,
                        });
                    }
                    TokenKind::LessOrEqual => {
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
            AstKind::Index { ref lhs, ref index } => {
                let dst = Argument::Register(function.add_register(ast.data_type.clone()));

                let index = self.compile_ast(index, bytecode, function);
                let lhs = self.compile_ast(lhs, bytecode, function);

                function.add_opcode(OpCode::Index {
                    dst: dst.clone(),
                    src: lhs,
                    index,
                });

                dst
            }
            AstKind::GetField { ref lhs, name } => {
                let lhs = self.compile_ast(lhs, bytecode, function);

                Argument::StructField {
                    data: Box::new(lhs),
                    name,
                }
            }
            AstKind::Prefix { ref oper, ref node } => match oper.kind {
                TokenKind::Sub => {
                    let dst = Argument::Register(function.add_register(ast.data_type.clone()));

                    let node = self.compile_ast(node, bytecode, function);

                    function.add_opcode(OpCode::Mov {
                        dst: dst.clone(),
                        src: node.clone(),
                    });

                    function.add_opcode(OpCode::Negate { dst: dst.clone() });

                    dst
                }
                TokenKind::Not => {
                    let dst = Argument::Register(function.add_register(ast.data_type.clone()));

                    let node = self.compile_ast(node, bytecode, function);

                    function.add_opcode(OpCode::Mov {
                        dst: dst.clone(),
                        src: node.clone(),
                    });

                    function.add_opcode(OpCode::Not { dst: dst.clone() });

                    dst
                }
                TokenKind::Hash => {
                    let dst = Argument::Register(function.add_register(ast.data_type.clone()));

                    let node = self.compile_ast(node, bytecode, function);

                    function.add_opcode(OpCode::Ref {
                        dst: dst.clone(),
                        src: node.clone(),
                    });

                    dst
                }
                TokenKind::AtSymbol => {
                    let node = self.compile_ast(node, bytecode, function);

                    Argument::Deref(Box::new(node))
                }
                _ => unreachable!(),
            },
            AstKind::Assign { ref lhs, ref rhs } => {
                match &lhs.kind {
                    AstKind::Prefix {
                        oper:
                            Token {
                                kind: TokenKind::AtSymbol,
                                ..
                            },
                        ref node,
                    } => {
                        let lhs = self.compile_ast(node, bytecode, function);
                        let rhs = self.compile_ast(rhs, bytecode, function);

                        function.add_opcode(OpCode::SetIndex {
                            dst: lhs,
                            src: rhs,
                            index: Argument::VoidRegister,
                        });
                    }
                    AstKind::Index { ref lhs, ref index } => {
                        let lhs = self.compile_ast(lhs, bytecode, function);
                        let index = self.compile_ast(index, bytecode, function);
                        let rhs = self.compile_ast(rhs, bytecode, function);

                        function.add_opcode(OpCode::SetIndex {
                            dst: lhs,
                            src: rhs,
                            index,
                        });
                    }
                    _ => {
                        let lhs = self.compile_ast(lhs, bytecode, function);
                        let rhs = self.compile_ast(rhs, bytecode, function);

                        function.add_opcode(OpCode::Mov { dst: lhs, src: rhs });
                    }
                }

                Argument::VoidRegister
            }
            AstKind::Block {
                scope_id,
                ref statements,
            } => {
                self.symbol_table.enter_scope(*scope_id);

                let dst = if ast.data_type == DataType::Void {
                    Argument::VoidRegister
                } else {
                    Argument::Register(function.add_register(ast.data_type.clone()))
                };

                for (n, statement) in statements.iter().enumerate() {
                    let statement = self.compile_ast(statement, bytecode, function);

                    if n + 1 == statements.len() && ast.data_type != DataType::Void {
                        function.add_opcode(OpCode::Mov {
                            dst: dst.clone(),
                            src: statement,
                        });
                    };
                }

                self.symbol_table.leave_scope();

                dst
            }
            AstKind::VariableDeclaration(VariableDeclaration {
                name,
                ref data_type,
                ref value,
            }) => {
                let variable = Argument::Register(function.add_register(data_type.clone()));

                self.variable_registers.insert(
                    self.symbol_table.get_symbol_id(name).expect(
                        "Unreachable: this variable should have been defined in the parsing stage",
                    ),
                    variable.clone(),
                );

                if let Some(value) = value {
                    let value = self.compile_ast(value, bytecode, function);
                    function.add_opcode(OpCode::Mov {
                        dst: variable,
                        src: value,
                    });
                }

                Argument::VoidRegister
            }
            AstKind::FunctionDeclaration {
                name,
                scope_id,
                ref return_type,
                ref arguments,
                ref body,
                ..
            } => {
                let argument_types = arguments
                    .iter()
                    .map(|VariableDeclaration { data_type, .. }| data_type.clone())
                    .collect();

                let mut function = Function::new(name, return_type.clone(), argument_types);

                self.symbol_table.enter_scope(*scope_id);

                for (n, declaration) in arguments.iter().enumerate() {
                    self.variable_registers.insert(
                        self.symbol_table.get_symbol_id(declaration.name).expect(
                            "Unreachable: this variable should have been defined in the parsing stage",
                        ),
                        Argument::Argument(n)
                    );
                }

                let return_value = self.compile_ast(body, bytecode, &mut function);

                if *return_type != DataType::Void {
                    function.add_opcode(OpCode::Mov {
                        dst: Argument::ReturnValue,
                        src: return_value,
                    });
                }

                bytecode.add_function(function);

                self.symbol_table.leave_scope();

                Argument::VoidRegister
            }
            AstKind::StructDeclaration { .. } => Argument::VoidRegister,
            AstKind::IfStatement {
                condition,
                if_block,
                else_block,
            } => {
                let dst = if ast.data_type == DataType::Void {
                    Argument::VoidRegister
                } else {
                    Argument::Register(function.add_register(ast.data_type.clone()))
                };

                let condition = self.compile_ast(condition, bytecode, function);

                let else_label = function.add_label();
                let end_label = function.add_label();

                function.add_opcode(OpCode::GotoIfZero {
                    condition,
                    label_id: else_label,
                });

                let if_block = self.compile_ast(if_block, bytecode, function);

                if ast.data_type != DataType::Void {
                    function.add_opcode(OpCode::Mov {
                        dst: dst.clone(),
                        src: if_block,
                    });
                }

                function.add_opcode(OpCode::Goto {
                    label_id: end_label,
                });

                function.add_opcode(OpCode::Label {
                    label_id: else_label,
                });

                if let Some(ref else_block) = else_block {
                    let else_block = self.compile_ast(else_block, bytecode, function);

                    if ast.data_type != DataType::Void {
                        function.add_opcode(OpCode::Mov {
                            dst: dst.clone(),
                            src: else_block,
                        });
                    }
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

                let condition = self.compile_ast(condition, bytecode, function);

                function.add_opcode(OpCode::GotoIfZero {
                    condition,
                    label_id: end_label,
                });

                let _body = self.compile_ast(body, bytecode, function);

                function.add_opcode(OpCode::Goto {
                    label_id: start_label,
                });

                function.add_opcode(OpCode::Label {
                    label_id: end_label,
                });

                Argument::VoidRegister
            }
            AstKind::Call { ref lhs, arguments } => {
                let dst = if ast.data_type == DataType::Void {
                    Argument::VoidRegister
                } else {
                    Argument::Register(function.add_register(ast.data_type.clone()))
                };

                let lhs = self.compile_ast(lhs, bytecode, function);

                let arguments = arguments
                    .iter()
                    .rev()
                    .map(|argument| self.compile_ast(argument, bytecode, function))
                    .collect::<Vec<Argument>>();

                function.add_opcode(OpCode::Call {
                    dst: dst.clone(),
                    lhs,
                    arguments,
                });

                dst
            }
        }
    }
}
