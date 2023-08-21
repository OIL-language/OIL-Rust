use crate::{
    ast::{Ast, AstKind},
    bytecode::{Argument, ByteCode, Function, OpCode},
    parser::{Token, TokenKind},
    symbol_table::{SymbolTable, VariableID},
    types::DataType,
};
use std::{
    borrow::Cow,
    collections::HashMap
};

pub struct Compiler<'src> {
    symbol_table: SymbolTable<'src>,
    bytecode: ByteCode<'src>,
    variable_registers: HashMap<VariableID<'src>, Argument>,
}

impl<'src> Compiler<'src> {
    pub fn new(symbol_table: SymbolTable<'src>) -> Self {
        Self {
            symbol_table,
            bytecode: ByteCode::new(),
            variable_registers: HashMap::new()
        }
    }

    pub fn bytecode(self) -> ByteCode<'src> { self.bytecode }

    pub fn compile_ast(&mut self, ast: &'src Ast<'src>, function: &mut Function<'src>) -> Argument {
        match &ast.kind {
            AstKind::Node { ref token } => match &token.kind {
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
                        .cloned()
                        .unwrap_or(Argument::Symbol {
                            name: token.text.to_string(),
                            data_type: ast.data_type.clone(),
                        })
                }
                TokenKind::Str(text) => {
                    let text = match &text {
                        Cow::Owned(owned) => owned.as_bytes(),
                        Cow::Borrowed(borrowed) => borrowed.as_bytes(),
                    };

                    let name = format!("str_{}", self.bytecode.symbols_len());

                    self.bytecode.add_symbol(name.clone(), text);

                    Argument::Symbol {
                        name,
                        data_type: ast.data_type.clone(),
                    }
                }
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

                let lhs = self.compile_ast(lhs, function);
                let rhs = self.compile_ast(rhs, function);

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
            AstKind::Prefix { ref oper, ref node } => {
                let dst = Argument::Register(function.add_register(ast.data_type.clone()));

                let node = self.compile_ast(node, function);

                match oper.kind {
                    TokenKind::Sub => {
                        function.add_opcode(OpCode::Mov {
                            dst: dst.clone(),
                            src: node.clone(),
                        });

                        function.add_opcode(OpCode::Negate { dst: dst.clone() });
                    }
                    TokenKind::Not => {
                        function.add_opcode(OpCode::Mov {
                            dst: dst.clone(),
                            src: node.clone(),
                        });

                        function.add_opcode(OpCode::Not { dst: dst.clone() });
                    }
                    TokenKind::Hash => {
                        function.add_opcode(OpCode::Ref {
                            dst: dst.clone(),
                            src: node.clone(),
                        });
                    }
                    TokenKind::AtSymbol => {
                        function.add_opcode(OpCode::Deref {
                            dst: dst.clone(),
                            src: node.clone(),
                        });
                    }
                    _ => unreachable!(),
                }

                dst
            }
            AstKind::Assign { ref lhs, ref rhs } => {
                if let AstKind::Prefix {
                    oper: Token {
                        kind: TokenKind::AtSymbol,
                        ..
                    },
                    ref node
                } = &lhs.kind {
                    let lhs = self.compile_ast(node, function);
                    let rhs = self.compile_ast(rhs, function);

                    function.add_opcode(OpCode::DerefMov { dst: lhs, src: rhs });
                } else {
                    let lhs = self.compile_ast(lhs, function);
                    let rhs = self.compile_ast(rhs, function);

                    function.add_opcode(OpCode::Mov { dst: lhs, src: rhs });
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
                    let statement = self.compile_ast(statement, function);

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
            AstKind::Declaration {
                name,
                ref data_type,
                argument_id,
                ref value,
            } => {
                let variable = if let Some(argument_id) = argument_id {
                    Argument::Argument(*argument_id)
                } else {
                    Argument::Register(function.add_register(data_type.clone()))
                };

                self.variable_registers.insert(
                    self.symbol_table.get_variable_id(name).expect(
                        "Unreachable: this variable should have been defined in the parsing stage",
                    ),
                    variable.clone(),
                );

                if let Some(value) = value {
                    let value = self.compile_ast(value, function);
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
                let argument_types = arguments.iter()
                    .map(|ast| {
                        let AstKind::Declaration { ref data_type, .. } = ast.kind else {
                            unreachable!("There can only be declarations in the arguments space of a function")
                        };

                        data_type.clone()
                    })
                    .collect();

                let mut function = Function::new(name, return_type.clone(), argument_types);

                self.symbol_table.enter_scope(*scope_id);

                for argument in arguments {
                    let _ = self.compile_ast(argument, &mut function);
                }

                let return_value = self.compile_ast(body, &mut function);

                if *return_type != DataType::Void {
                    function.add_opcode(OpCode::Mov { dst: Argument::ReturnValue, src: return_value });
                }

                self.bytecode.add_function(function);

                self.symbol_table.leave_scope();

                Argument::VoidRegister
            }
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

                let condition = self.compile_ast(condition, function);

                let else_label = function.add_label();
                let end_label = function.add_label();

                function.add_opcode(OpCode::GotoIfZero {
                    condition,
                    label_id: else_label,
                });

                let if_block = self.compile_ast(if_block, function);

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
                    let else_block = self.compile_ast(else_block, function);

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

                let condition = self.compile_ast(condition, function);

                function.add_opcode(OpCode::GotoIfZero {
                    condition,
                    label_id: end_label,
                });

                let _body = self.compile_ast(body, function);

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

                let lhs = self.compile_ast(lhs, function);

                let arguments = arguments
                    .iter()
                    .rev()
                    .map(|argument| self.compile_ast(argument, function))
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
