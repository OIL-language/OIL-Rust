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
            function: Function::new("main", DataType::Int(IntType::U64))
        }
    }

    pub fn compile(ast: Ast<'src>, symbol_table: SymbolTable<'src>) -> ByteCode<'src> {
        let mut compiler = Compiler::new(symbol_table);

        compiler.compile_ast(&ast, Some(Argument::Register(compiler.function.return_value)));
        
        let mut bytecode = ByteCode::new();

        bytecode.add_function(compiler.function);

        println!("{bytecode:#?}");

        bytecode
    }

    fn compile_ast(&mut self, ast: &Ast<'src>, dst: Option<Argument>) -> Argument {
        match &ast.kind {
            AstKind::Node { ref token } => {
                let node = match token.kind {
                    TokenKind::Number(value) => Argument::Constant { value, data_type: ast.data_type.clone() },
                    TokenKind::Ident => Argument::Register(
                        self.variable_registers[&self.symbol_table.get_variable_id(token.text).unwrap()]
                    ),
                    TokenKind::True => Argument::Constant { value: 1, data_type: DataType::Bool },
                    TokenKind::False => Argument::Constant { value: 0, data_type: DataType::Bool },
                    _ => unreachable!(),
                };

                dst.map_or(node.clone(), |dst| {
                    self.function.add_opcode(OpCode::Mov { dst: dst.clone(), src: node });
                    dst
                })
            },
            AstKind::Infix {
                ref oper,
                ref lhs,
                ref rhs,
            } => {
                let dst = dst.unwrap_or_else(|| Argument::Register(self.function.add_register(ast.data_type.clone())));

                match oper.kind {
                    TokenKind::Add => {
                        let _lhs = self.compile_ast(lhs, Some(dst.clone()));
                        let rhs = self.compile_ast(rhs, None);

                        self.function.add_opcode(OpCode::Add { dst: dst.clone(), src: rhs });
                    },
                    TokenKind::Sub => {
                        let _lhs = self.compile_ast(lhs, Some(dst.clone()));
                        let rhs = self.compile_ast(rhs, None);

                        self.function.add_opcode(OpCode::Sub { dst: dst.clone(), src: rhs });
                    },
                    TokenKind::Mul => {
                        let _lhs = self.compile_ast(lhs, Some(dst.clone()));
                        let rhs = self.compile_ast(rhs, None);

                        self.function.add_opcode(OpCode::Mul { dst: dst.clone(), src: rhs });
                    },
                    TokenKind::Div => {
                        let _lhs = self.compile_ast(lhs, Some(dst.clone()));
                        let rhs = self.compile_ast(rhs, None);

                        self.function.add_opcode(OpCode::Div { dst: dst.clone(), src: rhs });
                    },
                    TokenKind::Mod => {
                        let _lhs = self.compile_ast(lhs, Some(dst.clone()));
                        let rhs = self.compile_ast(rhs, None);

                        self.function.add_opcode(OpCode::Mod { dst: dst.clone(), src: rhs });
                    },
                    TokenKind::Equals => {
                        let lhs = self.compile_ast(lhs, None);
                        let rhs = self.compile_ast(rhs, None);

                        self.function.add_opcode(OpCode::SetIfEqual { dst: dst.clone(), lhs, rhs });
                    },
                    TokenKind::Greater => {
                        let lhs = self.compile_ast(lhs, None);
                        let rhs = self.compile_ast(rhs, None);

                        self.function.add_opcode(OpCode::SetIfGreater { dst: dst.clone(), lhs, rhs });
                    },
                    TokenKind::Less => {
                        let lhs = self.compile_ast(lhs, None);
                        let rhs = self.compile_ast(rhs, None);

                        self.function.add_opcode(OpCode::SetIfLess { dst: dst.clone(), lhs, rhs });
                    },
                    _ => unreachable!(),
                }

                dst
            },
            AstKind::Prefix {
                ref oper,
                ref node
            } => {
                let dst = dst.unwrap_or_else(|| Argument::Register(self.function.add_register(ast.data_type.clone())));

                let _node = self.compile_ast(node, Some(dst.clone()));
                
                match oper.kind {
                    TokenKind::Sub => self.function.add_opcode(OpCode::Negate { dst: dst.clone() }),
                    _ => unreachable!()
                }

                dst
            },
            AstKind::Assign {
                ref lhs,
                ref rhs,
            } => {
                let lhs = self.compile_ast(lhs, None);
                let _rhs = self.compile_ast(rhs, Some(lhs.clone()));

                Argument::NullRegister
            },
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
                        self.compile_ast(statement, Some(dst.clone()))
                    } else {
                        self.compile_ast(statement, None)
                    };
                }

                self.symbol_table.leave_scope();

                dst
            },
            AstKind::Declaration { name, ref data_type, ref value } => {
                let variable = self.function.add_register(data_type.clone());

                self.variable_registers.insert(self.symbol_table.get_variable_id(name).unwrap(), variable);

                if let Some(value) = value {
                    self.compile_ast(value, Some(Argument::Register(variable)));
                }

                Argument::NullRegister
            },
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

                let _if_block = self.compile_ast(if_block, Some(dst.clone()));

                self.function.add_opcode(OpCode::Goto { label_id: end_label });
                self.function.add_opcode(OpCode::Label { label_id: else_label });

                if let Some(ref else_block) = else_block {
                    let _else_block = self.compile_ast(else_block, Some(dst.clone()));
                }

                self.function.add_opcode(OpCode::Label { label_id: end_label });

                dst
            },
            AstKind::WhileLoop { condition, body } => {
                let start_label = self.function.add_label();
                let end_label = self.function.add_label();

                self.function.add_opcode(OpCode::Label { label_id: start_label });

                let condition = self.compile_ast(condition, None);

                self.function.add_opcode(OpCode::GotoIfZero { condition, label_id: end_label });

                let _body = self.compile_ast(body, None);

                self.function.add_opcode(OpCode::Goto { label_id: start_label });
                self.function.add_opcode(OpCode::Label { label_id: end_label });

                Argument::NullRegister
            },
        }
    }
}
