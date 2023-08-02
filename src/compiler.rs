use crate::{
    ast::{Ast, AstKind},
    bytecode::{Argument, ByteCode, Function, OpCode, RegisterID},
    parser::TokenKind,
    symbol_table::{SymbolTable, VariableID},
    types::DataType,
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
            function: Function::new("main", DataType::Void)
        }
    }

    pub fn compile(ast: Ast<'src>, symbol_table: SymbolTable<'src>) -> ByteCode<'src> {
        let mut compiler = Compiler::new(symbol_table);

        compiler.compile_ast(&ast);
        
        let mut bytecode = ByteCode::new();

        bytecode.add_function(compiler.function);

        bytecode
    }

    fn compile_ast(&mut self, ast: &Ast<'src>) -> Argument {
        match &ast.kind {
            AstKind::Node { ref token } => match token.kind {
                TokenKind::Number(value) => Argument::Constant { value },
                TokenKind::Ident => {
                    let register = self.symbol_table.get_variable_id(token.text);

                    Argument::Register(self.variable_registers[&register.unwrap()]) // we unwrap deliberately
                },
                _ => unreachable!(),
            },
            AstKind::Infix {
                ref oper,
                ref lhs,
                ref rhs,
            } => {
                let dst = Argument::Register(self.function.add_register(lhs.data_type.clone()));

                let lhs = self.compile_ast(lhs);

                self.function.add_opcode(OpCode::Mov { dst, src: lhs });

                let rhs = self.compile_ast(rhs);

                match oper.kind {
                    TokenKind::Add => self.function.add_opcode(OpCode::Add { dst: dst, src: rhs }),
                    TokenKind::Sub => self.function.add_opcode(OpCode::Sub { dst: dst, src: rhs }),
                    TokenKind::Mul => self.function.add_opcode(OpCode::Mul { dst: dst, src: rhs }),
                    TokenKind::Div => self.function.add_opcode(OpCode::Div { dst: dst, src: rhs }),
                    TokenKind::Mod => self.function.add_opcode(OpCode::Mod { dst: dst, src: rhs }),
                    _ => unreachable!(),
                }

                dst
            }
            AstKind::Assign {
                ref lhs,
                ref rhs,
            } => {
                let lhs = self.compile_ast(lhs);
                let rhs = self.compile_ast(rhs);

                self.function.add_opcode(OpCode::Mov { dst: lhs, src: rhs });

                Argument::Register(self.function.add_register(DataType::Void))
            }
            AstKind::Block { scope_id, ref statements } => {
                let dst = Argument::Register(self.function.add_register(ast.data_type.clone()));

                self.symbol_table.enter_scope(*scope_id);

                for (n, statement) in statements.iter().enumerate() {
                    let statement = self.compile_ast(statement);

                    if n + 1 == statements.len() && ast.data_type != DataType::Void {
                        self.function.add_opcode(OpCode::Mov { dst, src: statement });
                    }
                }

                self.symbol_table.leave_scope();

                dst
            }
            AstKind::Declaration { ref name, ref data_type, ref value } => {
                let variable = self.function.add_register(data_type.clone());

                self.variable_registers.insert(self.symbol_table.get_variable_id(name).unwrap(), variable);

                if let Some(value) = value {
                    let value = self.compile_ast(value);

                    self.function.add_opcode(OpCode::Mov { dst: Argument::Register(variable), src: value });
                }

                Argument::Register(self.function.add_register(DataType::Void))
            }
            _ => todo!(),
        }
    }
}
