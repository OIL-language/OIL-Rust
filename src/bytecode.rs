use crate::types::{DataType, IntType};

fn nasm_oper_text(data_type: &DataType) -> &str {
    match data_type {
        DataType::Int(IntType::U8 | IntType::S8) => "byte",
        DataType::Int(IntType::U16 | IntType::S16) => "word",
        DataType::Int(IntType::U32 | IntType::S32) => "dword",
        DataType::Int(IntType::U64 | IntType::S64) | DataType::Ref(_) => "qword",
        _ => unreachable!(),
    }
}

#[derive(Debug, Clone)]
pub struct Register {
    pub data_type: DataType,
    pub stack_pos: usize,
}

#[derive(Debug, Copy, Clone)]
pub enum Argument {
    Constant { value: u64 },
    Register(usize),
}

#[derive(Debug, Copy, Clone)]
pub enum OpCode {
    Mov { dst: Argument, src: Argument },
    Add { dst: Argument, src: Argument },
    Sub { dst: Argument, src: Argument },
    Mul { dst: Argument, src: Argument },
    Div { dst: Argument, src: Argument },
    Mod { dst: Argument, src: Argument },
    Deref { dst: Argument, src: Argument },
}

impl OpCode {
    fn nasm_instruction(&self) -> &str {
        match self {
            Self::Mov { .. } => "mov",
            Self::Add { .. } => "add",
            Self::Sub { .. } => "sub",
            Self::Mul { .. } => "mul",
            Self::Div { .. } => "div",
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct Function<'a> {
    name: &'a str,
    registers: Vec<Register>,
    opcodes: Vec<OpCode>,
}

impl<'a> Function<'a> {
    pub fn new(name: &'a str, return_type: DataType) -> Self {
        let mut function = Self {
            name,
            registers: Vec::new(),
            opcodes: Vec::new(),
        };

        function.add_register(return_type);

        function
    }

    pub fn stack_size(&self) -> usize {
        self.registers.iter()
            .last()
            .map_or(0, |register| register.stack_pos + register.data_type.size())
    }

    pub fn add_register(&mut self, data_type: DataType) -> Argument {
        self.registers.push(Register {
            data_type,
            stack_pos: self.stack_size(),
        });

        Argument::Register(self.registers.len() - 1)
    }

    pub fn add_opcode(&mut self, opcode: OpCode) {
        self.opcodes.push(opcode);
    }

    fn compile_nasm_argument(&self, argument: Argument) -> String {
        match argument {
            Argument::Constant { value } => value.to_string(),
            Argument::Register(register) => {
                let register = &self.registers[register];
                format!(
                    "{} [rsp + {}]",
                    nasm_oper_text(&register.data_type),
                    register.stack_pos
                )
            }
        }
    }

    fn compile_nasm_opcode(&self, opcode: OpCode) -> String {
        let instruction = opcode.nasm_instruction();

        match opcode {
            | OpCode::Mov { dst, src }
            | OpCode::Add { dst, src }
            | OpCode::Sub { dst, src } => {
                let src_compiled = self.compile_nasm_argument(src);
                let dst_compiled = self.compile_nasm_argument(dst);

                if let Argument::Register(_) = src {
                    format!("    mov rax, {src_compiled}\n    {instruction} {dst_compiled}, rax")
                } else {
                    format!("    {instruction} {dst_compiled}, {src_compiled}")
                }
            }
            | OpCode::Mul { dst, src }
            | OpCode::Div { dst, src } => {
                let src_compiled = self.compile_nasm_argument(src);
                let dst_compiled = self.compile_nasm_argument(dst);

                format!("    mov rax, {dst_compiled}\n    mov rbx, {src_compiled}\n    {instruction} rbx\n    mov {dst_compiled}, rax")
            }
            _ => todo!(),
        }
    }

    pub fn compile_nasm(&self) -> String {
        let code = self.opcodes.iter()
            .map(|opcode| self.compile_nasm_opcode(*opcode) + "\n")
            .collect::<String>();

        format!(
            "{}:\n    enter {}, 0\n{code}    mov rax, [rsp]\n    leave\n    ret\n",
            self.name,
            self.stack_size()
        )
    }
}

#[derive(Debug)]
pub struct ByteCode<'a> {
    functions: Vec<Function<'a>>,
}

impl<'a> ByteCode<'a> {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
        }
    }

    pub fn add_function(&mut self, function: Function<'a>) {
        self.functions.push(function);
    }

    pub fn compile_nasm(&self) -> String {
        format!(
            "\
[BITS 64]
global _start
section .text
_start:
    call main
    mov rdi, rax
    mov rax, 60
    syscall
{code}",
            code = self
                .functions
                .iter()
                .map(|function| function.compile_nasm())
                .collect::<String>()
        )
    }
}

impl Default for ByteCode<'_> {
    fn default() -> Self {
        Self::new()
    }
}
