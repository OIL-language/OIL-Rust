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

pub enum NasmRegister {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rdi,
    Rsi,
    Rsp,
    Rbp,
    R8,
    R9,
    R10,
    R11,
}

impl NasmRegister {
    fn register_text(&self, data_type: &DataType) -> &str {
        let text_options = match self {
            Self::Rax => &["al", "ax", "eax", "rax"],
            Self::Rbx => &["bl", "bx", "ebx", "rbx"],
            Self::Rcx => &["cl", "cx", "ecx", "rcx"],
            Self::Rdx => &["dl", "dx", "edx", "rdx"],
            Self::Rsi => &["sil", "si", "esi", "rsi"],
            Self::Rdi => &["dil", "di", "edi", "rdi"],
            Self::Rsp => &["spl", "sp", "esp", "rsp"],
            Self::Rbp => &["bpl", "bp", "ebp", "rbp"],
            Self::R8 => &["r8b", "r8d", "r8w", "r8"],
            Self::R9 => &["r9b", "r9d", "r9w", "r9"],
            Self::R10 => &["r10b", "r10d", "r10w", "r10"],
            Self::R11 => &["r11b", "r11d", "r11w", "r11"],
        };

        match data_type {
            DataType::Int(IntType::U8 | IntType::S8) => text_options[0],
            DataType::Int(IntType::U16 | IntType::S16) => text_options[1],
            DataType::Int(IntType::U32 | IntType::S32) => text_options[2],
            DataType::Int(IntType::U64 | IntType::S64) | DataType::Ref(_) => text_options[3],
            _ => unreachable!(),
        }
    }
}

pub type RegisterID = usize;

#[derive(Debug, Clone)]
pub struct Register {
    pub data_type: DataType,
    pub stack_pos: usize,
}

#[derive(Debug, Copy, Clone)]
pub enum Argument {
    Constant { value: u64 },
    Register(RegisterID),
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
pub struct Function<'src> {
    name: &'src str,
    registers: Vec<Register>,
    opcodes: Vec<OpCode>,
}

impl<'src> Function<'src> {
    pub fn new(name: &'src str, return_type: DataType) -> Self {
        let mut function = Self {
            name,
            registers: Vec::new(),
            opcodes: Vec::new(),
        };

        function.add_register(return_type);

        function
    }

    pub fn stack_size(&self) -> usize {
        self.registers
            .iter()
            .last()
            .map_or(0, |register| register.stack_pos + register.data_type.size())
    }

    pub fn add_register(&mut self, data_type: DataType) -> usize {
        self.registers.push(Register {
            data_type,
            stack_pos: self.stack_size(),
        });

        self.registers.len() - 1
    }

    pub fn get_register(&mut self, id: RegisterID) -> &Register {
        &self.registers[id]
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
            OpCode::Mov { dst, src } | OpCode::Add { dst, src } | OpCode::Sub { dst, src } => {
                let dst_register = match dst {
                    Argument::Register(register) => &self.registers[register],
                    _ => unreachable!(),
                };
                let src_compiled = self.compile_nasm_argument(src);
                let dst_compiled = self.compile_nasm_argument(dst);

                let rax = NasmRegister::Rax.register_text(&dst_register.data_type);

                if let Argument::Register(_) = src {
                    format!(
                        "    mov {rax}, {src_compiled}\n    {instruction} {dst_compiled}, {rax}"
                    )
                } else {
                    format!("    {instruction} {dst_compiled}, {src_compiled}")
                }
            }
            OpCode::Mul { dst, src } | OpCode::Div { dst, src } => {
                let dst_register = match dst {
                    Argument::Register(register) => &self.registers[register],
                    _ => unreachable!(),
                };
                let src_compiled = self.compile_nasm_argument(src);
                let dst_compiled = self.compile_nasm_argument(dst);

                let rax = NasmRegister::Rax.register_text(&dst_register.data_type);
                let rbx = NasmRegister::Rbx.register_text(&dst_register.data_type);

                format!("    mov {rax}, {dst_compiled}\n    mov {rbx}, {src_compiled}\n    {instruction} {rbx}\n    mov {dst_compiled}, {rax}")
            }
            _ => todo!(),
        }
    }

    pub fn compile_nasm(&self) -> String {
        let code = self
            .opcodes
            .iter()
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
pub struct ByteCode<'src> {
    functions: Vec<Function<'src>>,
}

impl<'src> ByteCode<'src> {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
        }
    }

    pub fn add_function(&mut self, function: Function<'src>) {
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
