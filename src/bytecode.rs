use crate::types::DataType;
use std::iter;

fn nasm_oper_text(data_type: &DataType) -> &str {
    match data_type.size() {
        1 => "byte",
        2 => "word",
        4 => "dword",
        8 => "qword",
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

        text_options[match data_type.size() {
            1 => 0,
            2 => 1,
            4 => 2,
            8 => 3,
            _ => unreachable!(),
        }]
    }
}

pub type RegisterID = usize;
pub type LabelID = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Argument {
    Constant { value: u64, data_type: DataType },
    Register(RegisterID),
    Argument(RegisterID),
    Symbol { name: String, data_type: DataType },
    VoidRegister,
}

impl Argument {
    fn is_not_nasm_comparable(&self) -> bool {
        matches!(self, Self::Register(_) | Self::Argument(_))
    }
}

#[derive(Debug, Clone)]
pub enum OpCode {
    Mov {
        dst: Argument,
        src: Argument,
    },
    Add {
        dst: Argument,
        src: Argument,
    },
    Sub {
        dst: Argument,
        src: Argument,
    },
    Mul {
        dst: Argument,
        src: Argument,
    },
    Div {
        dst: Argument,
        src: Argument,
    },
    Mod {
        dst: Argument,
        src: Argument,
    },
    Not {
        dst: Argument,
    },
    Ref {
        dst: Argument,
        src: Argument,
    },
    Deref {
        dst: Argument,
        src: Argument,
    },
    SetIfEqual {
        dst: Argument,
        lhs: Argument,
        rhs: Argument,
    },
    SetIfNotEqual {
        dst: Argument,
        lhs: Argument,
        rhs: Argument,
    },
    SetIfGreater {
        dst: Argument,
        lhs: Argument,
        rhs: Argument,
    },
    SetIfLess {
        dst: Argument,
        lhs: Argument,
        rhs: Argument,
    },
    SetIfGreaterOrEqual {
        dst: Argument,
        lhs: Argument,
        rhs: Argument,
    },
    SetIfLessOrEqual {
        dst: Argument,
        lhs: Argument,
        rhs: Argument,
    },
    Negate {
        dst: Argument,
    },
    Label {
        label_id: LabelID,
    },
    Goto {
        label_id: LabelID,
    },
    GotoIfZero {
        condition: Argument,
        label_id: LabelID,
    },
    GotoIfNotZero {
        condition: Argument,
        label_id: LabelID,
    },
    Call {
        dst: Argument,
        lhs: Argument,
        arguments: Vec<Argument>,
    },
}

#[derive(Debug)]
pub struct Function<'src> {
    name: &'src str,
    label_id: LabelID,
    arguments: Vec<DataType>,
    arguments_size: usize,
    registers: Vec<DataType>,
    registers_size: usize,
    opcodes: Vec<OpCode>,
}

impl<'src> Function<'src> {
    pub fn new<'a>(name: &'src str, arguments: Vec<DataType>, return_type: DataType) -> Self {
        let (arguments, arguments_size) = iter::once(return_type.clone()).chain(arguments).fold(
            (Vec::new(), 0),
            |(mut vec, stack_size), data_type| {
                let size = data_type.size_aligned();

                vec.push(data_type);

                (vec, stack_size + size)
            },
        );

        let mut function = Self {
            name,
            label_id: 0,
            arguments,
            arguments_size,
            registers: Vec::new(),
            registers_size: 0,
            opcodes: Vec::new(),
        };

        function.add_register(return_type);

        function
    }

    // kinda useless
    #[inline(always)]
    pub const fn return_value(&self) -> Argument {
        Argument::Argument(0)
    }

    pub fn stack_size(&self) -> usize {
        self.arguments_size + self.registers_size
    }

    pub fn add_register(&mut self, data_type: DataType) -> RegisterID {
        self.registers_size += data_type.size_aligned();

        self.registers.push(data_type);

        self.registers.len() - 1
    }

    fn register_position(&self, register_id: RegisterID) -> usize {
        self.registers
            .iter()
            .take(register_id)
            .fold(0, |n, data_type| n + data_type.size_aligned())
    }

    fn argument_position(&self, argument_id: RegisterID) -> usize {
        self.arguments
            .iter()
            .take(argument_id)
            .fold(0, |n, data_type| n + data_type.size_aligned())
    }

    pub fn add_label(&mut self) -> LabelID {
        let prev_label_id = self.label_id;

        self.label_id += 1;

        prev_label_id
    }

    pub fn argument_data_type(&self, argument: &'src Argument) -> &DataType {
        match argument {
            Argument::Constant { data_type, .. } | Argument::Symbol { data_type, .. } => data_type,
            Argument::Register(register_id) => &self.registers[*register_id],
            Argument::Argument(argument_id) => &self.arguments[*argument_id],
            Argument::VoidRegister => unreachable!(),
        }
    }

    pub fn add_opcode(&mut self, opcode: OpCode) {
        self.opcodes.push(opcode);
    }

    fn compile_nasm_argument(&self, argument: &Argument) -> String {
        match argument {
            Argument::Constant { value, .. } => value.to_string(),
            Argument::Register(register_id) => {
                format!(
                    "{} [rbp - {}]",
                    nasm_oper_text(&self.registers[*register_id]),
                    8 + self.register_position(*register_id)
                )
            }
            Argument::Argument(argument_id) => {
                format!(
                    "{} [rbp + {}]",
                    nasm_oper_text(&self.arguments[*argument_id]),
                    8 + self.arguments_size - self.argument_position(*argument_id)
                )
            }
            Argument::Symbol { name, .. } => name.clone(),
            Argument::VoidRegister => unreachable!(),
        }
    }

    fn compile_nasm_opcode(&self, opcode: &OpCode) -> Option<String> {
        let data = match opcode {
            OpCode::Mov { dst, src } => {
                if src == dst {
                    return None;
                }

                let rax = NasmRegister::Rax.register_text(self.argument_data_type(dst));

                let src_compiled = self.compile_nasm_argument(src);
                let dst_compiled = self.compile_nasm_argument(dst);

                if src.is_not_nasm_comparable() {
                    format!("    mov {rax}, {src_compiled}\n    mov {dst_compiled}, {rax}\n")
                } else {
                    format!("    mov {dst_compiled}, {src_compiled}\n")
                }
            }
            OpCode::Add { dst, src } => {
                let rax = NasmRegister::Rax.register_text(self.argument_data_type(dst));

                let src_compiled = self.compile_nasm_argument(src);
                let dst_compiled = self.compile_nasm_argument(dst);

                if src.is_not_nasm_comparable() {
                    format!("    mov {rax}, {src_compiled}\n    add {dst_compiled}, {rax}\n")
                } else {
                    format!("    add {dst_compiled}, {src_compiled}\n")
                }
            }
            OpCode::Sub { dst, src } => {
                let rax = NasmRegister::Rax.register_text(self.argument_data_type(dst));

                let src_compiled = self.compile_nasm_argument(src);
                let dst_compiled = self.compile_nasm_argument(dst);

                if src.is_not_nasm_comparable() {
                    format!("    mov {rax}, {src_compiled}\n    sub {dst_compiled}, {rax}\n")
                } else {
                    format!("    sub {dst_compiled}, {src_compiled}\n")
                }
            }
            OpCode::Mul { dst, src } => {
                let rax = NasmRegister::Rax.register_text(self.argument_data_type(dst));
                let rbx = NasmRegister::Rbx.register_text(self.argument_data_type(dst));

                let src_compiled = self.compile_nasm_argument(src);
                let dst_compiled = self.compile_nasm_argument(dst);

                format!("    mov {rax}, {dst_compiled}\n    mov {rbx}, {src_compiled}\n    mul {rbx}\n    mov {dst_compiled}, {rax}\n")
            }
            OpCode::Div { dst, src } => {
                let rax = NasmRegister::Rax.register_text(self.argument_data_type(dst));
                let rbx = NasmRegister::Rbx.register_text(self.argument_data_type(dst));
                let rdx = NasmRegister::Rdx.register_text(self.argument_data_type(dst));

                let src_compiled = self.compile_nasm_argument(src);
                let dst_compiled = self.compile_nasm_argument(dst);

                format!("    mov {rdx}, 0\n    mov {rax}, {dst_compiled}\n    mov {rbx}, {src_compiled}\n    div {rbx}\n    mov {dst_compiled}, {rax}\n")
            }
            OpCode::Mod { dst, src } => {
                let rax = NasmRegister::Rax.register_text(self.argument_data_type(dst));
                let rbx = NasmRegister::Rbx.register_text(self.argument_data_type(dst));
                let rdx = NasmRegister::Rdx.register_text(self.argument_data_type(dst));

                let src_compiled = self.compile_nasm_argument(src);
                let dst_compiled = self.compile_nasm_argument(dst);

                format!("    mov {rdx}, 0\n    mov {rax}, {dst_compiled}\n    mov {rbx}, {src_compiled}\n    div {rbx}\n    mov {dst_compiled}, {rdx}\n")
            }
            OpCode::Not { dst } => {
                let dst_compiled = self.compile_nasm_argument(dst);

                format!("    and {dst_compiled}, 0x1\n    xor {dst_compiled}, 0x1\n")
            }
            OpCode::Ref { dst, src } => {
                let dst_compiled = self.compile_nasm_argument(dst);
                let src_compiled = self.compile_nasm_argument(dst);

                let rax = NasmRegister::Rax.register_text(self.argument_data_type(src));

                format!("    lea {rax}, {dst_compiled}\n    mov {src_compiled}, {rax}\n")
            }
            OpCode::Deref { dst, src } => {
                let dst_compiled = self.compile_nasm_argument(dst);
                let src_compiled = self.compile_nasm_argument(src);

                let rax = NasmRegister::Rax.register_text(self.argument_data_type(src));
                let rbx = NasmRegister::Rbx.register_text(self.argument_data_type(dst));

                format!("    mov {rax}, {src_compiled}\n    mov {rbx}, [{rax}]\n    mov {dst_compiled}, {rbx}\n")
            }
            OpCode::SetIfEqual { dst, lhs, rhs } => {
                assert_eq!(*self.argument_data_type(dst), DataType::Bool);

                let rax = NasmRegister::Rax.register_text(self.argument_data_type(lhs));

                let lhs_compiled = self.compile_nasm_argument(lhs);
                let rhs_compiled = self.compile_nasm_argument(rhs);
                let dst_compiled = self.compile_nasm_argument(dst);

                format!("    mov {rax}, {lhs_compiled}\n    cmp {rax}, {rhs_compiled}\n    sete {dst_compiled}\n")
            }
            OpCode::SetIfNotEqual { dst, lhs, rhs } => {
                assert_eq!(*self.argument_data_type(dst), DataType::Bool);

                let rax = NasmRegister::Rax.register_text(self.argument_data_type(lhs));

                let lhs_compiled = self.compile_nasm_argument(lhs);
                let rhs_compiled = self.compile_nasm_argument(rhs);
                let dst_compiled = self.compile_nasm_argument(dst);

                format!("    mov {rax}, {lhs_compiled}\n    cmp {rax}, {rhs_compiled}\n    setne {dst_compiled}\n")
            }
            OpCode::SetIfGreater { dst, lhs, rhs } => {
                assert_eq!(*self.argument_data_type(dst), DataType::Bool);

                let rax = NasmRegister::Rax.register_text(self.argument_data_type(lhs));

                let lhs_compiled = self.compile_nasm_argument(lhs);
                let rhs_compiled = self.compile_nasm_argument(rhs);
                let dst_compiled = self.compile_nasm_argument(dst);

                format!("    mov {rax}, {lhs_compiled}\n    cmp {rax}, {rhs_compiled}\n    setg {dst_compiled}\n")
            }
            OpCode::SetIfLess { dst, lhs, rhs } => {
                assert_eq!(*self.argument_data_type(dst), DataType::Bool);

                let rax = NasmRegister::Rax.register_text(self.argument_data_type(lhs));

                let lhs_compiled = self.compile_nasm_argument(lhs);
                let rhs_compiled = self.compile_nasm_argument(rhs);
                let dst_compiled = self.compile_nasm_argument(dst);

                format!("    mov {rax}, {lhs_compiled}\n    cmp {rax}, {rhs_compiled}\n    setl {dst_compiled}\n")
            }
            OpCode::SetIfGreaterOrEqual { dst, lhs, rhs } => {
                assert_eq!(*self.argument_data_type(dst), DataType::Bool);

                let rax = NasmRegister::Rax.register_text(self.argument_data_type(lhs));

                let lhs_compiled = self.compile_nasm_argument(lhs);
                let rhs_compiled = self.compile_nasm_argument(rhs);
                let dst_compiled = self.compile_nasm_argument(dst);

                format!("    mov {rax}, {lhs_compiled}\n    cmp {rax}, {rhs_compiled}\n    setge {dst_compiled}\n")
            }
            OpCode::SetIfLessOrEqual { dst, lhs, rhs } => {
                assert_eq!(*self.argument_data_type(dst), DataType::Bool);

                let rax = NasmRegister::Rax.register_text(self.argument_data_type(lhs));

                let lhs_compiled = self.compile_nasm_argument(lhs);
                let rhs_compiled = self.compile_nasm_argument(rhs);
                let dst_compiled = self.compile_nasm_argument(dst);

                format!("    mov {rax}, {lhs_compiled}\n    cmp {rax}, {rhs_compiled}\n    setle {dst_compiled}\n")
            }
            OpCode::Negate { dst } => {
                let dst_compiled = self.compile_nasm_argument(dst);

                format!("neg {dst_compiled}\n")
            }
            OpCode::Label { label_id } => format!(".L{label_id}:\n"),
            OpCode::Goto { label_id } => format!("    jmp .L{label_id}\n"),
            OpCode::GotoIfZero {
                condition,
                label_id,
            } => {
                assert_eq!(*self.argument_data_type(condition), DataType::Bool);

                let rax = NasmRegister::Rax.register_text(&DataType::Bool);

                let condition_compiled = self.compile_nasm_argument(condition);

                format!("    mov {rax}, {condition_compiled}\n    test {rax}, {rax}\n    jz .L{label_id}\n")
            }
            OpCode::GotoIfNotZero {
                condition,
                label_id,
            } => {
                assert_eq!(*self.argument_data_type(condition), DataType::Bool);

                let condition_compiled = self.compile_nasm_argument(condition);

                let rax = NasmRegister::Rax.register_text(&DataType::Bool);

                format!("    mov {rax}, {condition_compiled}\n    test {rax}, {rax}\n    jnz .L{label_id}\n")
            }
            OpCode::Call {
                dst,
                lhs,
                arguments,
            } => {
                let lhs_compiled = self.compile_nasm_argument(lhs);

                // generate the code that pushes all the function arguments onto the stack
                let push_arguments_code = arguments
                    .iter()
                    .rev()
                    .map(|argument| {
                        let rax =
                            NasmRegister::Rax.register_text(self.argument_data_type(argument));

                        format!(
                            "    mov {rax}, {}\n    push rax\n",
                            self.compile_nasm_argument(argument)
                        )
                    })
                    .collect::<String>();

                // get the size of the section of the stack used for arguments
                let argument_stack_size = arguments
                    .iter()
                    .map(|argument| self.argument_data_type(argument).size_aligned())
                    .sum::<usize>();

                // generate the code for calling the function
                let call_code = if let Argument::Symbol { .. } = lhs {
                    format!("    call {lhs_compiled}\n")
                } else {
                    let rax = NasmRegister::Rax.register_text(self.argument_data_type(lhs));

                    format!("    mov {rax}, {lhs_compiled}\n    call {rax}\n")
                };

                let DataType::Function { return_type, .. } = self.argument_data_type(lhs) else {
                    unreachable!("This should be a function. If there was an error, it should have been caught in the typechecking phase.")
                };

                // for a void function, we don't need to store any "result variable"
                // TODO: when we can, use rax instead
                if **return_type == DataType::Void {
                    format!("{push_arguments_code}{call_code}    add rsp, {argument_stack_size}\n")
                } else {
                    let dst_compiled = self.compile_nasm_argument(dst);

                    format!("    push {dst_compiled}\n{push_arguments_code}{call_code}    add rsp, {argument_stack_size}\n    pop {dst_compiled}\n")
                }
            }
        };

        Some(data)
    }

    pub fn compile_nasm(&self) -> String {
        let code = self
            .opcodes
            .iter()
            .filter_map(|opcode| self.compile_nasm_opcode(opcode))
            .collect::<String>();

        format!(
            "{}:\n    enter {}, 0\n{code}    leave\n    ret\n",
            self.name,
            self.stack_size(),
        )
    }
}

#[derive(Debug, Default)]
pub struct ByteCode<'src> {
    symbols: Vec<(String, &'src [u8])>,
    functions: Vec<Function<'src>>,
}

impl<'src> ByteCode<'src> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_function(&mut self, function: Function<'src>) {
        self.functions.push(function);
    }

    pub fn add_symbol(&mut self, name: String, data: &'src [u8]) {
        self.symbols.push((name, data));
    }

    pub fn symbols_len(&self) -> usize {
        self.symbols.len()
    }

    pub fn compile_nasm(&self) -> String {
        format!(
            "\
[BITS 64]
global _start
section .text
print:
    enter 0, 0
    mov rax, 1
    mov rdi, 1
    mov rsi, [rbp + 24]
    mov rdx, [rbp + 16]
    syscall
    leave
    ret
_start:
    sub rsp, 8
    call @main
    mov rax, 60
    pop rdi
    syscall
{}section .data
{}",
            self.functions
                .iter()
                .map(|function| function.compile_nasm())
                .collect::<String>(),
            self.symbols
                .iter()
                .map(|(name, data)| {
                    format!(
                        "{name}: db {}\n",
                        data.iter()
                            .map(|byte| byte.to_string())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                })
                .collect::<String>()
        )
    }
}
