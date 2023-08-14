use crate::{div_round_up, types::DataType};

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

#[derive(Debug, Clone)]
pub struct Register {
    pub data_type: DataType,
    pub stack_pos: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Argument {
    Constant {
        value: u64,
        data_type: DataType
    },
    Register(RegisterID),
    Symbol {
        name: String,
        data_type: DataType
    },
    VoidRegister,
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
    SetIfEqual {
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
        arguments: Vec<Argument>
    }
}

#[derive(Debug)]
pub struct Function<'src> {
    name: &'src str,
    label_id: LabelID,
    pub return_value: RegisterID,
    registers: Vec<Register>,
    opcodes: Vec<OpCode>,
}

impl<'src> Function<'src> {
    pub fn new(name: &'src str, return_type: DataType) -> Self {
        Self {
            name,
            label_id: 0,
            return_value: 0,
            registers: vec![Register {
                stack_pos: 0,
                data_type: return_type,
            }],
            opcodes: Vec::new(),
        }
    }

    pub fn stack_size(&self) -> usize {
        self.registers.iter().last().map_or(0, |register| {
            register.stack_pos + div_round_up(register.data_type.size(), 8) * 8
        })
    }

    pub fn add_register(&mut self, data_type: DataType) -> RegisterID {
        self.registers.push(Register {
            data_type,
            stack_pos: self.stack_size(),
        });

        self.registers.len() - 1
    }

    pub fn add_label(&mut self) -> LabelID {
        let prev_label_id = self.label_id;

        self.label_id += 1;

        prev_label_id
    }

    pub fn get_register(&self, id: RegisterID) -> &Register {
        &self.registers[id]
    }

    pub fn argument_data_type(&self, argument: &'src Argument) -> &DataType {
        match argument {
            Argument::Constant { data_type, .. }
            | Argument::Symbol { data_type, .. } => data_type,
            Argument::Register(register_id) => &self.registers[*register_id].data_type,
            Argument::VoidRegister => unreachable!(),
        }
    }

    pub fn add_opcode(&mut self, opcode: OpCode) {
        self.opcodes.push(opcode);
    }

    fn compile_nasm_argument_explicit(&self, argument: &Argument) -> String {
        match argument {
            Argument::Constant { value, .. } => value.to_string(),
            Argument::Register(register) => {
                let register = &self.registers[*register];
                format!(
                    "{} [rsp + {}]",
                    nasm_oper_text(&register.data_type),
                    register.stack_pos
                )
            }
            Argument::Symbol { name, .. } => name.clone(),
            Argument::VoidRegister => unreachable!(),
        }
    }

    fn compile_nasm_argument(&self, argument: &Argument) -> String {
        match argument {
            Argument::Constant { value, .. } => value.to_string(),
            Argument::Register(register) => format!("[rsp + {}]", &self.registers[*register].stack_pos),
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
                let dst_compiled = self.compile_nasm_argument_explicit(dst);

                if let Argument::Register(_) = src {
                    format!("    mov {rax}, {src_compiled}\n    mov {dst_compiled}, {rax}\n")
                } else {
                    format!("    mov {dst_compiled}, {src_compiled}\n")
                }
            }
            OpCode::Add { dst, src } => {
                let rax = NasmRegister::Rax.register_text(self.argument_data_type(dst));

                let src_compiled = self.compile_nasm_argument(src);
                let dst_compiled = self.compile_nasm_argument_explicit(dst);

                if let Argument::Register(_) = src {
                    format!("    mov {rax}, {src_compiled}\n    add {dst_compiled}, {rax}\n")
                } else {
                    format!("    add {dst_compiled}, {src_compiled}\n")
                }
            }
            OpCode::Sub { dst, src } => {
                let rax = NasmRegister::Rax.register_text(self.argument_data_type(dst));

                let src_compiled = self.compile_nasm_argument(src);
                let dst_compiled = self.compile_nasm_argument_explicit(dst);

                if let Argument::Register(_) = src {
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

                let src_compiled = self.compile_nasm_argument(src);
                let dst_compiled = self.compile_nasm_argument(dst);

                format!("    mov {rax}, {dst_compiled}\n    mov {rbx}, {src_compiled}\n    div {rbx}\n    mov {dst_compiled}, {rax}\n")
            }
            OpCode::Mod { dst, src } => {
                let rax = NasmRegister::Rax.register_text(self.argument_data_type(dst));
                let rbx = NasmRegister::Rbx.register_text(self.argument_data_type(dst));
                let rdx = NasmRegister::Rdx.register_text(self.argument_data_type(dst));

                let src_compiled = self.compile_nasm_argument(src);
                let dst_compiled = self.compile_nasm_argument(dst);

                format!("    mov {rax}, {dst_compiled}\n    mov {rbx}, {src_compiled}\n    div {rbx}\n    mov {dst_compiled}, {rdx}\n")
            }
            OpCode::SetIfEqual { dst, lhs, rhs } => {
                assert_eq!(*self.argument_data_type(dst), DataType::Bool);

                let rax = NasmRegister::Rax.register_text(self.argument_data_type(lhs));

                let lhs_compiled = self.compile_nasm_argument(lhs);
                let rhs_compiled = self.compile_nasm_argument(rhs);
                let dst_compiled = self.compile_nasm_argument_explicit(dst);

                format!("    mov {rax}, {lhs_compiled}\n    cmp {rax}, {rhs_compiled}\n    sete {dst_compiled}\n")
            }
            OpCode::SetIfGreater { dst, lhs, rhs } => {
                assert_eq!(*self.argument_data_type(dst), DataType::Bool);

                let rax = NasmRegister::Rax.register_text(self.argument_data_type(lhs));

                let lhs_compiled = self.compile_nasm_argument(lhs);
                let rhs_compiled = self.compile_nasm_argument(rhs);
                let dst_compiled = self.compile_nasm_argument_explicit(dst);

                format!("    mov {rax}, {lhs_compiled}\n    cmp {rax}, {rhs_compiled}\n    setg {dst_compiled}\n")
            }
            OpCode::SetIfLess { dst, lhs, rhs } => {
                assert_eq!(*self.argument_data_type(dst), DataType::Bool);

                let rax = NasmRegister::Rax.register_text(self.argument_data_type(lhs));

                let lhs_compiled = self.compile_nasm_argument(lhs);
                let rhs_compiled = self.compile_nasm_argument(rhs);
                let dst_compiled = self.compile_nasm_argument_explicit(dst);

                format!("    mov {rax}, {lhs_compiled}\n    cmp {rax}, {rhs_compiled}\n    setl {dst_compiled}\n")
            }
            OpCode::SetIfGreaterOrEqual { dst, lhs, rhs } => {
                assert_eq!(*self.argument_data_type(dst), DataType::Bool);

                let rax = NasmRegister::Rax.register_text(self.argument_data_type(lhs));

                let lhs_compiled = self.compile_nasm_argument(lhs);
                let rhs_compiled = self.compile_nasm_argument(rhs);
                let dst_compiled = self.compile_nasm_argument_explicit(dst);

                format!("    mov {rax}, {lhs_compiled}\n    cmp {rax}, {rhs_compiled}\n    setge {dst_compiled}\n")
            }
            OpCode::SetIfLessOrEqual { dst, lhs, rhs } => {
                assert_eq!(*self.argument_data_type(dst), DataType::Bool);

                let rax = NasmRegister::Rax.register_text(self.argument_data_type(lhs));

                let lhs_compiled = self.compile_nasm_argument(lhs);
                let rhs_compiled = self.compile_nasm_argument(rhs);
                let dst_compiled = self.compile_nasm_argument_explicit(dst);

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
            OpCode::Call { dst, lhs, arguments } => {
                let lhs_compiled = self.compile_nasm_argument_explicit(lhs);

                let push_arguments_code = arguments
                    .iter()
                    .rev()
                    .map(|argument| format!("    push {}\n", self.compile_nasm_argument_explicit(argument)))
                    .collect::<String>();

                let argument_stack_size = arguments
                    .iter()
                    .map(|argument| self.argument_data_type(argument).size())
                    .sum::<usize>();

                let call_code = if let Argument::Register(_) = lhs {
                    let rax = NasmRegister::Rax.register_text(self.argument_data_type(lhs));

                    format!("    mov {rax}, {lhs_compiled}\n    call {rax}\n")
                } else {
                    format!("    call {lhs_compiled}\n")
                };

                if let Argument::VoidRegister = dst {
                    format!("{push_arguments_code}{call_code}    add rsp, {argument_stack_size}\n")
                } else {
                    let dst_compiled = self.compile_nasm_argument_explicit(dst);

                    format!("{push_arguments_code}    push {dst_compiled}\n{call_code}    pop {dst_compiled}\n    add rsp, {argument_stack_size}\n")
                }
            }
        };

        Some(data)
    }

    pub fn compile_nasm(&self) -> String {
        let code = self.opcodes
            .iter()
            .filter_map(|opcode| self.compile_nasm_opcode(opcode))
            .collect::<String>();

        let rax = NasmRegister::Rax.register_text(&self.get_register(self.return_value).data_type);

        let return_value_compiled = self.compile_nasm_argument(&Argument::Register(self.return_value));

        format!(
            "{name}:\n    enter {size}, 0\n{code}    mov {rax}, {return_value_compiled}\n    leave\n    ret\n",
            size = self.stack_size(),
            name = self.name,
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
    mov rsi, [rbp + 16]
    mov rdx, [rbp + 24]
    syscall
    leave
    ret
_start:
    call main
    mov rdi, rax
    mov rax, 60
    syscall
{}section .data
{}",
            self.functions.iter()
                .map(|function| function.compile_nasm())
                .collect::<String>(),
            self.symbols.iter()
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
