use crate::{
    bytecode::{Argument, ByteCode, CodeGenerator, Function, OpCode},
    types::DataType,
};
use std::fmt::{self, Write};

pub fn data_type_generate(data_type: &DataType) -> &'static str {
    match data_type.size() {
        1 => "byte",
        2 => "word",
        4 => "dword",
        8 => "qword",
        _ => unreachable!(),
    }
}

pub fn is_argument_comparable(argument: &Argument) -> bool {
    !matches!(
        argument,
        Argument::Register(_) | Argument::Argument(_) | Argument::StructField { .. }
    )
}

#[derive(PartialEq, Eq, Copy, Clone)]
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
    fn generate(&self, data_type: &DataType) -> &str {
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

const READ_CODE: &str = "\
read:
    enter 0, 0
    mov rax, 0x0        ; syscall read
    mov rdi, 0x0        ; stdin
    mov rsi, [rbp + 24] ; text
    mov rdx, [rbp + 16] ; length
    syscall
    leave
    ret
";

const PRINT_CODE: &str = "\
print:
    enter 0, 0
    mov rax, 0x1        ; syscall write
    mov rdi, 0x1        ; stdout
    mov rsi, [rbp + 24] ; text
    mov rdx, [rbp + 16] ; length
    syscall
    leave
    ret
";

const MALLOC_CODE: &str = "\
malloc:
    enter 0, 0
    mov rax, 0x9        ; syscall mmap
    mov rdi, 0x0        ; addr chosed by kernel
    mov rsi, [rbp + 16] ; length
    mov rdx, 0x3        ; PROT_READ | PROT_WRITE
    mov r10, 0x22       ; MAP_ANONYMOUS | MAP_PRIVATE
    mov r8, -1          ; no file descriptor
    mov r9, 0           ; no offset
    syscall
    cmp rax, -1    ; MAP_FAILED
    mov rbx, 0x0   ; null
    mov qword [rbp + 24], rax
    leave
    ret
";

const FREE_CODE: &str = "\
free:
    enter 0, 0
    mov rax, 0xb        ; syscall munmap
    mov rdi, [rbp + 24] ; addr
    mov rsi, [rbp + 16] ; length
    syscall
    leave
    ret
";

const ENTRY_CODE: &str = "\
_start:
    sub rsp, 0x8
    call @main
    mov rax, 0x3c ; syscall exit
    pop rdi       ; exit code
    syscall
";

pub struct Nasm {
    text: String,
}

impl Nasm {
    fn generate_argument(
        &mut self,
        function: &Function,
        argument: &Argument,
        scratch: NasmRegister,
        lvalue: bool,
    ) -> Result<String, fmt::Error> {
        let mut can_rvalue = true;

        let text = match argument {
            Argument::ReturnValue => {
                format!(
                    "rbp + {}",
                    8 + function.arguments_size + function.return_type.size_aligned()
                )
            }
            Argument::Register(register_id) => {
                format!("rbp - {}", 8 + function.register_position(*register_id))
            }
            Argument::Argument(argument_id) => {
                format!(
                    "rbp + {}",
                    8 + function.arguments_size - function.argument_position(*argument_id)
                )
            }
            Argument::Deref(deref) => {
                let deref_compiled = self.generate_argument(function, deref, scratch, true)?;
                let scratch_compiled = scratch.generate(function.argument_data_type(deref));

                writeln!(self.text, "    mov {scratch_compiled}, [{deref_compiled}]")?;

                scratch_compiled.to_owned()
            }
            Argument::StructField { ref data, name } => {
                let DataType::Struct(ref fields) = function.argument_data_type(data) else { panic!(); };

                let offset = fields
                    .iter()
                    .take_while(|(find_name, _)| find_name != name)
                    .map(|(_, data_type)| data_type.size_aligned())
                    .sum::<usize>();

                let data_compiled = self.generate_argument(function, data, scratch, true)?;

                format!("{data_compiled} - {offset}")
            }
            Argument::Constant { value, .. } => {
                can_rvalue = false;
                value.to_string()
            }
            Argument::Symbol { name, .. } => {
                can_rvalue = false;
                name.clone()
            }
            _ => unreachable!(),
        };

        if !lvalue && can_rvalue {
            Ok(format!(
                "{} [{text}]",
                data_type_generate(function.argument_data_type(argument))
            ))
        } else {
            Ok(text)
        }
    }

    fn generate_push_argument(&mut self, function: &Function, argument: &Argument) -> fmt::Result {
        if let DataType::Struct(ref fields) = function.argument_data_type(argument) {
            for (name, _) in fields.iter().rev() {
                self.generate_push_argument(
                    function,
                    &Argument::StructField {
                        data: Box::new(argument.clone()),
                        name,
                    },
                )?;
            }
        } else {
            let rax = NasmRegister::Rax.generate(function.argument_data_type(argument));

            let argument_compiled =
                self.generate_argument(function, argument, NasmRegister::Rbx, false)?;

            writeln!(
                self.text,
                "    mov {rax}, {argument_compiled}\n    push rax"
            )?;
        }

        Ok(())
    }

    fn generate_pop_argument(&mut self, function: &Function, argument: &Argument) -> fmt::Result {
        if let DataType::Struct(ref fields) = function.argument_data_type(argument) {
            for (name, _) in fields.iter().rev() {
                self.generate_pop_argument(
                    function,
                    &Argument::StructField {
                        data: Box::new(argument.clone()),
                        name,
                    },
                )?;
            }
        } else {
            let rax = NasmRegister::Rax.generate(function.argument_data_type(argument));

            let argument_compiled =
                self.generate_argument(function, argument, NasmRegister::Rbx, false)?;

            writeln!(self.text, "    pop rax\n    mov {argument_compiled}, {rax}")?;
        }

        Ok(())
    }

    fn generate_infix(
        &mut self,
        function: &Function,
        dst: &Argument,
        src: &Argument,
        operation: &str,
    ) -> fmt::Result {
        if let DataType::Struct(ref fields) = function.argument_data_type(src) {
            let DataType::Struct(_) = function.argument_data_type(dst) else { unreachable!(); };

            for (name, _) in fields {
                self.generate_infix(
                    function,
                    &Argument::StructField {
                        data: Box::new(dst.clone()),
                        name,
                    },
                    &Argument::StructField {
                        data: Box::new(src.clone()),
                        name,
                    },
                    operation,
                )?;
            }
        } else {
            let rax = NasmRegister::Rax.generate(function.argument_data_type(dst));

            let src_compiled = self.generate_argument(function, src, NasmRegister::Rbx, false)?;
            let dst_compiled = self.generate_argument(function, dst, NasmRegister::Rcx, false)?;

            if is_argument_comparable(src) && is_argument_comparable(dst) {
                writeln!(self.text, "    {operation} {dst_compiled}, {src_compiled}")?;
            } else {
                writeln!(
                    self.text,
                    "    mov {rax}, {src_compiled}\n    {operation} {dst_compiled}, {rax}"
                )?;
            }
        }

        Ok(())
    }

    fn generate_comparison(
        &mut self,
        function: &Function,
        dst: &Argument,
        lhs: &Argument,
        rhs: &Argument,
        operation: &str,
    ) -> fmt::Result {
        assert_eq!(*function.argument_data_type(dst), DataType::Bool);

        let rax = NasmRegister::Rax.generate(function.argument_data_type(lhs));

        let lhs_compiled = self.generate_argument(function, lhs, NasmRegister::Rbx, false)?;
        let rhs_compiled = self.generate_argument(function, rhs, NasmRegister::Rcx, false)?;
        let dst_compiled = self.generate_argument(function, dst, NasmRegister::Rdi, false)?;

        writeln!(self.text, "    mov {rax}, {lhs_compiled}\n    cmp {rax}, {rhs_compiled}\n    {operation} {dst_compiled}")?;

        Ok(())
    }

    fn generate_opcode(&mut self, function: &Function, opcode: &OpCode) -> fmt::Result {
        match opcode {
            OpCode::Mov { dst, src } => {
                if dst != src {
                    self.generate_infix(function, dst, src, "mov")?
                }
            }
            OpCode::Add { dst, src } => self.generate_infix(function, dst, src, "add")?,
            OpCode::Sub { dst, src } => self.generate_infix(function, dst, src, "sub")?,
            OpCode::Mul { dst, src } => {
                let rax = NasmRegister::Rax.generate(function.argument_data_type(dst));
                let rbx = NasmRegister::Rbx.generate(function.argument_data_type(dst));

                let src_compiled =
                    self.generate_argument(function, src, NasmRegister::Rcx, false)?;
                let dst_compiled =
                    self.generate_argument(function, dst, NasmRegister::Rdi, false)?;

                writeln!(self.text, "    mov {rax}, {dst_compiled}\n    mov {rbx}, {src_compiled}\n    mul {rbx}\n    mov {dst_compiled}, {rax}")?;
            }
            OpCode::Div { dst, src } => {
                let rax = NasmRegister::Rax.generate(function.argument_data_type(dst));
                let rbx = NasmRegister::Rbx.generate(function.argument_data_type(dst));
                let rdx = NasmRegister::Rdx.generate(function.argument_data_type(dst));

                let src_compiled =
                    self.generate_argument(function, src, NasmRegister::Rcx, false)?;
                let dst_compiled =
                    self.generate_argument(function, dst, NasmRegister::Rdi, false)?;

                writeln!(self.text, "    xor {rdx}, {rdx}\n    mov {rax}, {dst_compiled}\n    mov {rbx}, {src_compiled}\n    div {rbx}\n    mov {dst_compiled}, {rax}")?;
            }
            OpCode::Mod { dst, src } => {
                let rax = NasmRegister::Rax.generate(function.argument_data_type(dst));
                let rbx = NasmRegister::Rbx.generate(function.argument_data_type(dst));
                let rdx = NasmRegister::Rdx.generate(function.argument_data_type(dst));

                let src_compiled =
                    self.generate_argument(function, src, NasmRegister::Rcx, false)?;
                let dst_compiled =
                    self.generate_argument(function, dst, NasmRegister::Rdi, false)?;

                writeln!(self.text, "    xor {rdx}, {rdx}\n    mov {rax}, {dst_compiled}\n    mov {rbx}, {src_compiled}\n    div {rbx}\n    mov {dst_compiled}, {rdx}")?;
            }
            OpCode::Not { dst } => {
                let dst_compiled =
                    self.generate_argument(function, dst, NasmRegister::Rax, false)?;

                writeln!(
                    self.text,
                    "    and {dst_compiled}, 0x1\n    xor {dst_compiled}, 0x1"
                )?;
            }
            OpCode::Ref { dst, src } => {
                let rax = NasmRegister::Rax.generate(function.argument_data_type(dst));

                let src_compiled =
                    self.generate_argument(function, src, NasmRegister::Rbx, true)?;
                let dst_compiled =
                    self.generate_argument(function, dst, NasmRegister::Rcx, false)?;

                writeln!(
                    self.text,
                    "    lea {rax}, [{src_compiled}]\n    mov {dst_compiled}, {rax}"
                )?;
            }
            OpCode::Index { dst, src, index } => {
                let rax = NasmRegister::Rax.generate(function.argument_data_type(src));
                let rbx = NasmRegister::Rbx.generate(function.argument_data_type(dst));

                let dst_compiled =
                    self.generate_argument(function, dst, NasmRegister::Rdi, false)?;
                let src_compiled =
                    self.generate_argument(function, src, NasmRegister::Rsi, false)?;

                writeln!(self.text, "    mov {rax}, {src_compiled}")?;

                if *index == Argument::VoidRegister {
                    writeln!(self.text, "    mov {rbx}, [{rax}]")?;
                } else {
                    let rcx = NasmRegister::Rcx.generate(function.argument_data_type(index));

                    let index_compiled =
                        self.generate_argument(function, index, NasmRegister::R8, false)?;

                    writeln!(
                        self.text,
                        "    mov {rcx}, {index_compiled}\n    mov {rbx}, [{rax} + {rcx}]"
                    )?;
                }

                writeln!(self.text, "    mov {dst_compiled}, {rbx}")?;
            }
            OpCode::SetIndex { dst, src, index } => {
                let rax = NasmRegister::Rax.generate(function.argument_data_type(dst));
                let rbx = NasmRegister::Rbx.generate(function.argument_data_type(src));

                let dst_compiled =
                    self.generate_argument(function, dst, NasmRegister::Rdi, false)?;
                let src_compiled =
                    self.generate_argument(function, src, NasmRegister::Rsi, false)?;

                writeln!(self.text, "    mov {rax}, {dst_compiled}")?;
                writeln!(self.text, "    mov {rbx}, {src_compiled}")?;

                if *index == Argument::VoidRegister {
                    writeln!(self.text, "    mov [{rax}], {rbx}")?;
                } else {
                    let rcx = NasmRegister::Rcx.generate(function.argument_data_type(index));

                    let index_compiled =
                        self.generate_argument(function, index, NasmRegister::R8, false)?;

                    writeln!(
                        self.text,
                        "    mov {rcx}, {index_compiled}\n    mov [{rax} + {rcx}], {rbx}"
                    )?;
                }
            }
            OpCode::SetField { dst, src, offset } => {
                let rax = NasmRegister::Rax.generate(function.argument_data_type(dst));
                let rbx = NasmRegister::Rbx.generate(function.argument_data_type(src));

                let dst_compiled =
                    self.generate_argument(function, dst, NasmRegister::Rcx, false)?;
                let src_compiled =
                    self.generate_argument(function, src, NasmRegister::Rdi, false)?;

                writeln!(
                    self.text,
                    "    mov {rax}, {dst_compiled}\n    mov {rbx}, {src_compiled}\n    mov [{rax} - {offset}], {rbx}"
                )?;
            }
            OpCode::SetIfEqual { dst, lhs, rhs } => {
                self.generate_comparison(function, dst, lhs, rhs, "sete")?
            }
            OpCode::SetIfNotEqual { dst, lhs, rhs } => {
                self.generate_comparison(function, dst, lhs, rhs, "setne")?
            }
            OpCode::SetIfGreater { dst, lhs, rhs } => {
                self.generate_comparison(function, dst, lhs, rhs, "setg")?
            }
            OpCode::SetIfLess { dst, lhs, rhs } => {
                self.generate_comparison(function, dst, lhs, rhs, "setl")?
            }
            OpCode::SetIfGreaterOrEqual { dst, lhs, rhs } => {
                self.generate_comparison(function, dst, lhs, rhs, "setge")?
            }
            OpCode::SetIfLessOrEqual { dst, lhs, rhs } => {
                self.generate_comparison(function, dst, lhs, rhs, "setle")?
            }
            OpCode::Negate { dst } => {
                let dst_compiled =
                    self.generate_argument(function, dst, NasmRegister::Rax, false)?;

                writeln!(self.text, "neg {dst_compiled}")?;
            }
            OpCode::Label { label_id } => writeln!(self.text, ".L{label_id}:")?,
            OpCode::Goto { label_id } => writeln!(self.text, "    jmp .L{label_id}")?,
            OpCode::GotoIfZero {
                condition,
                label_id,
            } => {
                let rax = NasmRegister::Rax.generate(function.argument_data_type(condition));

                let condition_compiled =
                    self.generate_argument(function, condition, NasmRegister::Rbx, false)?;

                writeln!(self.text, "    mov {rax}, {condition_compiled}\n    test {rax}, {rax}\n    jz .L{label_id}")?;
            }
            OpCode::GotoIfNotZero {
                condition,
                label_id,
            } => {
                let rax = NasmRegister::Rax.generate(function.argument_data_type(condition));

                let condition_compiled =
                    self.generate_argument(function, condition, NasmRegister::Rbx, false)?;

                writeln!(self.text, "    mov {rax}, {condition_compiled}\n    test {rax}, {rax}\n    jnz .L{label_id}\n")?;
            }
            OpCode::Call {
                dst,
                lhs,
                arguments,
            } => {
                let DataType::Function { return_type, .. } = function.argument_data_type(lhs) else {
                    unreachable!("This should be a function. If there was an error, it should have been caught in the typechecking phase.")
                };

                let lhs_compiled =
                    self.generate_argument(function, lhs, NasmRegister::Rbx, false)?;

                if **return_type != DataType::Void {
                    self.generate_push_argument(function, dst)?;
                }

                for argument in arguments.iter().rev() {
                    self.generate_push_argument(function, argument)?;
                }

                if let Argument::Symbol { .. } = lhs {
                    writeln!(self.text, "    call {lhs_compiled}")?;
                } else {
                    let rax = NasmRegister::Rax.generate(function.argument_data_type(lhs));

                    writeln!(self.text, "    mov {rax}, {lhs_compiled}\n    call {rax}")?;
                };

                let argument_stack_size = arguments
                    .iter()
                    .map(|argument| function.argument_data_type(argument).size_aligned())
                    .sum::<usize>();

                writeln!(self.text, "    add rsp, {argument_stack_size}")?;

                if **return_type != DataType::Void {
                    self.generate_pop_argument(function, dst)?;
                }
            }
        };

        Ok(())
    }
}

impl<'src> CodeGenerator<'src> for Nasm {
    fn generate(bytecode: &ByteCode<'src>) -> Result<String, fmt::Error> {
        let mut nasm = Self {
            text: format!("[BITS 64]\nglobal _start\nsection .text\n{MALLOC_CODE}{FREE_CODE}{READ_CODE}{PRINT_CODE}{ENTRY_CODE}"),
        };

        for function in &bytecode.functions {
            writeln!(
                nasm.text,
                "{}:\n    enter {}, 0",
                function.name,
                function.stack_size()
            )?;

            for opcode in &function.opcodes {
                nasm.generate_opcode(function, opcode)?;
            }

            writeln!(nasm.text, "    leave\n    ret")?;
        }

        writeln!(nasm.text, "section .data")?;

        for (id, string) in bytecode.strings.iter().enumerate() {
            let bytes = string
                .bytes()
                .chain(std::iter::once(0))
                .map(|byte| byte.to_string())
                .collect::<Vec<String>>()
                .join(", ");

            writeln!(
                nasm.text,
                "{}: db {bytes}",
                ByteCode::string_symbol_name(id)
            )?;
        }

        Ok(nasm.text)
    }
}
