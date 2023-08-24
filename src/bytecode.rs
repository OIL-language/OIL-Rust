use crate::types::DataType;
use std::fmt;

pub type RegisterID = usize;
pub type LabelID = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Argument {
    ReturnValue,
    Register(RegisterID),
    Argument(RegisterID),
    Constant { value: u64, data_type: DataType },
    Symbol { name: String, data_type: DataType },
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
    Not {
        dst: Argument,
    },
    Ref {
        dst: Argument,
        src: Argument,
    },
    Index {
        dst: Argument,
        src: Argument,
        index: Argument
    },
    SetIndex {
        dst: Argument,
        src: Argument,
        index: Argument
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
    }
}

#[derive(Debug)]
pub struct Function<'src> {
    pub name: &'src str,
    pub labels: LabelID,
    pub return_type: DataType,
    pub argument_types: Vec<DataType>,
    pub arguments_size: usize,
    pub register_types: Vec<DataType>,
    pub registers_size: usize,
    pub opcodes: Vec<OpCode>,
}

impl<'src> Function<'src> {
    pub fn new(name: &'src str, return_type: DataType, argument_types: Vec<DataType>) -> Self {
        let arguments_size = argument_types
            .iter()
            .map(|data_type| data_type.size_aligned())
            .sum();

        Self {
            name,
            labels: 0,
            return_type,
            argument_types,
            arguments_size,
            register_types: Vec::new(),
            registers_size: 0,
            opcodes: Vec::new(),
        }
    }

    pub fn stack_size(&self) -> usize {
        self.arguments_size + self.registers_size
    }

    pub fn add_register(&mut self, data_type: DataType) -> RegisterID {
        self.registers_size += data_type.size_aligned();

        self.register_types.push(data_type);

        self.register_types.len() - 1
    }

    pub fn add_label(&mut self) -> LabelID {
        let prev_labels = self.labels;

        self.labels += 1;

        prev_labels
    }

    pub fn add_opcode(&mut self, opcode: OpCode) {
        self.opcodes.push(opcode);
    }

    pub fn register_position(&self, register_id: RegisterID) -> usize {
        self.register_types
            .iter()
            .take(register_id)
            .map(|data_type| data_type.size_aligned())
            .sum()
    }

    pub fn argument_position(&self, argument_id: RegisterID) -> usize {
        self.argument_types
            .iter()
            .take(argument_id)
            .map(|data_type| data_type.size_aligned())
            .sum::<usize>()
    }

    pub fn argument_data_type(&self, argument: &'src Argument) -> &DataType {
        match argument {
            Argument::ReturnValue => &self.return_type,
            Argument::Register(register_id) => &self.register_types[*register_id],
            Argument::Argument(argument_id) => &self.argument_types[*argument_id],
            Argument::Constant { data_type, .. } | Argument::Symbol { data_type, .. } => data_type,
            Argument::VoidRegister => unreachable!(),
        }
    }
}

#[derive(Debug, Default)]
pub struct ByteCode<'src> {
    pub strings: Vec<&'src str>,
    pub functions: Vec<Function<'src>>,
}

impl<'src> ByteCode<'src> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_function(&mut self, function: Function<'src>) {
        self.functions.push(function);
    }

    pub fn add_string(&mut self, string: &'src str) -> usize {
        self.strings.push(string);
        self.strings.len() - 1
    }

    pub fn string_symbol_name(id: usize) -> String { format!("str_{id}") }
}

pub trait CodeGenerator<'src> {
    fn generate(bytecode: &ByteCode<'src>) -> Result<String, fmt::Error>;
}
