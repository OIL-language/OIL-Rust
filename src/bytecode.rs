use crate::types::DataType;
use std::fmt;

pub type RegisterID = usize;
pub type ArgumentID = usize;
pub type LabelID = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Argument<'src> {
    ReturnValue,
    Register(RegisterID),
    Argument(ArgumentID),
    Deref(Box<Self>),
    StructField {
        data: Box<Self>,
        name: &'src str,
    },
    Constant {
        value: u64,
        data_type: DataType<'src>,
    },
    Symbol {
        name: String,
        data_type: DataType<'src>,
    },
    VoidRegister,
}

#[derive(Debug, Clone)]
pub enum OpCode<'src> {
    Mov {
        dst: Argument<'src>,
        src: Argument<'src>,
    },
    Add {
        dst: Argument<'src>,
        src: Argument<'src>,
    },
    Sub {
        dst: Argument<'src>,
        src: Argument<'src>,
    },
    Mul {
        dst: Argument<'src>,
        src: Argument<'src>,
    },
    Div {
        dst: Argument<'src>,
        src: Argument<'src>,
    },
    Mod {
        dst: Argument<'src>,
        src: Argument<'src>,
    },
    Not {
        dst: Argument<'src>,
    },
    Ref {
        dst: Argument<'src>,
        src: Argument<'src>,
    },
    Index {
        dst: Argument<'src>,
        src: Argument<'src>,
        index: Argument<'src>,
    },
    SetIndex {
        dst: Argument<'src>,
        src: Argument<'src>,
        index: Argument<'src>,
    },
    SetField {
        dst: Argument<'src>,
        src: Argument<'src>,
        offset: usize,
    },
    SetIfEqual {
        dst: Argument<'src>,
        lhs: Argument<'src>,
        rhs: Argument<'src>,
    },
    SetIfNotEqual {
        dst: Argument<'src>,
        lhs: Argument<'src>,
        rhs: Argument<'src>,
    },
    SetIfGreater {
        dst: Argument<'src>,
        lhs: Argument<'src>,
        rhs: Argument<'src>,
    },
    SetIfLess {
        dst: Argument<'src>,
        lhs: Argument<'src>,
        rhs: Argument<'src>,
    },
    SetIfGreaterOrEqual {
        dst: Argument<'src>,
        lhs: Argument<'src>,
        rhs: Argument<'src>,
    },
    SetIfLessOrEqual {
        dst: Argument<'src>,
        lhs: Argument<'src>,
        rhs: Argument<'src>,
    },
    Negate {
        dst: Argument<'src>,
    },
    Label {
        label_id: LabelID,
    },
    Goto {
        label_id: LabelID,
    },
    GotoIfZero {
        condition: Argument<'src>,
        label_id: LabelID,
    },
    GotoIfNotZero {
        condition: Argument<'src>,
        label_id: LabelID,
    },
    Call {
        dst: Argument<'src>,
        lhs: Argument<'src>,
        arguments: Vec<Argument<'src>>,
    },
}

#[derive(Debug)]
pub struct Function<'src> {
    pub name: &'src str,
    pub labels: LabelID,
    pub return_type: DataType<'src>,
    pub argument_types: Vec<DataType<'src>>,
    pub arguments_size: usize,
    pub register_types: Vec<DataType<'src>>,
    pub registers_size: usize,
    pub opcodes: Vec<OpCode<'src>>,
}

impl<'src> Function<'src> {
    pub fn new(
        name: &'src str,
        return_type: DataType<'src>,
        argument_types: Vec<DataType<'src>>,
    ) -> Self {
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

    pub fn add_register(&mut self, data_type: DataType<'src>) -> RegisterID {
        self.registers_size += data_type.size_aligned();

        self.register_types.push(data_type);

        self.register_types.len() - 1
    }

    pub fn add_label(&mut self) -> LabelID {
        let prev_labels = self.labels;

        self.labels += 1;

        prev_labels
    }

    pub fn add_opcode(&mut self, opcode: OpCode<'src>) {
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

    pub fn argument_data_type(&self, argument: &'src Argument<'src>) -> &DataType<'src> {
        match argument {
            Argument::ReturnValue => &self.return_type,
            Argument::Register(register_id) => &self.register_types[*register_id],
            Argument::Argument(argument_id) => &self.argument_types[*argument_id],
            Argument::Deref(deref) => {
                let DataType::Ref(ref deref) = self.argument_data_type(deref) else { panic!() };

                deref
            }
            Argument::StructField { data, name } => {
                let DataType::Struct(ref fields) = self.argument_data_type(data) else { panic!() };

                fields
                    .iter()
                    .find(|(find_name, _)| find_name == name)
                    .map_or_else(|| panic!(), |(_, data_type)| data_type)
            }
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

    pub fn string_symbol_name(id: usize) -> String {
        format!("str_{id}")
    }
}

pub trait CodeGenerator<'src> {
    fn generate(bytecode: &ByteCode<'src>) -> Result<String, fmt::Error>;
}
