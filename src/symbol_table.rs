use crate::types::{DataType, IntType};
use std::collections::HashMap;

pub type VariableID<'src> = (&'src str, usize);

pub struct Variable {
    pub data_type: DataType,
}

pub struct SymbolTable<'src> {
    pub scope_id: usize,
    variables: HashMap<VariableID<'src>, Variable>,
    pub scopes: Vec<usize>,
}

impl<'src> SymbolTable<'src> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_variable(&mut self, name: &'src str, variable: Variable) {
        self.variables.insert((name, self.scope_id), variable);
    }

    pub fn get_variable(&self, name: &'src str) -> Option<&Variable> {
        let mut scope_id = self.scope_id;

        loop {
            if let Some(variable) = self.variables.get(&(name, scope_id)) {
                return Some(variable);
            }

            if scope_id == 0 {
                break;
            }

            scope_id = self.scopes[scope_id];
        }

        None
    }

    pub fn get_variable_id(&self, name: &'src str) -> Option<VariableID<'src>> {
        let mut scope_id = self.scope_id;

        loop {
            if self.variables.get(&(name, scope_id)).is_some() {
                return Some((name, scope_id));
            }

            if scope_id == 0 {
                break;
            }

            scope_id = self.scopes[scope_id];
        }

        None
    }

    pub fn add_scope(&mut self) -> usize {
        self.scopes.push(self.scope_id);

        self.scope_id = self.scopes.len() - 1;

        self.scope_id
    }

    pub fn enter_scope(&mut self, scope_id: usize) {
        assert!(scope_id < self.scopes.len());

        self.scope_id = scope_id;
    }

    pub fn leave_scope(&mut self) {
        self.scope_id = self.scopes[self.scope_id];
    }
}

impl<'src> Default for SymbolTable<'src> {
    fn default() -> Self {
        Self {
            scope_id: 0,
            variables: HashMap::from([
                (
                    ("print", 0),
                    Variable {
                        data_type: DataType::Function {
                            return_type: Box::new(DataType::Void),
                            argument_types: vec![DataType::Ref(Box::new(DataType::Int(IntType::U8))), DataType::Int(IntType::U64)]
                        }
                    }
                )
            ]),
            scopes: vec![0],
        }
    }
}
