use crate::types::{DataType, IntType};
use std::collections::HashMap;

#[derive(Hash, PartialEq, Eq)]
pub struct VariableID<'src> {
    name: &'src str,
    scope_id: ScopeID
}

pub type ScopeID = usize;

pub struct Variable {
    pub data_type: DataType,
}

pub struct SymbolTable<'src> {
    pub scope_id: ScopeID,
    variables: HashMap<VariableID<'src>, Variable>,
    pub scopes: Vec<ScopeID>,
}

impl<'src> SymbolTable<'src> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_variable(&mut self, name: &'src str, variable: Variable) {
        self.variables.insert(VariableID { name, scope_id: self.scope_id }, variable);
    }

    pub fn get_variable(&self, name: &'src str) -> Option<&Variable> {
        let mut scope_id = self.scope_id;

        loop {
            if let Some(variable) = self.variables.get(&VariableID { name, scope_id }) {
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
            let variable_id = VariableID { name, scope_id };
            if self.variables.get(&variable_id).is_some() {
                return Some(variable_id);
            }

            if scope_id == 0 {
                break;
            }

            scope_id = self.scopes[scope_id];
        }

        None
    }

    pub fn add_scope(&mut self) -> ScopeID {
        self.scopes.push(self.scope_id);

        self.scope_id = self.scopes.len() - 1;

        self.scope_id
    }

    pub fn get_scope(&self) -> ScopeID {
        self.scope_id
    }

    pub fn enter_scope(&mut self, scope_id: ScopeID) {
        assert!(scope_id < self.scopes.len());

        self.scope_id = scope_id;
    }

    pub fn leave_scope(&mut self) {
        self.scope_id = self.scopes[self.scope_id];
    }
}

impl<'src> Default for SymbolTable<'src> {
    fn default() -> Self {
        let mut symbol_table = Self {
            scope_id: 0,
            variables: HashMap::new(),
            scopes: vec![0],
        };

        symbol_table.add_variable(
            "print",
            Variable {
                data_type: DataType::Function {
                    return_type: Box::new(DataType::Void),
                    argument_types: vec![DataType::Ref(Box::new(DataType::Int(IntType::U8))), DataType::Int(IntType::U64)]
                }
            }
        );

        symbol_table
    }
}
