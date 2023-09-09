use crate::types::{DataType, IntType};
use std::collections::HashMap;

#[derive(Hash, PartialEq, Eq)]
pub struct SymbolID<'src> {
    name: &'src str,
    scope_id: ScopeID,
}

pub type ScopeID = usize;

pub enum Symbol<'src> {
    Variable(DataType<'src>),
    Struct(DataType<'src>),
}

// Symbol Table:
//
// In order to find a symbol with a given name, the table searches through every scope starting with the first one going backwards.
// To achieve this, every scope has a mapping to the previous scope, going back to the first scope, which maps recursively to itself.
// If the table cannot find your symbol in the first scope, the global scope,
// then it cannot find your symbol in any scope, and throws a runtime error.
// Every symbol in the hashmap contains both its name, and the ID number of its scope.
pub struct SymbolTable<'src> {
    pub scope_id: ScopeID,
    symbols: HashMap<SymbolID<'src>, Symbol<'src>>,
    pub scopes: Vec<ScopeID>,
}

impl<'src> SymbolTable<'src> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_symbol(&mut self, name: &'src str, symbol: Symbol<'src>) {
        self.symbols.insert(
            SymbolID {
                name,
                scope_id: self.scope_id,
            },
            symbol,
        );
    }

    pub fn get_symbol(&self, name: &'src str) -> Option<&Symbol<'src>> {
        let mut scope_id = self.scope_id;

        loop {
            if let Some(symbol) = self.symbols.get(&SymbolID { name, scope_id }) {
                return Some(symbol);
            }

            if scope_id == 0 {
                break;
            }

            scope_id = self.scopes[scope_id];
        }

        None
    }

    pub fn get_symbol_id(&self, name: &'src str) -> Option<SymbolID<'src>> {
        let mut scope_id = self.scope_id;

        loop {
            let symbol_id = SymbolID { name, scope_id };
            if self.symbols.get(&symbol_id).is_some() {
                return Some(symbol_id);
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
            symbols: HashMap::new(),
            scopes: vec![0],
        };

        symbol_table.add_symbol(
            "read",
            Symbol::Variable(DataType::Function {
                return_type: Box::new(DataType::Void),
                argument_types: vec![
                    DataType::Ref(Box::new(DataType::Int(IntType::U8))),
                    DataType::Int(IntType::U64),
                ],
            }),
        );

        symbol_table.add_symbol(
            "print",
            Symbol::Variable(DataType::Function {
                return_type: Box::new(DataType::Void),
                argument_types: vec![
                    DataType::Ref(Box::new(DataType::Int(IntType::U8))),
                    DataType::Int(IntType::U64),
                ],
            }),
        );

        symbol_table.add_symbol(
            "malloc",
            Symbol::Variable(DataType::Function {
                return_type: Box::new(DataType::Ref(Box::new(DataType::Int(IntType::U8)))),
                argument_types: vec![DataType::Int(IntType::U64)],
            }),
        );

        symbol_table.add_symbol(
            "free",
            Symbol::Variable(DataType::Function {
                return_type: Box::new(DataType::Void),
                argument_types: vec![
                    DataType::Ref(Box::new(DataType::Int(IntType::U8))),
                    DataType::Int(IntType::U64),
                ],
            }),
        );

        symbol_table
    }
}
