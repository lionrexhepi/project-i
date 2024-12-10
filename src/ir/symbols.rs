use std::collections::HashMap;

use smol_str::SmolStr;

use super::types::{Type, TypeId, TypeMap};

pub struct SymbolTable {
    types: TypeMap,
    symbols: HashMap<SmolStr, Symbol>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        let mut symbols = HashMap::new();
        let mut types = TypeMap::default();
        let add2 = types.push(Type::Function {
            args: vec![TypeId::INT],
            ret: TypeId::INT,
        });
        symbols.insert("i32".into(), Symbol::Type(TypeId::INT));
        symbols.insert("bool".into(), Symbol::Type(TypeId::BOOL));
        symbols.insert("add2".into(), Symbol::Variable(add2));
        SymbolTable { symbols, types }
    }
}

impl SymbolTable {
    pub fn insert(&mut self, name: &str, symbol: Symbol) {
        self.symbols.insert(name.into(), symbol);
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }

    pub fn resolve_type(&self, id: TypeId) -> &Type {
        self.types.get(id).expect("Invalid type id")
    }
}

#[derive(Debug, PartialEq)]
pub enum Symbol {
    Variable(TypeId),
    Type(TypeId),
}
