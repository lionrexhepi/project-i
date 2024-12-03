use std::collections::HashMap;

use smol_str::SmolStr;

pub struct SymbolTable {
    symbols: HashMap<SmolStr, Symbol>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        let mut symbols = HashMap::new();
        symbols.insert("int".into(), Symbol::Type(Type::Int));
        symbols.insert("bool".into(), Symbol::Type(Type::Bool));
        symbols.insert("fn".into(), Symbol::Type(Type::Function));
        SymbolTable { symbols }
    }
}

impl SymbolTable {
    pub fn insert(&mut self, name: &str, symbol: Symbol) {
        self.symbols.insert(name.into(), symbol);
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }
}

#[derive(Debug, PartialEq)]
pub enum Symbol {
    Variable(Type),
    Type(Type),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Type {
    Int,
    Bool,
    Function,
}
