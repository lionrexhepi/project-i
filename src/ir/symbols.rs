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
        symbols.insert("function".into(), Symbol::Type(Type::Function));
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

impl Type {
    pub fn name(&self) -> &str {
        match self {
            Type::Int => "int",
            Type::Bool => "bool",
            Type::Function => "###FUNCTION###", // To make GCC error if this gets into the generated C code
        }
    }
}
