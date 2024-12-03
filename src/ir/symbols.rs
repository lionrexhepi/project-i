use std::collections::HashMap;

use smol_str::SmolStr;

#[derive(Default)]
pub struct SymbolTable {
    symbols: HashMap<SmolStr, Symbol>,
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
pub struct Symbol;
