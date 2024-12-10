use std::collections::HashMap;

use smol_str::SmolStr;

use super::types::{Type, TypeId, TypeMap};

pub struct SymbolTable {
    types: TypeMap,
    scopes: Vec<Scope>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        SymbolTable {
            scopes: vec![Scope::global()],
            types: TypeMap::default(),
        }
    }
}

impl SymbolTable {
    pub fn insert(&mut self, name: &str, symbol: Symbol) {
        self.scopes
            .last_mut()
            .expect("Must not pop global scope")
            .insert(name, symbol);
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    pub fn resolve_type(&self, id: TypeId) -> &Type {
        self.types.get(id).expect("Invalid type id")
    }

    pub fn create_temporary(&mut self, ty: TypeId) -> TempId {
        self.scopes
            .last_mut()
            .expect("Must not pop global scope")
            .create_temporary(ty)
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope {
            symbols: HashMap::new(),
            temporaries: vec![],
        });
    }

    /// Returns the temporaries created in the popped scope.
    pub fn pop_scope(&mut self) -> impl Iterator<Item = (TempId, TypeId)> {
        self.scopes
            .pop()
            .expect("Must not pop global scope")
            .take_temporaries()
    }
}

struct Scope {
    symbols: HashMap<SmolStr, Symbol>,
    temporaries: Vec<TypeId>,
}

impl Scope {
    fn global() -> Self {
        let mut symbols = HashMap::new();
        let mut types = TypeMap::default();
        let add2 = types.push(Type::Function {
            args: vec![TypeId::INT],
            ret: TypeId::INT,
        });
        symbols.insert("i32".into(), Symbol::Type(TypeId::INT));
        symbols.insert("bool".into(), Symbol::Type(TypeId::BOOL));
        symbols.insert("add2".into(), Symbol::Variable(add2));
        Scope {
            symbols,
            temporaries: vec![],
        }
    }

    fn insert(&mut self, name: &str, symbol: Symbol) {
        self.symbols.insert(name.into(), symbol);
    }

    fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }

    fn create_temporary(&mut self, ty: TypeId) -> TempId {
        let id = TempId(self.temporaries.len());
        self.temporaries.push(ty);
        id
    }

    fn take_temporaries(self) -> impl Iterator<Item = (TempId, TypeId)> {
        self.temporaries
            .into_iter()
            .enumerate()
            .map(|(i, ty)| (TempId(i), ty))
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct TempId(usize);

impl TempId {
    pub fn get_name(self) -> SmolStr {
        format!("t{}", self.0).into()
    }
}

#[derive(Debug, PartialEq)]
pub enum Symbol {
    Variable(TypeId),
    Type(TypeId),
}
