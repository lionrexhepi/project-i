use std::collections::HashMap;

use crate::ast::Identifier;

use super::types::{Signature, Type, TypeId, TypeMap};

#[derive(Debug)]
pub struct SymbolTable {
    types: TypeMap,
    scope_arena: Vec<Scope>,
    current_scope: ScopeIndex,
}

impl Default for SymbolTable {
    fn default() -> Self {
        SymbolTable {
            scope_arena: vec![Scope::global()],
            types: TypeMap::default(),
            current_scope: 0,
        }
    }
}

impl SymbolTable {
    pub fn insert(&mut self, name: &str, symbol: Symbol) {
        self.scope_arena
            .get_mut(self.current_scope)
            .expect("Must not pop global scope")
            .insert(name, symbol);
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        let mut scope = self.current_scope;
        loop {
            if let Some(symbol) = self.scope_arena[scope].get(name) {
                return Some(symbol);
            }
            if let Some(parent) = self.scope_arena[scope].parent {
                scope = parent;
            } else {
                break;
            }
        }
        None
    }

    pub fn resolve_type(&self, id: TypeId) -> &Type {
        self.types.get(id).expect("Invalid type id")
    }

    pub fn function_pointer(&mut self, args: Vec<TypeId>, ret: TypeId) -> TypeId {
        self.types.function_pointer(args, ret)
    }

    #[track_caller]
    pub fn push_scope(&mut self) {
        let child = self.scope_arena.len();
        self.scope_arena[self.current_scope].append_child(child);
        self.scope_arena.push(Scope::new(self.current_scope));
        self.current_scope = child;
    }

    /// Returns the temporaries created in the popped scope.
    #[track_caller]
    pub fn pop_scope(&mut self) {
        let scope = &self.scope_arena[self.current_scope];
        self.current_scope = scope.parent.expect("Popped global scope");
    }
}

#[derive(Debug)]
struct Scope {
    symbols: HashMap<Identifier, Symbol>,
    parent: Option<ScopeIndex>,
    children: Vec<ScopeIndex>,
}

impl Scope {
    fn global() -> Self {
        let mut symbols = HashMap::new();
        let mut types = TypeMap::default();
        let add2 = types.push(Type::Function(Signature {
            args: vec![TypeId::INT],
            ret: TypeId::INT,
        }));
        symbols.insert("i32".into(), Symbol::Type(TypeId::INT));
        symbols.insert("bool".into(), Symbol::Type(TypeId::BOOL));
        symbols.insert("add2".into(), Symbol::Variable(add2));
        Scope {
            symbols,
            parent: None,
            children: vec![],
        }
    }

    fn new(parent: ScopeIndex) -> Self {
        Scope {
            symbols: HashMap::new(),
            parent: Some(parent),
            children: vec![],
        }
    }

    fn append_child(&mut self, child: ScopeIndex) {
        self.children.push(child);
    }

    fn insert(&mut self, name: &str, symbol: Symbol) {
        self.symbols.insert(name.into(), symbol);
    }

    fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }
}

type ScopeIndex = usize;

#[derive(Debug, PartialEq)]
pub enum Symbol {
    Variable(TypeId),
    Type(TypeId),
}
