use std::collections::HashMap;

use smol_str::SmolStr;

use crate::ast::Identifier;

use super::types::{Type, TypeId, TypeMap};

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
            println!("Checking scope #{scope}");
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

    pub fn create_temporary(&mut self) -> TempId {
        self.scope_arena
            .get_mut(self.current_scope)
            .expect("Must not pop global scope")
            .create_temporary()
    }

    pub fn set_temporary_type(&mut self, id: TempId, ty: TypeId) {
        self.scope_arena
            .get_mut(self.current_scope)
            .expect("Must not pop global scope")
            .set_temporary_type(id, ty);
    }

    #[track_caller]
    pub fn push_scope(&mut self) {
        println!("Pushing scope, {:#?}", std::panic::Location::caller());
        let child = self.scope_arena.len();
        self.scope_arena[self.current_scope].append_child(child);
        self.scope_arena.push(Scope::new(self.current_scope));
        self.current_scope = child;
    }

    /// Returns the temporaries created in the popped scope.
    #[track_caller]
    pub fn pop_scope(&mut self) -> Vec<(TempId, TypeId)> {
        let scope = &self.scope_arena[self.current_scope];
        self.current_scope = scope.parent.expect("Popped global scope");
        println!(
            "Popping scope, {:#?}. New scope: {}",
            std::panic::Location::caller(),
            self.current_scope
        );

        scope.temporaries().collect()
    }
}

#[derive(Debug)]
struct Scope {
    symbols: HashMap<Identifier, Symbol>,
    temporaries: Vec<Temp>,
    parent: Option<ScopeIndex>,
    children: Vec<ScopeIndex>,
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
            parent: None,
            children: vec![],
        }
    }

    fn new(parent: ScopeIndex) -> Self {
        Scope {
            symbols: HashMap::new(),
            temporaries: vec![],
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

    fn create_temporary(&mut self) -> TempId {
        let id = TempId(self.temporaries.len());
        self.temporaries.push(Temp { ty: None, id });
        id
    }

    fn set_temporary_type(&mut self, id: TempId, ty: TypeId) {
        self.temporaries[id.0].ty = Some(ty);
    }

    fn temporaries(&self) -> impl Iterator<Item = (TempId, TypeId)> + '_ {
        self.temporaries
            .iter()
            .map(|tmp| (tmp.id, tmp.ty.expect("Temporary type not set")))
    }
}

#[derive(Debug, Clone, Copy)]
struct Temp {
    id: TempId,
    ty: Option<TypeId>,
}

type ScopeIndex = usize;

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
