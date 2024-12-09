use std::collections::HashMap;

pub struct TypeMap(HashMap<TypeId, Type>);

impl Default for TypeMap {
    fn default() -> Self {
        let mut map = TypeMap::new();
        map.insert(TypeId::INT, Type::Int);
        map.insert(TypeId::BOOL, Type::Bool);
        map.insert(TypeId::FUNCTION, Type::Function);
        map
    }
}

impl TypeMap {
    fn new() -> Self {
        TypeMap(HashMap::new())
    }

    pub fn insert(&mut self, id: TypeId, ty: Type) {
        self.0.insert(id, ty);
    }

    pub fn get(&self, id: TypeId) -> Option<&Type> {
        self.0.get(&id)
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub struct TypeId(usize);

impl TypeId {
    pub const VOID: TypeId = TypeId(0);
    pub const INT: TypeId = TypeId(1);
    pub const BOOL: TypeId = TypeId(2);
    pub const FUNCTION: TypeId = TypeId(3);
}

#[derive(Debug, PartialEq, Clone)]
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
