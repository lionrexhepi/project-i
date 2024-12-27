use std::collections::HashMap;

#[derive(Debug)]
pub struct TypeMap {
    types: Vec<Type>,
    functions: HashMap<Signature, TypeId>,
}

impl Default for TypeMap {
    fn default() -> Self {
        TypeMap {
            types: vec![Type::Unit, Type::Int, Type::Bool],
            functions: HashMap::new(),
        }
    }
}

impl TypeMap {
    pub fn push(&mut self, ty: Type) -> TypeId {
        let id = TypeId(self.types.len());
        self.types.push(ty);
        id
    }

    pub fn get(&self, id: TypeId) -> Option<&Type> {
        self.types.get(id.0)
    }

    pub fn function_pointer(&mut self, args: Vec<TypeId>, ret: TypeId) -> TypeId {
        let sig = Signature { args, ret };
        if let Some(&id) = self.functions.get(&sig) {
            return id;
        }
        let id = self.push(Type::Function(sig.clone()));
        self.functions.insert(sig, id);
        id
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
    Unit,
    Int,
    Bool,
    Function(Signature),
}

impl Type {
    pub fn c_name(&self) -> &str {
        match self {
            Type::Unit => "void",
            Type::Int => "int",
            Type::Bool => "bool",
            Type::Function { .. } => "#!!!!!SENTINEL_GCC_ERRORTHIS!!!!#",
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Type::Unit => "unit",
            Type::Int => "int",
            Type::Bool => "bool",
            Type::Function { .. } => "function",
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Signature {
    pub args: Vec<TypeId>,
    pub ret: TypeId,
}
