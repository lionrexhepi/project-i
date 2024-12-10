pub struct TypeMap(Vec<Type>);

impl Default for TypeMap {
    fn default() -> Self {
        TypeMap(vec![
            Type::Unit,
            Type::Int,
            Type::Bool,
            Type::Function {
                args: vec![],
                ret: TypeId::VOID,
            },
        ])
    }
}

impl TypeMap {
    pub fn push(&mut self, ty: Type) -> TypeId {
        let id = TypeId(self.0.len());
        self.0.push(ty);
        id
    }

    pub fn get(&self, id: TypeId) -> Option<&Type> {
        self.0.get(id.0)
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
    Function { args: Vec<TypeId>, ret: TypeId },
}

impl Type {
    pub fn name(&self) -> &str {
        match self {
            Type::Unit => "void",
            Type::Int => "int",
            Type::Bool => "bool",
            Type::Function { args: _, ret: _ } => "###FUNCTION###", // To make GCC error if this gets into the generated C code
        }
    }
}
