#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    Bool,
    Int(u32),
    Float(u32),
    String,
    Tensor(Shape, DType),
    Mut(Box<Type>),
    Ref(Box<Type>),
    Cap(Box<Type>),
    Symbolic(String),
    Grad(Box<Type>),
    DepType {
        var: String,
        var_type: Box<Type>,
        prop: String, // or a logical expression AST node
    },
    Simple(String),
}

// Additional structs for Shape, DType, etc.
