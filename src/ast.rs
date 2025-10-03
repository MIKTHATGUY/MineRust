/// Abstract Syntax Tree representation for DataRust

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Function(Function),
    Struct(StructDef),
    Enum(EnumDef),
    Const(ConstDef),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDef {
    pub name: String,
    pub variants: Vec<Variant>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variant {
    pub name: String,
    pub data: Option<VariantData>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariantData {
    Tuple(Vec<Type>),
    Struct(Vec<Field>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstDef {
    pub name: String,
    pub ty: Option<Type>,
    pub value: Expr,
}

/// Type system
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // Primitives
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Str,
    Char,

    // Collections
    Vec(Box<Type>),
    ByteVec,
    I32Vec,
    I64Vec,

    // Custom
    Struct(String),
    Enum(String),

    // Advanced
    Option(Box<Type>),
    Result(Box<Type>, Box<Type>),

    // Storage modifier
    Fast(Box<Type>, Option<u32>), // Fast storage, optional scale for floats
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::F32
                | Type::F64
        )
    }

    pub fn is_fast_compatible(&self) -> bool {
        matches!(
            self,
            Type::Bool
                | Type::I8
                | Type::I16
                | Type::I32
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::F32
                | Type::F64
        )
    }

    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::F32 | Type::F64)
    }
}

/// Expressions
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Option<Type>, // Filled in by type checker
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    // Literals with explicit types
    IntLit(i64, Option<Type>), // Value and optional explicit type (i8, i16, i32, i64, u8, u16, u32, u64)
    FloatLit(f64, Option<Type>), // Value and optional explicit type (f32, f64)
    BoolLit(bool),
    StringLit(String),
    CharLit(char),

    // Variables
    Var(String),

    // Arithmetic operations
    BinaryOp {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    UnaryOp {
        op: UnOp,
        expr: Box<Expr>,
    },

    // Variable declaration
    Let {
        name: String,
        ty: Option<Type>,
        value: Box<Expr>,
    },

    // Assignment
    Assign {
        target: String,
        value: Box<Expr>,
    },

    // Function call
    Call {
        name: String,
        args: Vec<Expr>,
    },

    // Type cast
    Cast {
        expr: Box<Expr>,
        target_ty: Type,
    },

    // Method call (e.g., .clone())
    Method {
        receiver: Box<Expr>,
        method: String,
        args: Vec<Expr>,
    },

    // Block (sequence of expressions)
    Block(Vec<Expr>),

    // Control flow
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
    },

    // Struct literal
    StructLit {
        name: String,
        fields: Vec<(String, Expr)>,
    },

    // Field access
    FieldAccess {
        expr: Box<Expr>,
        field: String,
    },

    // Vector literal
    VecLit(Vec<Expr>),

    // Index access
    Index {
        expr: Box<Expr>,
        index: Box<Expr>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Comparison (for control flow)
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    // Logical operators
    And,
    Or,
    // Bitwise operators
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnOp {
    Neg,
    Not,
}

impl Expr {
    pub fn new(kind: ExprKind) -> Self {
        Expr { kind, ty: None }
    }
}
