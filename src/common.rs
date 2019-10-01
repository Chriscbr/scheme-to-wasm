use im_rc::Vector;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Str,
    List(Box<Type>),
    Func(Vector<Type>, Box<Type>), // array of input types, and a return type
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Binop(BinOp, Box<Expr>, Box<Expr>),     // operator, arg1, arg2
    If(Box<Expr>, Box<Expr>, Box<Expr>),    // pred, consequent, alternate
    Let(Vector<(String, Expr)>, Box<Expr>), // variable bindings, body
    Lambda(Vector<(String, Type)>, Type, Box<Expr>), // arg names/types, return type, body
    Begin(Vector<Expr>),
    Set(String, Box<Expr>),
    Cons(Box<Expr>, Box<Expr>),
    Car(Box<Expr>),
    Cdr(Box<Expr>),
    IsNull(Box<Expr>),
    Null(Type),
    FnApp(Box<Expr>, Vector<Expr>), // func, arguments
    Id(String),
    Num(i64),
    Bool(bool),
    Str(String),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    LessOrEqual,
    GreaterOrEqual,
    EqualTo,
    And,
    Or,
    Concat,
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Subtract => write!(f, "-"),
            BinOp::Multiply => write!(f, "*"),
            BinOp::Divide => write!(f, "/"),
            BinOp::LessThan => write!(f, "<"),
            BinOp::GreaterThan => write!(f, ">"),
            BinOp::LessOrEqual => write!(f, "<="),
            BinOp::GreaterOrEqual => write!(f, ">="),
            BinOp::EqualTo => write!(f, "="),
            BinOp::And => write!(f, "and"),
            BinOp::Or => write!(f, "or"),
            BinOp::Concat => write!(f, "concat"),
        }
    }
}

#[derive(Default, Debug)]
pub struct TypeEnv<T: Clone> {
    bindings: Vector<(String, T)>,
}

// New values are appended to the front of the frame
impl<T: Clone> TypeEnv<T> {
    pub fn new() -> Self {
        TypeEnv {
            bindings: Vector::new(),
        }
    }

    /// Returns a new environment extended with the provided binding.
    pub fn add_binding(&self, new_binding: (String, T)) -> TypeEnv<T> {
        let mut bindings = self.bindings.clone();
        bindings.push_front(new_binding);
        TypeEnv { bindings }
    }

    /// Returns a new environment extended with the provided bindings.
    pub fn add_bindings(&self, new_bindings: Vector<(String, T)>) -> TypeEnv<T> {
        let mut bindings = self.bindings.clone();
        for binding in new_bindings {
            bindings.push_front(binding);
        }
        TypeEnv { bindings }
    }

    pub fn find(&self, key: &str) -> Option<&T> {
        for pair in self.bindings.iter() {
            if pair.0 == key {
                return Some(&pair.1);
            }
        }
        None
    }
}
