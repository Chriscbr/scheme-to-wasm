use im_rc::Vector;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Str,
    List(Box<Type>),
    Func(Vector<Type>, Box<Type>), // array of input types, and a return type
    Tuple(Vector<Type>),           // array of types
    Unknown,                       // placeholder
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Str => write!(f, "string"),
            Type::List(typ) => write!(f, "(list {})", typ),
            Type::Func(in_typs, ret_typ) => {
                // TODO: extract this to a function
                let mut in_typs_str = String::new();
                for typ in in_typs {
                    in_typs_str.push_str(" ");
                    in_typs_str.push_str(format!("{}", typ).as_str());
                }
                write!(f, "(-> {}{})", in_typs_str, ret_typ)
            }
            Type::Tuple(typs) => {
                let mut typs_str = String::new();
                for typ in typs {
                    typs_str.push_str(" ");
                    typs_str.push_str(format!("{}", typ).as_str());
                }
                write!(f, "(tuple{})", typs_str)
            }
            // // TODO: add existential types properly
            // Type::Env(typs) => {
            //     let mut typs_str = String::new();
            //     for typ in typs {
            //         typs_str.push_str(" ");
            //         typs_str.push_str(format!("{}", typ).as_str());
            //     }
            //     write!(f, "(env ({}))", typs_str)
            // }
            Type::Unknown => write!(f, "unknown"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub checked_type: Type,
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(kind: ExprKind) -> Expr {
        Expr {
            checked_type: Type::Unknown,
            kind,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
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
    FnApp(Box<Expr>, Vector<Expr>),    // func, arguments
    Tuple(Vector<Expr>, Vector<Type>), // list of expressions, type annotation
    TupleGet(Box<Expr>, Box<Expr>),    // env, index - index must explicitly be a number
    Env(Vector<(String, Expr)>),       // map from var_name to exp
    EnvGet(Box<Expr>, String),         // env, key
    Id(String),
    Num(i64),
    Bool(bool),
    Str(String),
}

// TODO: Finish implementation
impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ExprKind::Binop(op, exp1, exp2) => write!(f, "({} {} {})", op, exp1, exp2),
            ExprKind::If(pred, cons, alt) => write!(f, "(if {} {} {})", pred, cons, alt),
            ExprKind::Let(bindings, body) => {
                let mut bindings_str = String::new();
                for pair in bindings {
                    bindings_str.push_str(format!("({} {}) ", pair.0, pair.1).as_str());
                }
                bindings_str.pop();
                write!(f, "(let ({}) {})", bindings_str, body)
            }
            ExprKind::Lambda(params, ret_type, body) => {
                let mut params_str = String::new();
                for pair in params {
                    params_str.push_str(format!("({} : {}) ", pair.0, pair.1).as_str());
                }
                if !params.is_empty() {
                    params_str.pop();
                }
                write!(f, "(lambda ({}) : {} {})", params_str, ret_type, body)
            }
            ExprKind::FnApp(func, args) => {
                let mut args_str = String::new();
                for arg in args {
                    args_str.push_str(format!(" {}", arg).as_str());
                }
                write!(f, "({}{})", func, args_str)
            }
            ExprKind::Env(bindings) => {
                let mut bindings_str = String::new();
                for binding in bindings {
                    bindings_str.push_str(format!(" ({} {})", binding.0, binding.1).as_str());
                }
                write!(f, "(make-env {})", bindings_str)
            }
            ExprKind::EnvGet(clos_env, key) => write!(f, "(env-ref {} {})", clos_env, key),
            ExprKind::Begin(exps) => {
                let mut exps_str = String::new();
                for exp in exps {
                    exps_str.push_str(format!(" {}", exp).as_str());
                }
                write!(f, "(begin {})", exps_str)
            }
            ExprKind::Set(var_name, exp) => write!(f, "(set! {} {})", var_name, exp),
            ExprKind::Cons(first, second) => write!(f, "(cons {} {})", first, second),
            ExprKind::Car(exp) => write!(f, "(car {})", exp),
            ExprKind::Cdr(exp) => write!(f, "(cdr {})", exp),
            ExprKind::IsNull(exp) => write!(f, "(null? {})", exp),
            ExprKind::Null(typ) => write!(f, "(null {})", typ),
            ExprKind::Tuple(exps, typs) => {
                let mut exps_str = String::new();
                for exp in exps {
                    exps_str.push_str(format!(" {}", exp).as_str());
                }
                let mut typs_str = String::new();
                for typ in typs {
                    typs_str.push_str(format!("{} ", typ).as_str());
                }
                if !typs.is_empty() {
                    typs_str.pop();
                }
                write!(f, "(make-tuple{} : ({}))", exps_str, typs_str)
            }
            ExprKind::TupleGet(tup, key) => write!(f, "(get-nth {} {})", tup, key),
            ExprKind::Id(val) => write!(f, "{}", val),
            ExprKind::Num(val) => write!(f, "{}", val),
            ExprKind::Bool(val) => write!(f, "{}", if *val { "true" } else { "false" }),
            ExprKind::Str(val) => write!(f, "\"{}\"", val),
        }
    }
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
