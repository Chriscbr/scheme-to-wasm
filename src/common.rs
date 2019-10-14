use crate::util::format_vector;
use im_rc::Vector;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Str,
    List(Box<Type>),                // homogenous list
    Func(Vector<Type>, Box<Type>),  // array of input types, and a return type
    Tuple(Vector<Type>),            // array of types
    Record(Vector<(String, Type)>), // array of bindings
    Exists(u64, Box<Type>),         // abstract type T, and base type in terms of T
    TypeVar(u64),                   // abstract type T
    Unknown,                        // placeholder
}

pub fn type_contains_var(typ: &Type, var: u64) -> bool {
    match typ {
        Type::Int => false,
        Type::Bool => false,
        Type::Str => false,
        Type::List(x) => type_contains_var(x, var),
        Type::Func(typs, ret_typ) => {
            typs.iter().any(|typ| type_contains_var(typ, var)) || type_contains_var(ret_typ, var)
        }
        Type::Tuple(typs) => typs.iter().any(|typ| type_contains_var(typ, var)),
        Type::Record(fields) => fields.iter().any(|field| type_contains_var(&field.1, var)),
        Type::Exists(bound_var, typ) => *bound_var != var && type_contains_var(typ, var),
        Type::TypeVar(x) => *x == var,
        Type::Unknown => false,
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Str => write!(f, "string"),
            Type::List(typ) => write!(f, "(list {})", typ),
            Type::Func(in_typs, ret_typ) => {
                if in_typs.is_empty() {
                    write!(f, "(-> {})", ret_typ)
                } else {
                    write!(f, "(-> {} {})", format_vector(in_typs.clone()), ret_typ)
                }
            }
            Type::Tuple(typs) => write!(f, "(tuple {})", format_vector(typs.clone())),
            Type::Record(bindings) => {
                if bindings.is_empty() {
                    write!(f, "(record)")
                } else {
                    let bindings_str_vec = bindings
                        .iter()
                        .map(|pair| format!("({} : {})", pair.0, pair.1))
                        .collect();
                    write!(f, "(record {})", format_vector(bindings_str_vec))
                }
            }
            Type::Exists(typ_var, base) => write!(f, "(exists T{} {})", typ_var, base),
            Type::TypeVar(id) => write!(f, "T{}", id),
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
    FnApp(Box<Expr>, Vector<Expr>),             // func, arguments
    Tuple(Vector<Expr>),                        // list of expressions, type annotation
    TupleGet(Box<Expr>, u64),                   // env, index - index must explicitly be a number
    Pack(Box<Expr>, Type, Type),                // exp, type substitution, existential type
    Unpack(String, Box<Expr>, Type, Box<Expr>), // new var, package, type substitution, body
    Record(Vector<(String, Expr)>),             // map from values to labels
    RecordGet(Box<Expr>, String),               // record, label
    Id(String),
    Num(i64),
    Bool(bool),
    Str(String),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ExprKind::Binop(op, exp1, exp2) => write!(f, "({} {} {})", op, exp1, exp2),
            ExprKind::If(pred, cons, alt) => write!(f, "(if {} {} {})", pred, cons, alt),
            ExprKind::Let(bindings, body) => {
                let bindings_str_vec = bindings
                    .iter()
                    .map(|pair| format!("({} {})", pair.0, pair.1))
                    .collect();
                write!(f, "(let ({}) {})", format_vector(bindings_str_vec), body)
            }
            ExprKind::Lambda(params, ret_type, body) => {
                let params_str_vec = params
                    .iter()
                    .map(|pair| format!("({} : {})", pair.0, pair.1))
                    .collect();
                write!(
                    f,
                    "(lambda ({}) : {} {})",
                    format_vector(params_str_vec),
                    ret_type,
                    body
                )
            }
            ExprKind::FnApp(func, args) => write!(f, "({} {})", func, format_vector(args.clone())),
            ExprKind::Record(bindings) => {
                if bindings.is_empty() {
                    write!(f, "(make-record)")
                } else {
                    let bindings_str_vec = bindings
                        .iter()
                        .map(|pair| format!("({} {})", pair.0, pair.1))
                        .collect();
                    write!(f, "(make-record {})", format_vector(bindings_str_vec))
                }
            }
            ExprKind::RecordGet(record, key) => write!(f, "(record-ref {} {})", record, key),
            ExprKind::Begin(exps) => write!(f, "(begin {})", format_vector(exps.clone())),
            ExprKind::Set(var_name, exp) => write!(f, "(set! {} {})", var_name, exp),
            ExprKind::Cons(first, second) => write!(f, "(cons {} {})", first, second),
            ExprKind::Car(exp) => write!(f, "(car {})", exp),
            ExprKind::Cdr(exp) => write!(f, "(cdr {})", exp),
            ExprKind::IsNull(exp) => write!(f, "(null? {})", exp),
            ExprKind::Null(typ) => write!(f, "(null {})", typ),
            ExprKind::Tuple(exps) => write!(f, "(make-tuple {})", format_vector(exps.clone())),
            ExprKind::TupleGet(tup, key) => write!(f, "(tuple-ref {} {})", tup, key),
            // TODO: change to (pack typ_sub val : exist)
            ExprKind::Pack(val, sub, exist) => write!(f, "(pack {} {} {})", val, sub, exist),
            ExprKind::Unpack(var, package, typ_sub, body) => {
                write!(f, "(unpack ({} {} {}) {})", var, package, typ_sub, body)
            }
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
