use crate::types::Type;
use crate::util::format_vector;
use im_rc::Vector;

use std::sync::atomic::{AtomicU64, Ordering};

// "global variable" usage derived from https://stackoverflow.com/a/27826181
static GENSYM_COUNT: AtomicU64 = AtomicU64::new(0);

pub fn generate_env_name() -> String {
    let name = format!("env{}", GENSYM_COUNT.load(Ordering::SeqCst));
    GENSYM_COUNT.fetch_add(1, Ordering::SeqCst);
    name
}

pub fn generate_var_name() -> String {
    let name = format!("temp{}", GENSYM_COUNT.load(Ordering::SeqCst));
    GENSYM_COUNT.fetch_add(1, Ordering::SeqCst);
    name
}

pub fn generate_func_name() -> String {
    let name = format!("func{}", GENSYM_COUNT.load(Ordering::SeqCst));
    GENSYM_COUNT.fetch_add(1, Ordering::SeqCst);
    name
}

pub fn generate_id() -> u64 {
    let val = GENSYM_COUNT.load(Ordering::SeqCst);
    GENSYM_COUNT.fetch_add(1, Ordering::SeqCst);
    val
}

/// Only use this for testing purposes!
pub fn dangerously_reset_gensym_count() {
    GENSYM_COUNT.store(0, Ordering::SeqCst);
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub checked_type: Type,
    pub kind: Box<ExprKind>,
}

impl Expr {
    pub fn new(kind: ExprKind) -> Expr {
        Expr {
            checked_type: Type::Unknown,
            kind: Box::from(kind),
        }
    }
}

// TODO: add (not x) operator
#[allow(clippy::large_enum_variant)]
#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Binop(BinOp, Expr, Expr),                   // operator, arg1, arg2
    If(Expr, Expr, Expr),                       // pred, consequent, alternate
    Let(Vector<(String, Expr)>, Expr),          // variable bindings, body
    Lambda(Vector<(String, Type)>, Type, Expr), // arg names/types, return type, body
    Begin(Vector<Expr>),
    Set(String, Expr),
    Cons(Expr, Expr),
    Car(Expr),
    Cdr(Expr),
    IsNull(Expr),
    Null(Type),
    FnApp(Expr, Vector<Expr>),       // func, arguments
    Tuple(Vector<Expr>),             // list of expressions, type annotation
    TupleGet(Expr, u64),             // env, index - index must explicitly be a number
    Pack(Expr, Type, Type),          // exp, type substitution, existential type
    Unpack(String, Expr, u64, Expr), // new var, package, type var, body
    Record(Vector<(String, Expr)>),  // map from values to labels
    RecordGet(Expr, String),         // record, label
    Id(String),
    Num(i64),
    Bool(bool),
    Str(String),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &*self.kind {
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

#[derive(Clone)]
pub struct Prog {
    pub fns: Vector<(String, Expr)>,
    pub exp: Expr,
}

impl std::fmt::Display for Prog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut fns_str = String::new();
        if !self.fns.is_empty() {
            for pair in self.fns.iter() {
                fns_str.push_str(format!("{}: {}, ", pair.0, pair.1).as_str())
            }
            fns_str.pop();
            fns_str.pop();
        }
        write!(f, "Prog(fns: ({}), exp: {})", fns_str, self.exp)
    }
}
