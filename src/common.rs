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

#[derive(Clone, Debug)]
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

// PartialEq is implemented manually to handle the specific case where two
// types are both existential types, and they should be equal with respect to
// substitution of one type variable for the other
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::List(base_a), Type::List(base_b)) => base_a == base_b,
            (Type::Func(in_a, ret_a), Type::Func(in_b, ret_b)) => in_a == in_b && ret_a == ret_b,
            (Type::Tuple(vec_a), Type::Tuple(vec_b)) => vec_a == vec_b,
            (Type::Record(vec_a), Type::Record(vec_b)) => vec_a == vec_b,
            (Type::Exists(typ_var_a, base_typ_a), Type::Exists(typ_var_b, base_typ_b)) => {
                let other_sub = type_substitute(base_typ_b, *typ_var_b, &Type::TypeVar(*typ_var_a));
                **base_typ_a == other_sub
            }
            (Type::TypeVar(a), Type::TypeVar(b)) => a == b,
            (Type::Int, Type::Int) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Str, Type::Str) => true,
            (Type::Unknown, Type::Unknown) => true,
            (_, _) => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypeSubstituteError(String);

impl From<&str> for TypeSubstituteError {
    fn from(message: &str) -> Self {
        TypeSubstituteError(String::from(message))
    }
}

impl std::fmt::Display for TypeSubstituteError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "TypeSubstituteError: {}", self.0)
    }
}

// allows other errors to wrap this one
// see https://doc.rust-lang.org/rust-by-example/error/multiple_error_types/define_error_type.html
impl std::error::Error for TypeSubstituteError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        // Generic error, underlying cause isn't tracked.
        None
    }
}

// TODO: rename to type_var_substitute
pub fn type_substitute(typ: &Type, type_var: u64, replace_with: &Type) -> Type {
    match typ {
        Type::Int => Type::Int,
        Type::Bool => Type::Bool,
        Type::Str => Type::Str,
        Type::List(base_typ) => {
            let sbase_typ = type_substitute(base_typ, type_var, replace_with);
            Type::List(Box::from(sbase_typ))
        }
        Type::Func(in_typs, ret_typ) => {
            let sin_typs: Vector<Type> = in_typs
                .iter()
                .map(|inner_typ| type_substitute(inner_typ, type_var, replace_with))
                .collect();
            let sret_typ = type_substitute(ret_typ, type_var, replace_with);
            Type::Func(sin_typs, Box::from(sret_typ))
        }
        Type::Tuple(typs) => {
            let styps: Vector<Type> = typs
                .iter()
                .map(|inner_typ| type_substitute(inner_typ, type_var, replace_with))
                .collect();
            Type::Tuple(styps)
        }
        Type::Record(bindings) => {
            let sbindings: Vector<(String, Type)> = bindings
                .iter()
                .map(|pair| {
                    let styp = type_substitute(&pair.1, type_var, replace_with);
                    (pair.0.clone(), styp)
                })
                .collect();
            Type::Record(sbindings)
        }
        Type::Exists(base_typ_var, base_typ) => {
            if *base_typ_var == type_var {
                let new_base_typ_var = type_var + 1;
                let base_typ_clean =
                    type_substitute(base_typ, *base_typ_var, &Type::TypeVar(new_base_typ_var));
                let sbase_typ = type_substitute(&base_typ_clean, type_var, replace_with);
                Type::Exists(new_base_typ_var, Box::from(sbase_typ))
            } else {
                let sbase_typ = type_substitute(base_typ, type_var, replace_with);
                Type::Exists(*base_typ_var, Box::from(sbase_typ))
            }
        }
        Type::TypeVar(x) => {
            if *x == type_var {
                replace_with.clone()
            } else {
                Type::TypeVar(*x)
            }
        }
        Type::Unknown => Type::Unknown,
    }
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
    FnApp(Expr, Vector<Expr>), // func, arguments
    Tuple(Vector<Expr>),       // list of expressions, type annotation
    TupleGet(Expr, u64),       // env, index - index must explicitly be a number
    Pack(Expr, Type, Type),    // exp, type substitution, existential type
    // TODO: change third argument to u64 to enforce that it is abstract type?
    Unpack(String, Expr, Type, Expr), // new var, package, type var, body
    Record(Vector<(String, Expr)>),   // map from values to labels
    RecordGet(Expr, String),          // record, label
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

pub struct Prog {
    pub fns: Vector<(String, Expr)>,
    pub exp: Expr,
}
