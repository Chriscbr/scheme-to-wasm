use crate::types::Type;
use crate::util::format_vector;
use im_rc::Vector;
use std::fmt::Debug;
use std::fmt::Display;

use std::sync::atomic::{AtomicU64, Ordering};

// "global variable" usage derived from https://stackoverflow.com/a/27826181
static GENSYM_COUNT: AtomicU64 = AtomicU64::new(0);

pub fn generate_env_name() -> String {
    let name = format!("env{}", GENSYM_COUNT.load(Ordering::SeqCst));
    GENSYM_COUNT.fetch_add(1, Ordering::SeqCst);
    name
}

pub fn generate_record_name() -> String {
    let name = format!("Record{}", GENSYM_COUNT.load(Ordering::SeqCst));
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
///
/// If tests aren't being run sequentially / in series, then it's entirely
/// possible that the GENSYM_COUNT counter will get desynchronized. The primary
/// issue with this isn't that it will generate inconsistent programs, but
/// that the programs may have temporary variable names etc. that vary
/// from one compilation to another, which makes validating tests more
/// difficult.
pub fn dangerously_reset_gensym_count() {
    GENSYM_COUNT.store(0, Ordering::SeqCst);
}

/// A blanket trait that defines all of the traits that all expression types
/// must share / implement.
///
/// Currently, the two expression types are Expr and TypedExpr. Once an
/// expression type implements all of these traits, it will be usable within
/// all other types that are generic over expression types, such as
/// ExprKind and Prog.
pub trait ExprMeta: Clone + Debug + Display + PartialEq {}
impl<T: Clone + Debug + Display + PartialEq> ExprMeta for T {}

/// A representation of an expression (essentially an AST node) without any
/// associated type information.
#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub kind: Box<ExprKind<Expr>>,
}

impl Expr {
    pub fn new(kind: ExprKind<Expr>) -> Self {
        Expr {
            kind: Box::new(kind),
        }
    }
}

/// A representation of an expression (essentially an AST node) with
/// associated type information.
#[derive(Clone, Debug, PartialEq)]
pub struct TypedExpr {
    pub typ: Type,
    pub kind: Box<ExprKind<TypedExpr>>,
}

impl TypedExpr {
    pub fn new(typ: Type, kind: ExprKind<TypedExpr>) -> Self {
        TypedExpr {
            typ,
            kind: Box::new(kind),
        }
    }
}

/// A representation of a kind of expression in our language.
///
/// This enum is parameterized over E, a parent expression type such as
/// `Expr` or `TypedExpr`. The goal behind this abstraction is that we want to
/// support different kinds of abstract syntax trees (ASTs) whose nodes contain
/// different kinds of extra information, e.g. untyped trees (`Expr`) with no
/// extra information, or typed trees (`TypedExpr`), in which each node has
/// a typed annotation field. However, all of these ASTs will share the same
/// method of recursive definition, which is through an `ExprKind` type field.
/// Through this definition, we will still get the type guarantee that children
/// of a E (Expr, TypedExpr) must also be a corresponding E, without having
/// to define the ExprKind cases (Binop, If, etc.) in multiple places.
///
/// TODO: See if it's possible to add a "kind: ExprKind" field as a requirement
/// ExprMeta, in a way so that it's then required for both Expr and TypedExpr
/// and any other expression types that are developed later.
/// TODO: add (not x) operation
/// TODO: add (string-equal? x y) operation
/// TODO: add (incr x) and (decr x) operations
/// TODO: add (set-car! x) and (set-cdr! x) operations
#[allow(clippy::large_enum_variant)]
#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind<E: ExprMeta> {
    Binop(BinOp, E, E),                      // operator, arg1, arg2
    If(E, E, E),                             // pred, consequent, alternate
    Let(Vector<(String, E)>, E),             // variable bindings, body
    Lambda(Vector<(String, Type)>, Type, E), // arg names/types, return type, body
    Begin(Vector<E>),
    Set(String, E),
    Cons(E, E),
    Car(E),
    Cdr(E),
    IsNull(E),
    Null(Type),
    FnApp(E, Vector<E>),         // func, arguments
    Tuple(Vector<E>),            // list of expressions, type annotation
    TupleGet(E, u32),            // env, index - index must explicitly be a number
    Pack(E, Type, Type),         // exp, type substitution, existential type
    Unpack(String, E, u64, E),   // new var, package, type var, body
    Record(Vector<(String, E)>), // map from values to labels
    RecordGet(E, String),        // record, label
    Id(String),
    Num(i32),
    Bool(bool),
    Str(String),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for TypedExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl<E: Clone + Display + Debug + PartialEq> Display for ExprKind<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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
            // TODO: change to (pack typ_sub val : exist)?
            ExprKind::Pack(val, sub, exist) => write!(f, "(pack {} {} {})", val, sub, exist),
            ExprKind::Unpack(var, package, typ_sub, body) => {
                write!(f, "(unpack ({} {} T{}) {})", var, package, typ_sub, body)
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

impl<T: Clone> From<Vector<(String, T)>> for TypeEnv<T> {
    fn from(bindings: Vector<(String, T)>) -> Self {
        TypeEnv { bindings }
    }
}

// TODO: implment a generic trait (e.g. "CompilerPass"?) which just requires
// implementing a TypedExpr -> TypedExpr function; then automatically allow any
// pass to be applied to Prog<TypedExpr> through a generic implementation

#[derive(Clone)]
pub struct Prog<E> {
    pub fns: Vector<(String, E)>,
    pub exp: E,
}

impl<E: ExprMeta> std::fmt::Display for Prog<E> {
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
