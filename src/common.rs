use crate::type_check::validate_lambda_type;
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

pub fn transform_type_recursive<G, E>(typ: &Type, transform_type: G) -> Result<Type, E>
where
    G: Fn(&Type) -> Option<Result<Type, E>> + Copy,
    E: std::error::Error,
{
    if let Some(ttype) = transform_type(&typ) {
        return ttype;
    }
    match typ {
        Type::Int => Ok(Type::Int),
        Type::Bool => Ok(Type::Bool),
        Type::Str => Ok(Type::Str),
        Type::List(base_type) => {
            let tbase_type = transform_type_recursive(base_type, transform_type)?;
            Ok(Type::List(Box::new(tbase_type)))
        }
        Type::Func(in_types, ret_type) => {
            let tin_types = transform_type_array(in_types, transform_type)?;
            let tret_type = transform_type_recursive(ret_type, transform_type)?;
            Ok(Type::Func(tin_types, Box::new(tret_type)))
        }
        Type::Tuple(types) => {
            let ttypes = transform_type_array(types, transform_type)?;
            Ok(Type::Tuple(ttypes))
        }
        Type::Record(bindings) => {
            let tbindings = bindings
                .iter()
                .map(|(name, inner_type)| {
                    Ok((
                        name.clone(),
                        transform_type_recursive(inner_type, transform_type)?,
                    ))
                })
                .collect::<Result<Vector<(String, Type)>, E>>()?;
            Ok(Type::Record(tbindings))
        }
        Type::Exists(type_var, base_type) => {
            let tbase_type = transform_type_recursive(base_type, transform_type)?;
            Ok(Type::Exists(*type_var, Box::new(tbase_type)))
        }
        Type::TypeVar(x) => Ok(Type::TypeVar(*x)),
        Type::Unknown => Ok(Type::Unknown),
    }
}

fn transform_type_array<G, E>(types: &Vector<Type>, transform_type: G) -> Result<Vector<Type>, E>
where
    G: Fn(&Type) -> Option<Result<Type, E>> + Copy,
    E: std::error::Error,
{
    types
        .iter()
        .map(|typ| Ok(transform_type_recursive(typ, transform_type)?))
        .collect()
}

/// Performs a transformation on a typed AST, provided a function for
/// transforming expressions and a function for transforming types.
///
/// In general, when choosing how to construct a `transform_exp` to pass in,
/// the function should choose some ExprKind cases to transform, and
/// some ExprKind cases to leave as normal (but still recurse on). For example,
/// the Record Elimination compiler pass will have special logic to return
/// Some(Result) for the cases ExprKind::Record and ExprKind::RecordGet, but
/// in all other cases it will return None.
///
/// By doing so, we will still recursively apply the desired transformation
/// everywhere in the tree, but the individualized logic for recursing into
/// children of different kinds of ExprKind nodes does not have to be
/// duplicated for every compiler pass.
pub fn transform_typed_exp_recursive<'a, F, G, E>(
    exp: &TypedExpr,
    transform_exp: F,
    transform_type: G,
) -> Result<TypedExpr, E>
where
    F: Fn(&TypedExpr) -> Option<Result<TypedExpr, E>> + Copy,
    G: Fn(&Type) -> Option<Result<Type, E>> + Copy,
    E: std::error::Error + From<&'a str>,
{
    // If the user's custom `transform_exp` function has a special way to
    // transform the provided node, then let's return that value.
    if let Some(transformed_exp) = transform_exp(&exp) {
        return transformed_exp;
    }
    // Otherwise, recurse normally according to the individual structures.
    match &*exp.kind {
        ExprKind::Num(x) => Ok(TypedExpr::new(Type::Int, ExprKind::Num(*x))),
        ExprKind::Bool(x) => Ok(TypedExpr::new(Type::Bool, ExprKind::Bool(*x))),
        ExprKind::Str(x) => Ok(TypedExpr::new(Type::Str, ExprKind::Str(x.clone()))),
        ExprKind::Id(x) => Ok(TypedExpr::new(
            transform_type_recursive(&exp.typ, transform_type)?,
            ExprKind::Id(x.clone()),
        )),
        ExprKind::Binop(op, arg1, arg2) => {
            let targ1 = transform_typed_exp_recursive(arg1, transform_exp, transform_type)?;
            let targ2 = transform_typed_exp_recursive(arg2, transform_exp, transform_type)?;
            Ok(TypedExpr::new(
                // I suppose we can recurse here (opposed to just cloning
                // exp.typ) in case there are binops in the future that
                // return non-primitive types.
                transform_type_recursive(&exp.typ, transform_type)?,
                ExprKind::Binop(*op, targ1, targ2),
            ))
        }
        ExprKind::If(pred, cons, alt) => {
            let tpred = transform_typed_exp_recursive(pred, transform_exp, transform_type)?;
            let tcons = transform_typed_exp_recursive(cons, transform_exp, transform_type)?;
            let talt = transform_typed_exp_recursive(alt, transform_exp, transform_type)?;
            Ok(TypedExpr::new(
                tcons.typ.clone(),
                ExprKind::If(tpred, tcons, talt),
            ))
        }
        ExprKind::Let(bindings, body) => {
            let tbindings = bindings
                .iter()
                .map(|(name, subexp)| {
                    let tsubexp =
                        transform_typed_exp_recursive(subexp, transform_exp, transform_type)?;
                    Ok((name.clone(), tsubexp))
                })
                .collect::<Result<Vector<(String, TypedExpr)>, E>>()?;
            let tbody = transform_typed_exp_recursive(body, transform_exp, transform_type)?;
            Ok(TypedExpr::new(
                tbody.typ.clone(),
                ExprKind::Let(tbindings, tbody),
            ))
        }
        ExprKind::Lambda(params, ret_type, body) => {
            let tparams = params
                .iter()
                .map(|(name, typ)| {
                    let ttype = transform_type_recursive(typ, transform_type)?;
                    Ok((name.clone(), ttype))
                })
                .collect::<Result<Vector<(String, Type)>, E>>()?;
            let tbody = transform_typed_exp_recursive(body, transform_exp, transform_type)?;
            let tret_type = transform_type_recursive(ret_type, transform_type)?;
            let param_types: Vector<Type> =
                tparams.iter().map(|(_name, typ)| typ.clone()).collect();
            let lambda_type = Type::Func(param_types, Box::new(tret_type.clone()));
            Ok(TypedExpr::new(
                lambda_type,
                ExprKind::Lambda(tparams, tret_type, tbody),
            ))
        }
        ExprKind::Begin(exps) => {
            let texps = exps
                .iter()
                .map(|subexp| transform_typed_exp_recursive(subexp, transform_exp, transform_type))
                .collect::<Result<Vector<TypedExpr>, E>>()?;
            let mut inner_types = texps
                .iter()
                .map(|rexp| rexp.typ.clone())
                .collect::<Vector<Type>>();
            Ok(TypedExpr::new(
                inner_types.remove(inner_types.len() - 1),
                ExprKind::Begin(texps),
            ))
        }
        ExprKind::Set(id, val) => {
            let tval = transform_typed_exp_recursive(val, transform_exp, transform_type)?;
            Ok(TypedExpr::new(
                tval.typ.clone(),
                ExprKind::Set(id.clone(), tval),
            ))
        }
        ExprKind::Cons(first, rest) => {
            let tfirst = transform_typed_exp_recursive(first, transform_exp, transform_type)?;
            let trest = transform_typed_exp_recursive(rest, transform_exp, transform_type)?;
            Ok(TypedExpr::new(
                trest.typ.clone(),
                ExprKind::Cons(tfirst, trest),
            ))
        }
        ExprKind::Car(val) => {
            let tval = transform_typed_exp_recursive(val, transform_exp, transform_type)?;
            match tval.typ.clone() {
                Type::List(boxed_type) => Ok(TypedExpr::new(*boxed_type, ExprKind::Car(tval))),
                _ => Err(E::from("Expression in car is not a list type.")),
            }
        }
        ExprKind::Cdr(val) => {
            let tval = transform_typed_exp_recursive(val, transform_exp, transform_type)?;
            Ok(TypedExpr::new(tval.typ.clone(), ExprKind::Cdr(tval)))
        }
        ExprKind::IsNull(val) => {
            let tval = transform_typed_exp_recursive(val, transform_exp, transform_type)?;
            Ok(TypedExpr::new(Type::Bool, ExprKind::IsNull(tval)))
        }
        ExprKind::Null(typ) => {
            let ttyp = transform_type_recursive(typ, transform_type)?;
            Ok(TypedExpr::new(
                Type::List(Box::new(typ.clone())),
                ExprKind::Null(ttyp),
            ))
        }
        ExprKind::Tuple(exps) => {
            let texps = exps
                .iter()
                .map(|subexp| transform_typed_exp_recursive(subexp, transform_exp, transform_type))
                .collect::<Result<Vector<TypedExpr>, E>>()?;
            let inner_types = texps
                .iter()
                .map(|typed_exp| typed_exp.typ.clone())
                .collect::<Vector<Type>>();
            Ok(TypedExpr::new(
                Type::Tuple(inner_types),
                ExprKind::Tuple(texps),
            ))
        }
        ExprKind::TupleGet(tuple, key) => {
            let ttuple = transform_typed_exp_recursive(tuple, transform_exp, transform_type)?;
            match ttuple.typ.clone() {
                Type::Tuple(vec) => {
                    if (*key as usize) < vec.len() {
                        let elem_type = vec[*key as usize].clone();
                        Ok(TypedExpr::new(elem_type, ExprKind::TupleGet(ttuple, *key)))
                    } else {
                        Err(E::from("Key in tuple-ref is too large for the tuple."))
                    }
                }
                _ => Err(E::from("First expression in tuple-ref is not a tuple.")),
            }
        }
        ExprKind::Record(bindings) => {
            let tbindings = bindings
                .iter()
                .map(|(name, subexp)| {
                    let tsubexp =
                        transform_typed_exp_recursive(subexp, transform_exp, transform_type)?;
                    Ok((name.clone(), tsubexp))
                })
                .collect::<Result<Vector<(String, TypedExpr)>, E>>()?;
            let exp_vec: Vec<TypedExpr> =
                tbindings.iter().map(|(_field, exp)| exp.clone()).collect();
            let inner_types = exp_vec
                .iter()
                .map(|typed_exp| typed_exp.typ.clone())
                .collect::<Vector<Type>>();
            Ok(TypedExpr::new(
                Type::Tuple(inner_types),
                ExprKind::Record(tbindings),
            ))
        }
        ExprKind::RecordGet(record, key) => {
            let trecord = transform_typed_exp_recursive(record, transform_exp, transform_type)?;
            let tkey_type = match trecord.typ.clone() {
                Type::Record(fields) => {
                    let matches: Vector<(String, Type)> = fields
                        .iter()
                        .cloned()
                        .filter(|pair| pair.0 == *key)
                        .collect();
                    if matches.is_empty() {
                        return Err(E::from("Key in record-get not found in record."));
                    }
                    matches[0].1.clone()
                }
                _ => return Err(E::from("Non-record expression within record-get.")),
            };
            Ok(TypedExpr::new(
                tkey_type,
                ExprKind::RecordGet(trecord, key.clone()),
            ))
        }
        ExprKind::Pack(val, sub, exist) => {
            let tval = transform_typed_exp_recursive(val, transform_exp, transform_type)?;
            let tsub = transform_type_recursive(sub, transform_type)?;
            let texist = transform_type_recursive(exist, transform_type)?;
            Ok(TypedExpr::new(
                texist.clone(),
                ExprKind::Pack(tval, tsub, texist),
            ))
        }
        ExprKind::Unpack(var, package, typ_sub, body) => {
            let tpackage = transform_typed_exp_recursive(package, transform_exp, transform_type)?;
            let tbody = transform_typed_exp_recursive(body, transform_exp, transform_type)?;
            Ok(TypedExpr::new(
                tbody.typ.clone(),
                ExprKind::Unpack(var.clone(), tpackage, *typ_sub, tbody),
            ))
        }
        ExprKind::FnApp(func, args) => {
            let tfunc = transform_typed_exp_recursive(func, transform_exp, transform_type)?;
            let targs = args
                .iter()
                .map(|arg| transform_typed_exp_recursive(arg, transform_exp, transform_type))
                .collect::<Result<Vector<TypedExpr>, E>>()?;
            let targs_types = targs
                .iter()
                .map(|arg| arg.typ.clone())
                .collect::<Vector<Type>>();
            let apply_type = match validate_lambda_type(&tfunc.typ, &targs_types) {
                Ok(val) => val,
                Err(_e) => {
                    // let message = format!("Error in validate_lambda_type: {}", e);
                    return Err(E::from("Error in validate_lambda_type."));
                }
            };
            Ok(TypedExpr::new(apply_type, ExprKind::FnApp(tfunc, targs)))
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
