use crate::parser::{BinOp, Expr, Type};
use im_rc::{vector, Vector};

/// Helper function that returns concatenation of two im_rc::Vector's
fn concat_vectors<T: Clone>(vec1: Vector<T>, vec2: Vector<T>) -> Vector<T> {
    let mut val = vec1.clone();
    val.append(vec2);
    val
}

use std::sync::atomic::{AtomicU64, Ordering};

static GENSYM_COUNT: AtomicU64 = AtomicU64::new(0);

// TODO: Depending on whether closure conversion has any legitimate "edge" cases,
// I think I might be able to simplify a lot of the code to no longer require
// returning Result enums
#[derive(Clone, Debug)]
pub struct ClosureConvertError(String);

impl From<&str> for ClosureConvertError {
    fn from(message: &str) -> Self {
        ClosureConvertError(String::from(message))
    }
}

impl std::fmt::Display for ClosureConvertError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "ClosureConvertError: {}", self.0)
    }
}

// allows other errors to wrap this one
// see https://doc.rust-lang.org/rust-by-example/error/multiple_error_types/define_error_type.html
impl std::error::Error for ClosureConvertError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        // Generic error, underlying cause isn't tracked.
        None
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum CType {
    Int,
    Bool,
    Str,
    List(Box<CType>),
    Func(Vector<CType>, Box<CType>), // array of input types, and a return type
    Env(Vector<CType>),
    Unknown,
}

#[derive(Clone, Debug, PartialEq)]
/// Represents a closure-converted expression.
pub enum CExpr {
    // operator, arg1, arg2
    Binop(BinOp, Box<CExpr>, Box<CExpr>),

    // predicate, consequent, alternate
    If(Box<CExpr>, Box<CExpr>, Box<CExpr>),

    // variable bindings, body
    Let(Vector<(String, CExpr)>, Box<CExpr>),

    // arg names/types (environment should be first argument), return type, body
    Lambda(Vector<(String, CType)>, CType, Box<CExpr>),

    // lambda, environment
    Closure(Box<CExpr>, Box<CExpr>),

    // func, arguments
    ClosureApp(Box<CExpr>, Vector<CExpr>),

    // environment mapping
    Env(Vector<(String, CExpr)>),

    // environment name, key
    EnvGet(String, String),

    Begin(Vector<CExpr>),
    Set(String, Box<CExpr>),
    Cons(Box<CExpr>, Box<CExpr>),
    Car(Box<CExpr>),
    Cdr(Box<CExpr>),
    IsNull(Box<CExpr>),
    Null(CType),

    Sym(String),
    Num(i64),
    Bool(bool),
    Str(String),
}

fn closure_convert_type(typ: &Type) -> Result<CType, ClosureConvertError> {
    match typ {
        Type::Int => Ok(CType::Int),
        Type::Bool => Ok(CType::Bool),
        Type::Str => Ok(CType::Str),
        Type::List(x) => {
            closure_convert_type(x).and_then(|ctype| Ok(CType::List(Box::from(ctype))))
        },
        Type::Func(arg_types, ret_type) => {
            let carg_types: Result<Vector<CType>, ClosureConvertError> = arg_types
                .iter()
                .map(|arg_typ| closure_convert_type(arg_typ))
                .collect();

            carg_types.and_then(|carg_types| {
                closure_convert_type(ret_type.as_ref())
                    .and_then(|cret_type| Ok(CType::Func(carg_types, Box::from(cret_type))))
            })
        },
    }
}

fn closure_convert_bindings(
    bindings: &Vector<(String, Expr)>,
) -> Result<Vector<(String, CExpr)>, ClosureConvertError> {
    bindings
        .iter()
        .map(|pair| closure_convert(&pair.1).and_then(|cexp| Ok((pair.0.clone(), cexp))))
        .collect()
}

fn closure_convert_lambda(
    params: &Vector<(String, Type)>,
    ret_type: &Type,
    body: &Expr,
) -> Result<CExpr, ClosureConvertError> {
    // Constructing the parameter list for the new lambda expression
    // Start with converting Type -> CType
    let new_params: Result<Vector<(String, CType)>, ClosureConvertError> = params
        .iter()
        .map(|pair| {
            closure_convert_type(&pair.1).and_then(|cparam_type| Ok((pair.0.clone(), cparam_type)))
        })
        .collect();
    // Unwrap result
    let mut new_params: Vector<(String, CType)> = match new_params {
        Ok(val) => val,
        Err(e) => return Err(e),
    };

    let cbody = match closure_convert(body) {
        Ok(val) => val,
        Err(e) => return Err(e),
    };

    let free_vars = match get_free_vars_lambda(&new_params, &cbody) {
        Ok(val) => val,
        Err(e) => return Err(e),
    };
    // TODO: get actual types of free variables somehow
    let free_var_types: Vector<CType> = free_vars.iter().map(|_x| CType::Unknown).collect();

    // Construct environment name
    let env_name: String = generate_env_name();

    // Add environment to beginning of parameter list
    new_params.push_front((env_name.clone(), CType::Env(free_var_types)));

    // (x, Sym(x)) (y, Sym(y)) ...
    let env_contents: Vector<(String, CExpr)> = free_vars
        .iter()
        .map(|var| (var.clone(), CExpr::Sym(var.clone())))
        .collect();
    let env_exp = CExpr::Env(env_contents);
    let mut new_body: CExpr = match closure_convert(body) {
        Ok(val) => val,
        Err(e) => return Err(e),
    };
    for var in free_vars {
        new_body = match substitute(
            &new_body,
            &CExpr::Sym(var.clone()),
            &CExpr::EnvGet(env_name.clone(), var.clone()),
        ) {
            Ok(val) => val,
            Err(e) => return Err(e),
        };
    }
    let cret_type: CType = match closure_convert_type(ret_type) {
        Ok(val) => val,
        Err(e) => return Err(e),
    };
    Ok(CExpr::Closure(
        Box::from(CExpr::Lambda(new_params, cret_type, Box::from(new_body))),
        Box::from(env_exp),
    ))
}

// TODO: Implement
fn substitute(
    exp: &CExpr,
    match_exp: &CExpr,
    replace_with: &CExpr,
) -> Result<CExpr, ClosureConvertError> {
    Err(ClosureConvertError::from("Unimplemented."))
}

// "global variable" usage derived from https://stackoverflow.com/a/27826181
fn generate_env_name() -> String {
    let name = String::from(format!("env{}", GENSYM_COUNT.load(Ordering::SeqCst)));
    GENSYM_COUNT.fetch_add(1, Ordering::SeqCst);
    name
}

fn get_free_vars_array(exps: &Vector<CExpr>) -> Result<Vector<String>, ClosureConvertError> {
    let var_vecs: Result<Vector<Vector<String>>, ClosureConvertError> =
        exps.iter().map(|val| get_free_vars(val)).collect();
    var_vecs.and_then(|vecs: Vector<Vector<String>>| {
        Ok(vecs
            .iter()
            .fold(vector![], |vec1, vec2| concat_vectors(vec1, vec2.clone())))
    })
}

fn get_free_vars(exp: &CExpr) -> Result<Vector<String>, ClosureConvertError> {
    match exp {
        CExpr::Binop(_op, arg1, arg2) => get_free_vars(arg1).and_then(|vars1| {
            get_free_vars(arg2).and_then(|vars2| Ok(concat_vectors(vars1, vars2)))
        }),
        CExpr::If(pred, cons, alt) => get_free_vars(pred).and_then(|vars1| {
            get_free_vars(cons).and_then(|vars2| {
                get_free_vars(alt)
                    .and_then(|vars3| Ok(concat_vectors(concat_vectors(vars1, vars2), vars3)))
            })
        }),
        CExpr::Let(bindings, body) => {
            let binding_vars: Vector<String> = bindings.iter().map(|pair| pair.0.clone()).collect();
            let mut body_vars = match get_free_vars(body) {
                Ok(val) => val,
                Err(e) => return Err(e),
            };
            body_vars.retain(|var| !binding_vars.contains(var));
            Ok(body_vars)
        },
        CExpr::Lambda(params, _ret_type, body) => get_free_vars_lambda(params, body),
        CExpr::Closure(lambda, env) => get_free_vars(lambda).and_then(|vars1| {
            get_free_vars(env).and_then(|vars2| Ok(concat_vectors(vars1, vars2)))
        }),
        CExpr::ClosureApp(func, args) => {
            get_free_vars_array(&concat_vectors(vector![*func.clone()], args.clone()))
        },
        CExpr::Env(bindings) => {
            get_free_vars_array(&bindings.iter().map(|pair| pair.1.clone()).collect())
        },
        CExpr::EnvGet(env_name, _var) => Ok(vector![env_name.clone()]),
        CExpr::Begin(exps) => get_free_vars_array(exps),
        CExpr::Set(_var, val) => get_free_vars(val),
        CExpr::Cons(first, second) => get_free_vars(first).and_then(|vars1| {
            get_free_vars(second).and_then(|vars2| Ok(concat_vectors(vars1, vars2)))
        }),
        CExpr::Car(val) => get_free_vars(val.as_ref()),
        CExpr::Cdr(val) => get_free_vars(val.as_ref()),
        CExpr::IsNull(val) => get_free_vars(val.as_ref()),
        CExpr::Null(_) => Ok(vector![]),
        CExpr::Sym(x) => Ok(vector![x.clone()]),
        CExpr::Num(_) => Ok(vector![]),
        CExpr::Bool(_) => Ok(vector![]),
        CExpr::Str(_) => Ok(vector![]),
    }
}

fn get_free_vars_lambda(
    params: &Vector<(String, CType)>,
    body: &CExpr,
) -> Result<Vector<String>, ClosureConvertError> {
    let param_vars: Vector<String> = params.iter().map(|pair| pair.0.clone()).collect();
    let mut free_vars: Vector<String> = match get_free_vars(body) {
        Ok(val) => val,
        Err(e) => return Err(e),
    };
    free_vars.retain(|var| !param_vars.contains(var));
    Ok(free_vars)
}

pub fn closure_convert(exp: &Expr) -> Result<CExpr, ClosureConvertError> {
    match exp {
        Expr::Num(x) => Ok(CExpr::Num(*x)),
        Expr::Bool(x) => Ok(CExpr::Bool(*x)),
        Expr::Str(x) => Ok(CExpr::Str(x.clone())),
        Expr::Sym(x) => Ok(CExpr::Sym(x.clone())),
        Expr::Binop(op, arg1, arg2) => closure_convert(arg1).and_then(|carg1| {
            closure_convert(arg2)
                .and_then(|carg2| Ok(CExpr::Binop(*op, Box::from(carg1), Box::from(carg2))))
        }),
        Expr::If(pred, cons, alt) => closure_convert(pred).and_then(|cpred| {
            closure_convert(cons).and_then(|ccons| {
                closure_convert(alt).and_then(|calt| {
                    Ok(CExpr::If(
                        Box::from(cpred),
                        Box::from(ccons),
                        Box::from(calt),
                    ))
                })
            })
        }),
        Expr::Let(bindings, body) => closure_convert_bindings(bindings).and_then(|cbindings| {
            closure_convert(body).and_then(|cbody| Ok(CExpr::Let(cbindings, Box::from(cbody))))
        }),
        Expr::Lambda(params, ret_typ, body) => closure_convert_lambda(params, ret_typ, body),
        Expr::Begin(exps) => {
            let cexps: Result<Vector<CExpr>, ClosureConvertError> = exps
                .iter()
                .map(|subexp| closure_convert(&subexp).and_then(|csubexp| Ok(csubexp)))
                .collect();
            cexps.and_then(|cexps| Ok(CExpr::Begin(cexps)))
        },
        Expr::Set(sym, val) => {
            return closure_convert(val)
                .and_then(|cval| Ok(CExpr::Set(sym.clone(), Box::from(cval))));
        },
        Expr::Cons(first, rest) => closure_convert(first).and_then(|cfirst| {
            closure_convert(rest)
                .and_then(|crest| Ok(CExpr::Cons(Box::from(cfirst), Box::from(crest))))
        }),
        Expr::Car(val) => closure_convert(val).and_then(|cval| Ok(CExpr::Car(Box::from(cval)))),
        Expr::Cdr(val) => closure_convert(val).and_then(|cval| Ok(CExpr::Cdr(Box::from(cval)))),
        Expr::IsNull(val) => {
            return closure_convert(val).and_then(|cval| Ok(CExpr::IsNull(Box::from(cval))));
        },
        Expr::Null(typ) => closure_convert_type(typ).and_then(|ctyp| Ok(CExpr::Null(ctyp))),
        // TODO: FnApp
        Expr::FnApp(func, args) => Err(ClosureConvertError::from("Unimplemented.")),
    }
}
