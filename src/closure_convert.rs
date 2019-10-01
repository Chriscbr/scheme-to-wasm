use crate::common::{BinOp, Expr, Type, TypeEnv};
use crate::type_checker::type_check;
use im_rc::{vector, Vector};
use std::sync::atomic::{AtomicU64, Ordering};

/// Helper function that returns concatenation of two im_rc::Vector's
fn concat_vectors<T: Clone>(vec1: Vector<T>, vec2: Vector<T>) -> Vector<T> {
    let mut val = vec1.clone();
    val.append(vec2);
    val
}

pub static GENSYM_COUNT: AtomicU64 = AtomicU64::new(0);

// "global variable" usage derived from https://stackoverflow.com/a/27826181
fn generate_env_name() -> String {
    let name = format!("env{}", GENSYM_COUNT.load(Ordering::SeqCst));
    GENSYM_COUNT.fetch_add(1, Ordering::SeqCst);
    name
}

/// Only use this for testing purposes!
pub fn dangerously_reset_gensym_count() {
    GENSYM_COUNT.store(0, Ordering::SeqCst);
}

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

    Id(String),
    Num(i64),
    Bool(bool),
    Str(String),
}

fn cc_type(typ: &Type) -> Result<CType, ClosureConvertError> {
    match typ {
        Type::Int => Ok(CType::Int),
        Type::Bool => Ok(CType::Bool),
        Type::Str => Ok(CType::Str),
        Type::List(x) => cc_type(x).and_then(|ctype| Ok(CType::List(Box::from(ctype)))),
        Type::Func(arg_types, ret_type) => {
            let carg_types: Result<Vector<CType>, ClosureConvertError> =
                arg_types.iter().map(|arg_typ| cc_type(arg_typ)).collect();

            carg_types.and_then(|carg_types| {
                cc_type(ret_type.as_ref())
                    .and_then(|cret_type| Ok(CType::Func(carg_types, Box::from(cret_type))))
            })
        }
        Type::Unknown => Ok(CType::Unknown),
    }
}

fn cc_bindings(
    bindings: &Vector<(String, Expr)>,
    env: &TypeEnv<CType>,
) -> Result<Vector<(String, CExpr)>, ClosureConvertError> {
    bindings
        .iter()
        .map(|pair| cc(&pair.1, env).and_then(|cexp| Ok((pair.0.clone(), cexp))))
        .collect()
}

fn cc_lambda(
    params: &Vector<(String, Type)>,
    ret_type: &Type,
    body: &Expr,
    env: &TypeEnv<CType>,
) -> Result<CExpr, ClosureConvertError> {
    // Constructing the parameter list for the new lambda expression
    // Start with converting Type -> CType
    let new_params: Result<Vector<(String, CType)>, ClosureConvertError> = params
        .iter()
        .map(|pair| cc_type(&pair.1).and_then(|cparam_type| Ok((pair.0.clone(), cparam_type))))
        .collect();
    // Unwrap result
    let mut new_params: Vector<(String, CType)> = match new_params {
        Ok(val) => val,
        Err(e) => return Err(e),
    };

    let cbody = match cc(body, &env.add_bindings(new_params.clone())) {
        Ok(val) => val,
        Err(e) => return Err(e),
    };

    let free_vars = match get_free_vars_lambda(&new_params, &cbody) {
        Ok(val) => val,
        Err(e) => return Err(e),
    };

    let free_var_types: Vector<CType> = match free_vars
        .iter()
        .map(|x| match env.find(x) {
            Some(val) => Ok(val.clone()),
            None => Err(ClosureConvertError::from(
                "Free variable found, environment cannot be constructed with proper type.",
            )),
        })
        .collect()
    {
        Ok(val) => val,
        Err(e) => return Err(e),
    };

    // Construct environment name
    let env_name: String = generate_env_name();

    // Add environment to beginning of parameter list
    new_params.push_front((env_name.clone(), CType::Env(free_var_types)));

    // (x, Id(x)) (y, Id(y)) ...
    let env_contents: Vector<(String, CExpr)> = free_vars
        .iter()
        .map(|var| (var.clone(), CExpr::Id(var.clone())))
        .collect();
    let env_exp = CExpr::Env(env_contents);
    let mut new_body: CExpr = match cc(body, env) {
        Ok(val) => val,
        Err(e) => return Err(e),
    };
    for var in free_vars {
        new_body = match substitute(
            &new_body,
            &var.clone(),
            &CExpr::EnvGet(env_name.clone(), var.clone()),
        ) {
            Ok(val) => val,
            Err(e) => return Err(e),
        };
    }
    let cret_type: CType = match cc_type(ret_type) {
        Ok(val) => val,
        Err(e) => return Err(e),
    };
    Ok(CExpr::Closure(
        Box::from(CExpr::Lambda(new_params, cret_type, Box::from(new_body))),
        Box::from(env_exp),
    ))
}

fn substitute_array(
    exps: &Vector<CExpr>,
    match_exp: &String,
    replace_with: &CExpr,
) -> Result<Vector<CExpr>, ClosureConvertError> {
    exps.iter()
        .map(|val| substitute(val, match_exp, replace_with))
        .collect()
}

fn substitute(
    exp: &CExpr,
    match_exp: &String,
    replace_with: &CExpr,
) -> Result<CExpr, ClosureConvertError> {
    match exp {
        CExpr::Binop(op, arg1, arg2) => {
            substitute(arg1, match_exp, replace_with).and_then(|sarg1| {
                substitute(arg2, match_exp, replace_with)
                    .and_then(|sarg2| Ok(CExpr::Binop(*op, Box::from(sarg1), Box::from(sarg2))))
            })
        }
        CExpr::If(pred, cons, alt) => substitute(pred, match_exp, replace_with).and_then(|spred| {
            substitute(cons, match_exp, replace_with).and_then(|scons| {
                substitute(alt, match_exp, replace_with).and_then(|salt| {
                    Ok(CExpr::If(
                        Box::from(spred),
                        Box::from(scons),
                        Box::from(salt),
                    ))
                })
            })
        }),
        CExpr::Let(bindings, body) => {
            let bindings_sub: Result<Vector<(String, CExpr)>, ClosureConvertError> = bindings
                .iter()
                .map(|pair| {
                    substitute(&pair.1, match_exp, replace_with)
                        .and_then(|sexp| Ok((pair.0.clone(), sexp)))
                })
                .collect();
            let bindings_sub: Vector<(String, CExpr)> = match bindings_sub {
                Ok(val) => val,
                Err(e) => return Err(e),
            };
            substitute(body, match_exp, replace_with)
                .and_then(|sbody| Ok(CExpr::Let(bindings_sub, Box::from(sbody))))
        }
        CExpr::Lambda(params, ret_type, body) => {
            let param_names: Vector<String> = params.iter().map(|pair| pair.0.clone()).collect();
            if !param_names.contains(match_exp) {
                let sbody = match substitute(body, match_exp, replace_with) {
                    Ok(val) => val,
                    Err(e) => return Err(e),
                };
                Ok(CExpr::Lambda(
                    params.clone(),
                    ret_type.clone(),
                    Box::from(sbody),
                ))
            } else {
                Ok(CExpr::Lambda(
                    params.clone(),
                    ret_type.clone(),
                    body.clone(),
                ))
            }
        }
        CExpr::Closure(lambda, env) => {
            substitute(lambda, match_exp, replace_with).and_then(|slambda| {
                substitute(env, match_exp, replace_with)
                    .and_then(|senv| Ok(CExpr::Closure(Box::from(slambda), Box::from(senv))))
            })
        }
        CExpr::ClosureApp(func, args) => {
            substitute(func, match_exp, replace_with).and_then(|sfunc| {
                substitute_array(args, match_exp, replace_with)
                    .and_then(|sargs| Ok(CExpr::ClosureApp(Box::from(sfunc), sargs)))
            })
        }
        CExpr::Env(bindings) => {
            match bindings
                .iter()
                .map(|pair| {
                    substitute(&pair.1, match_exp, replace_with)
                        .and_then(|sexp| Ok((pair.0.clone(), sexp)))
                })
                .collect()
            {
                Ok(val) => Ok(CExpr::Env(val)),
                Err(e) => Err(e),
            }
        }
        CExpr::EnvGet(_env_name, _var) => Ok(exp.clone()), // ?
        CExpr::Begin(exps) => substitute_array(exps, match_exp, replace_with)
            .and_then(|sexps| Ok(CExpr::Begin(sexps))),
        CExpr::Set(var, val) => substitute(val, match_exp, replace_with)
            .and_then(|sval| Ok(CExpr::Set(var.clone(), Box::from(sval)))),
        CExpr::Cons(first, second) => {
            substitute(first, match_exp, replace_with).and_then(|sfirst| {
                substitute(second, match_exp, replace_with)
                    .and_then(|ssecond| Ok(CExpr::Cons(Box::from(sfirst), Box::from(ssecond))))
            })
        }
        CExpr::Car(val) => substitute(val, match_exp, replace_with)
            .and_then(|sval| Ok(CExpr::Car(Box::from(sval)))),
        CExpr::Cdr(val) => substitute(val, match_exp, replace_with)
            .and_then(|sval| Ok(CExpr::Cdr(Box::from(sval)))),
        CExpr::IsNull(val) => substitute(val, match_exp, replace_with)
            .and_then(|sval| Ok(CExpr::IsNull(Box::from(sval)))),
        CExpr::Null(_) => Ok(exp.clone()),
        CExpr::Id(x) => {
            if x == match_exp {
                Ok(replace_with.clone())
            } else {
                Ok(CExpr::Id(x.clone()))
            }
        }
        CExpr::Num(_) => Ok(exp.clone()),
        CExpr::Bool(_) => Ok(exp.clone()),
        CExpr::Str(_) => Ok(exp.clone()),
    }
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
        }
        CExpr::Lambda(params, _ret_type, body) => get_free_vars_lambda(params, body),
        CExpr::Closure(lambda, env) => get_free_vars(lambda).and_then(|vars1| {
            get_free_vars(env).and_then(|vars2| Ok(concat_vectors(vars1, vars2)))
        }),
        CExpr::ClosureApp(func, args) => {
            get_free_vars_array(&concat_vectors(vector![*func.clone()], args.clone()))
        }
        CExpr::Env(bindings) => {
            get_free_vars_array(&bindings.iter().map(|pair| pair.1.clone()).collect())
        }
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
        CExpr::Id(x) => Ok(vector![x.clone()]),
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
    cc(exp, &TypeEnv::new())
}

fn cc(exp: &Expr, env: &TypeEnv<CType>) -> Result<CExpr, ClosureConvertError> {
    match exp {
        Expr::Num(x) => Ok(CExpr::Num(*x)),
        Expr::Bool(x) => Ok(CExpr::Bool(*x)),
        Expr::Str(x) => Ok(CExpr::Str(x.clone())),
        Expr::Id(x) => Ok(CExpr::Id(x.clone())),
        Expr::Binop(op, arg1, arg2) => cc(arg1, env).and_then(|carg1| {
            cc(arg2, env)
                .and_then(|carg2| Ok(CExpr::Binop(*op, Box::from(carg1), Box::from(carg2))))
        }),
        Expr::If(pred, cons, alt) => cc(pred, env).and_then(|cpred| {
            cc(cons, env).and_then(|ccons| {
                cc(alt, env).and_then(|calt| {
                    Ok(CExpr::If(
                        Box::from(cpred),
                        Box::from(ccons),
                        Box::from(calt),
                    ))
                })
            })
        }),
        Expr::Let(bindings, body) => {
            let binding_type_map: Vector<(String, CType)> = match bindings
                .iter()
                .map(|pair| match type_check(&pair.1) {
                    Ok(exp_typ) => {
                        cc_type(&exp_typ).and_then(|cexp_typ| Ok((pair.0.clone(), cexp_typ)))
                    }
                    Err(e) => Err(ClosureConvertError::from(
                        format!("Type checking error during closure conversion: {}", e).as_str(),
                    )),
                })
                .collect()
            {
                Ok(val) => val,
                Err(e) => return Err(e),
            };
            cc_bindings(bindings, env).and_then(|cbindings| {
                cc(body, &env.add_bindings(binding_type_map))
                    .and_then(|cbody| Ok(CExpr::Let(cbindings, Box::from(cbody))))
            })
        }
        Expr::Lambda(params, ret_typ, body) => cc_lambda(params, ret_typ, body, env),
        Expr::Begin(exps) => {
            let cexps: Result<Vector<CExpr>, ClosureConvertError> =
                exps.iter().map(|subexp| cc(&subexp, env)).collect();
            cexps.and_then(|cexps| Ok(CExpr::Begin(cexps)))
        }
        Expr::Set(sym, val) => {
            cc(val, env).and_then(|cval| Ok(CExpr::Set(sym.clone(), Box::from(cval))))
        }
        Expr::Cons(first, rest) => cc(first, env).and_then(|cfirst| {
            cc(rest, env).and_then(|crest| Ok(CExpr::Cons(Box::from(cfirst), Box::from(crest))))
        }),
        Expr::Car(val) => cc(val, env).and_then(|cval| Ok(CExpr::Car(Box::from(cval)))),
        Expr::Cdr(val) => cc(val, env).and_then(|cval| Ok(CExpr::Cdr(Box::from(cval)))),
        Expr::IsNull(val) => cc(val, env).and_then(|cval| Ok(CExpr::IsNull(Box::from(cval)))),
        Expr::Null(typ) => cc_type(typ).and_then(|ctyp| Ok(CExpr::Null(ctyp))),
        Expr::FnApp(func, args) => {
            let cargs: Vector<CExpr> = match args.iter().map(|arg| cc(&arg, env)).collect() {
                Ok(val) => val,
                Err(e) => return Err(e),
            };
            cc(func, env).and_then(|cfunc| Ok(CExpr::ClosureApp(Box::from(cfunc), cargs)))
        }
    }
}
