use crate::common::{Expr, ExprKind, Type, TypeEnv};
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

fn cc_bindings(
    bindings: &Vector<(String, Expr)>,
    env: &TypeEnv<Type>,
) -> Result<Vector<(String, Expr)>, ClosureConvertError> {
    bindings
        .iter()
        .map(|pair| cc(&pair.1, env).and_then(|cexp| Ok((pair.0.clone(), cexp))))
        .collect()
}

fn cc_lambda(
    params: &Vector<(String, Type)>,
    ret_type: &Type,
    body: &Expr,
    env: &TypeEnv<Type>,
) -> Result<Expr, ClosureConvertError> {
    let cbody = match cc(body, &env.add_bindings(params.clone())) {
        Ok(val) => val,
        Err(e) => return Err(e),
    };

    let free_vars = match get_free_vars_lambda(&params, &cbody) {
        Ok(val) => val,
        Err(e) => return Err(e),
    };

    // Construct environment name
    let env_name: String = generate_env_name();

    // Add environment to beginning of parameter list
    let mut new_params = params.clone();
    new_params.push_front((env_name.clone(), Type::Unknown));

    // (x, Id(x)) (y, Id(y)) ...
    let env_contents: Vector<(String, Expr)> = free_vars
        .iter()
        .map(|var| (var.clone(), Expr::new(ExprKind::Id(var.clone()))))
        .collect();
    let new_env = Expr::new(ExprKind::Env(env_contents));
    let mut new_body: Expr = match cc(body, env) {
        Ok(val) => val,
        Err(e) => return Err(e),
    };
    for var in free_vars {
        new_body = match substitute(
            &new_body,
            &var.clone(),
            &Expr::new(ExprKind::EnvGet(
                Box::from(Expr::new(ExprKind::Id(env_name.clone()))),
                var.clone(),
            )),
        ) {
            Ok(val) => val,
            Err(e) => return Err(e),
        };
    }

    let new_param_types: Vector<Type> = new_params.iter().map(|pair| pair.1.clone()).collect();
    let new_lambda = Expr::new(ExprKind::Lambda(
        new_params,
        ret_type.clone(),
        Box::from(new_body),
    ));
    let new_lambda_type = Type::Func(new_param_types, Box::from(ret_type.clone()));
    // TODO: implement type of environments
    Ok(Expr::new(ExprKind::Tuple(
        vector![new_lambda, new_env],
        vector![new_lambda_type, Type::Unknown],
    )))
}

fn substitute_array(
    exps: &Vector<Expr>,
    match_exp: &str,
    replace_with: &Expr,
) -> Result<Vector<Expr>, ClosureConvertError> {
    exps.iter()
        .map(|val| substitute(val, match_exp, replace_with))
        .collect()
}

fn substitute(
    exp: &Expr,
    match_exp: &str,
    replace_with: &Expr,
) -> Result<Expr, ClosureConvertError> {
    match &exp.kind {
        ExprKind::Binop(op, arg1, arg2) => {
            substitute(&arg1, match_exp, replace_with).and_then(|sarg1| {
                substitute(&arg2, match_exp, replace_with).and_then(|sarg2| {
                    Ok(Expr::new(ExprKind::Binop(
                        *op,
                        Box::from(sarg1),
                        Box::from(sarg2),
                    )))
                })
            })
        }
        ExprKind::If(pred, cons, alt) => {
            substitute(&pred, match_exp, replace_with).and_then(|spred| {
                substitute(&cons, match_exp, replace_with).and_then(|scons| {
                    substitute(&alt, match_exp, replace_with).and_then(|salt| {
                        Ok(Expr::new(ExprKind::If(
                            Box::from(spred),
                            Box::from(scons),
                            Box::from(salt),
                        )))
                    })
                })
            })
        }
        ExprKind::Let(bindings, body) => {
            let bindings_sub: Result<Vector<(String, Expr)>, ClosureConvertError> = bindings
                .iter()
                .map(|pair| {
                    substitute(&pair.1, match_exp, replace_with)
                        .and_then(|sexp| Ok((pair.0.clone(), sexp)))
                })
                .collect();
            let bindings_sub: Vector<(String, Expr)> = match bindings_sub {
                Ok(val) => val,
                Err(e) => return Err(e),
            };
            substitute(&body, match_exp, replace_with)
                .and_then(|sbody| Ok(Expr::new(ExprKind::Let(bindings_sub, Box::from(sbody)))))
        }
        ExprKind::Lambda(params, ret_type, body) => {
            let param_names: Vector<String> = params.iter().map(|pair| pair.0.clone()).collect();
            if !param_names.contains(&String::from(match_exp)) {
                let sbody = match substitute(&body, match_exp, replace_with) {
                    Ok(val) => val,
                    Err(e) => return Err(e),
                };
                Ok(Expr::new(ExprKind::Lambda(
                    params.clone(),
                    ret_type.clone(),
                    Box::from(sbody),
                )))
            } else {
                Ok(Expr::new(ExprKind::Lambda(
                    params.clone(),
                    ret_type.clone(),
                    body.clone(),
                )))
            }
        }
        ExprKind::FnApp(func, args) => {
            substitute(&func, match_exp, replace_with).and_then(|sfunc| {
                substitute_array(&args, match_exp, replace_with)
                    .and_then(|sargs| Ok(Expr::new(ExprKind::FnApp(Box::from(sfunc), sargs))))
            })
        }
        ExprKind::Env(bindings) => {
            match bindings
                .iter()
                .map(|pair| {
                    substitute(&pair.1, match_exp, replace_with)
                        .and_then(|sexp| Ok((pair.0.clone(), sexp)))
                })
                .collect()
            {
                Ok(val) => Ok(Expr::new(ExprKind::Env(val))),
                Err(e) => Err(e),
            }
        }
        ExprKind::EnvGet(clos_env, var) => {
            substitute(&clos_env, match_exp, replace_with).and_then(|sclos_env| {
                Ok(Expr::new(ExprKind::EnvGet(
                    Box::from(sclos_env),
                    var.clone(),
                )))
            })
        }
        ExprKind::Begin(exps) => substitute_array(&exps, match_exp, replace_with)
            .and_then(|sexps| Ok(Expr::new(ExprKind::Begin(sexps)))),
        ExprKind::Set(var, val) => substitute(&val, match_exp, replace_with)
            .and_then(|sval| Ok(Expr::new(ExprKind::Set(var.clone(), Box::from(sval))))),
        ExprKind::Cons(first, second) => {
            substitute(&first, match_exp, replace_with).and_then(|sfirst| {
                substitute(&second, match_exp, replace_with).and_then(|ssecond| {
                    Ok(Expr::new(ExprKind::Cons(
                        Box::from(sfirst),
                        Box::from(ssecond),
                    )))
                })
            })
        }
        ExprKind::Car(val) => substitute(&val, match_exp, replace_with)
            .and_then(|sval| Ok(Expr::new(ExprKind::Car(Box::from(sval))))),
        ExprKind::Cdr(val) => substitute(&val, match_exp, replace_with)
            .and_then(|sval| Ok(Expr::new(ExprKind::Cdr(Box::from(sval))))),
        ExprKind::Tuple(vals, typs) => substitute_array(&vals, match_exp, replace_with)
            .and_then(|svals| Ok(Expr::new(ExprKind::Tuple(svals, typs.clone())))),
        ExprKind::TupleGet(tuple, key) => {
            substitute(&tuple, match_exp, replace_with).and_then(|stuple| {
                Ok(Expr::new(ExprKind::TupleGet(
                    Box::from(stuple),
                    key.clone(),
                )))
            })
        }
        ExprKind::Pack(val, sub, exist) => unimplemented!(),
        ExprKind::IsNull(val) => substitute(&val, match_exp, replace_with)
            .and_then(|sval| Ok(Expr::new(ExprKind::IsNull(Box::from(sval))))),
        ExprKind::Null(_) => Ok(exp.clone()),
        ExprKind::Id(x) => {
            if x == match_exp {
                Ok(replace_with.clone())
            } else {
                Ok(Expr::new(ExprKind::Id(x.clone())))
            }
        }
        ExprKind::Num(_) => Ok(exp.clone()),
        ExprKind::Bool(_) => Ok(exp.clone()),
        ExprKind::Str(_) => Ok(exp.clone()),
    }
}

fn get_free_vars_array(exps: &Vector<Expr>) -> Result<Vector<String>, ClosureConvertError> {
    let var_vecs: Result<Vector<Vector<String>>, ClosureConvertError> =
        exps.iter().map(|val| get_free_vars(val)).collect();
    var_vecs.and_then(|vecs: Vector<Vector<String>>| {
        Ok(vecs
            .iter()
            .fold(vector![], |vec1, vec2| concat_vectors(vec1, vec2.clone())))
    })
}

fn get_free_vars(exp: &Expr) -> Result<Vector<String>, ClosureConvertError> {
    match &exp.kind {
        ExprKind::Binop(_op, arg1, arg2) => get_free_vars(&arg1).and_then(|vars1| {
            get_free_vars(&arg2).and_then(|vars2| Ok(concat_vectors(vars1, vars2)))
        }),
        ExprKind::If(pred, cons, alt) => get_free_vars(&pred).and_then(|vars1| {
            get_free_vars(&cons).and_then(|vars2| {
                get_free_vars(&alt)
                    .and_then(|vars3| Ok(concat_vectors(concat_vectors(vars1, vars2), vars3)))
            })
        }),
        ExprKind::Let(bindings, body) => {
            let binding_vars: Vector<String> = bindings.iter().map(|pair| pair.0.clone()).collect();
            let mut body_vars = match get_free_vars(&body) {
                Ok(val) => val,
                Err(e) => return Err(e),
            };
            body_vars.retain(|var| !binding_vars.contains(var));
            Ok(body_vars)
        }
        ExprKind::Lambda(params, _ret_type, body) => get_free_vars_lambda(&params, &body),
        ExprKind::FnApp(func, args) => {
            get_free_vars_array(&concat_vectors(vector![*func.clone()], args.clone()))
        }
        ExprKind::Env(bindings) => {
            get_free_vars_array(&bindings.iter().map(|pair| pair.1.clone()).collect())
        }
        ExprKind::EnvGet(env, _var) => get_free_vars(&env),
        ExprKind::Begin(exps) => get_free_vars_array(&exps),
        ExprKind::Set(_var, val) => get_free_vars(&val),
        ExprKind::Cons(first, second) => get_free_vars(&first).and_then(|vars1| {
            get_free_vars(&second).and_then(|vars2| Ok(concat_vectors(vars1, vars2)))
        }),
        ExprKind::Car(val) => get_free_vars(val.as_ref()),
        ExprKind::Cdr(val) => get_free_vars(val.as_ref()),
        ExprKind::Tuple(vals, _typs) => get_free_vars_array(&vals),
        ExprKind::TupleGet(tuple, _key) => get_free_vars(&tuple),
        ExprKind::Pack(val, sub, exist) => unimplemented!(),
        ExprKind::IsNull(val) => get_free_vars(val.as_ref()),
        ExprKind::Null(_) => Ok(vector![]),
        ExprKind::Id(x) => Ok(vector![x.clone()]),
        ExprKind::Num(_) => Ok(vector![]),
        ExprKind::Bool(_) => Ok(vector![]),
        ExprKind::Str(_) => Ok(vector![]),
    }
}

fn get_free_vars_lambda(
    params: &Vector<(String, Type)>,
    body: &Expr,
) -> Result<Vector<String>, ClosureConvertError> {
    let param_vars: Vector<String> = params.iter().map(|pair| pair.0.clone()).collect();
    let mut free_vars: Vector<String> = match get_free_vars(body) {
        Ok(val) => val,
        Err(e) => return Err(e),
    };
    free_vars.retain(|var| !param_vars.contains(var));
    Ok(free_vars)
}

pub fn closure_convert(exp: &Expr) -> Result<Expr, ClosureConvertError> {
    cc(exp, &TypeEnv::new())
}

fn cc(exp: &Expr, env: &TypeEnv<Type>) -> Result<Expr, ClosureConvertError> {
    match &exp.kind {
        ExprKind::Num(x) => Ok(Expr::new(ExprKind::Num(*x))),
        ExprKind::Bool(x) => Ok(Expr::new(ExprKind::Bool(*x))),
        ExprKind::Str(x) => Ok(Expr::new(ExprKind::Str(x.clone()))),
        ExprKind::Id(x) => Ok(Expr::new(ExprKind::Id(x.clone()))),
        ExprKind::Binop(op, arg1, arg2) => cc(&arg1, env).and_then(|carg1| {
            cc(&arg2, env).and_then(|carg2| {
                Ok(Expr::new(ExprKind::Binop(
                    *op,
                    Box::from(carg1),
                    Box::from(carg2),
                )))
            })
        }),
        ExprKind::If(pred, cons, alt) => cc(&pred, env).and_then(|cpred| {
            cc(&cons, env).and_then(|ccons| {
                cc(&alt, env).and_then(|calt| {
                    Ok(Expr::new(ExprKind::If(
                        Box::from(cpred),
                        Box::from(ccons),
                        Box::from(calt),
                    )))
                })
            })
        }),
        ExprKind::Let(bindings, body) => {
            let binding_type_map: Vector<(String, Type)> = match bindings
                .iter()
                .map(|pair| match type_check(&pair.1) {
                    Ok(exp_typ) => Ok((pair.0.clone(), exp_typ)),
                    Err(e) => Err(ClosureConvertError::from(
                        format!("Type checking error during closure conversion: {}", e).as_str(),
                    )),
                })
                .collect()
            {
                Ok(val) => val,
                Err(e) => return Err(e),
            };
            cc_bindings(&bindings, env).and_then(|cbindings| {
                cc(&body, &env.add_bindings(binding_type_map))
                    .and_then(|cbody| Ok(Expr::new(ExprKind::Let(cbindings, Box::from(cbody)))))
            })
        }
        ExprKind::Lambda(params, ret_typ, body) => cc_lambda(&params, &ret_typ, &body, env),
        ExprKind::Begin(exps) => {
            let cexps_wrapped: Result<Vector<Expr>, ClosureConvertError> =
                exps.iter().map(|subexp| cc(&subexp, env)).collect();
            cexps_wrapped.and_then(|cexps| Ok(Expr::new(ExprKind::Begin(cexps))))
        }
        ExprKind::Set(id, val) => {
            cc(&val, env).and_then(|cval| Ok(Expr::new(ExprKind::Set(id.clone(), Box::from(cval)))))
        }
        ExprKind::Cons(first, rest) => cc(&first, env).and_then(|cfirst| {
            cc(&rest, env).and_then(|crest| {
                Ok(Expr::new(ExprKind::Cons(
                    Box::from(cfirst),
                    Box::from(crest),
                )))
            })
        }),
        ExprKind::Car(val) => {
            cc(&val, env).and_then(|cval| Ok(Expr::new(ExprKind::Car(Box::from(cval)))))
        }
        ExprKind::Cdr(val) => {
            cc(&val, env).and_then(|cval| Ok(Expr::new(ExprKind::Cdr(Box::from(cval)))))
        }
        ExprKind::IsNull(val) => {
            cc(&val, env).and_then(|cval| Ok(Expr::new(ExprKind::IsNull(Box::from(cval)))))
        }
        ExprKind::Null(typ) => Ok(Expr::new(ExprKind::Null(typ.clone()))),
        ExprKind::Tuple(exps, typs) => {
            let cexps_wrapped: Result<Vector<Expr>, ClosureConvertError> =
                exps.iter().map(|subexp| cc(&subexp, env)).collect();
            cexps_wrapped.and_then(|cexps| Ok(Expr::new(ExprKind::Tuple(cexps, typs.clone()))))
        }
        ExprKind::TupleGet(tuple, key) => cc(&tuple, env).and_then(|ctuple| {
            Ok(Expr::new(ExprKind::TupleGet(
                Box::from(ctuple),
                key.clone(),
            )))
        }),
        ExprKind::Env(bindings) => cc_bindings(&bindings, env)
            .and_then(|cbindings| Ok(Expr::new(ExprKind::Env(cbindings)))),
        ExprKind::EnvGet(clos_env, key) => cc(&clos_env, env).and_then(|cclos_env| {
            Ok(Expr::new(ExprKind::EnvGet(
                Box::from(cclos_env),
                key.clone(),
            )))
        }),
        ExprKind::Pack(val, sub, exist) => unimplemented!(),
        ExprKind::FnApp(func, args) => {
            let cargs: Vector<Expr> = match args.iter().map(|arg| cc(&arg, env)).collect() {
                Ok(val) => val,
                Err(e) => return Err(e),
            };
            cc(&func, env).and_then(|cfunc| Ok(Expr::new(ExprKind::FnApp(Box::from(cfunc), cargs))))
        }
    }
}
