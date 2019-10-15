use crate::common::{Expr, ExprKind, Type, TypeEnv};
use crate::type_checker::{tc_with_env, type_check};
use crate::util::concat_vectors;
use im_rc::{vector, Vector};
use std::sync::atomic::{AtomicU64, Ordering};

// "global variable" usage derived from https://stackoverflow.com/a/27826181
pub static GENSYM_COUNT: AtomicU64 = AtomicU64::new(0);

fn generate_env_name() -> String {
    let name = format!("env{}", GENSYM_COUNT.load(Ordering::SeqCst));
    GENSYM_COUNT.fetch_add(1, Ordering::SeqCst);
    name
}

fn generate_var_name() -> String {
    let name = format!("temp{}", GENSYM_COUNT.load(Ordering::SeqCst));
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
    // Closure convert the body, with knowledge of the types of the lambda's parameters
    let mut new_body = cc(body, &env.add_bindings(params.clone()))?;

    // Calculate the set of free variables in the lambda
    // which is the free variables in the body, minus the variables bound by the parameters
    let free_vars = get_free_vars_lambda(&params, &new_body)?;

    // Construct the environment name
    let env_name: String = generate_env_name();

    // Construct the environment
    // (x, Id(x)) (y, Id(y)) ...
    let env_contents: Vector<(String, Expr)> = free_vars
        .iter()
        .map(|var| (var.clone(), Expr::new(ExprKind::Id(var.clone()))))
        .collect();
    let new_env = Expr::new(ExprKind::Record(env_contents));

    // Substitute free variables in the body with references to the environment
    // ex. if y is free, replace it with (env-ref envX y)
    for var in &free_vars {
        new_body = substitute(
            &new_body,
            &var.clone(),
            &Expr::new(ExprKind::RecordGet(
                Box::from(Expr::new(ExprKind::Id(env_name.clone()))),
                var.clone(),
            )),
        )?;
    }

    let free_var_types: Vector<(String, Type)> = free_vars
        .iter()
        .cloned()
        .map(|var| {
            (
                var.clone(),
                env.find(&var)
                    .or_else(|| Some(&Type::Unknown))
                    .unwrap()
                    .clone(),
            )
        })
        .collect();

    // Construct new parameter list
    // Same as original parameter list, except an environment is appended to the beginning
    // ex. (lambda ((x : int) (y : int)) <body>)
    //  -> (lambda ((env : (record <free var types>)) (x : int) (y : int)) <body>)
    let mut new_params = params.clone();
    new_params.push_front((env_name.clone(), Type::Record(free_var_types.clone())));

    let new_ret_typ = tc_with_env(&new_body, &mut env.add_bindings(new_params.clone()))
        .map_err(|_| "Type checking on closure converted lambda body failed.")?;

    let new_lambda = Expr::new(ExprKind::Lambda(
        new_params,
        new_ret_typ,
        Box::from(new_body),
    ));
    Ok(Expr::new(ExprKind::Tuple(vector![new_lambda, new_env])))
}

fn cc_fn_app(
    func: &Expr,
    args: &Vector<Expr>,
    env: &TypeEnv<Type>,
) -> Result<Expr, ClosureConvertError> {
    match &func.kind {
        ExprKind::Lambda(_params, _ret_typ, _body) => {
            let cfunc = cc(&func, env)?;
            let mut cargs: Vector<Expr> = args
                .iter()
                .map(|arg| cc(&arg, env))
                .collect::<Result<Vector<Expr>, ClosureConvertError>>()?;
            let temp_var = generate_var_name();
            let temp_id = Expr::new(ExprKind::Id(temp_var.clone()));
            let get_func = Expr::new(ExprKind::TupleGet(Box::from(temp_id.clone()), 0));
            let get_env = Expr::new(ExprKind::TupleGet(Box::from(temp_id.clone()), 1));
            cargs.push_front(get_env);
            let new_body = Expr::new(ExprKind::FnApp(Box::from(get_func), cargs));
            Ok(Expr::new(ExprKind::Let(
                vector![(temp_var, cfunc)],
                Box::from(new_body),
            )))
        }
        _ => {
            let cfunc = cc(&func, env)?;
            let cargs: Vector<Expr> = args
                .iter()
                .map(|arg| cc(&arg, env))
                .collect::<Result<Vector<Expr>, ClosureConvertError>>()?;
            Ok(Expr::new(ExprKind::FnApp(Box::from(cfunc), cargs)))
        }
    }
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
            let bindings_sub: Vector<(String, Expr)> = bindings_sub?;
            substitute(&body, match_exp, replace_with)
                .and_then(|sbody| Ok(Expr::new(ExprKind::Let(bindings_sub, Box::from(sbody)))))
        }
        ExprKind::Lambda(params, ret_type, body) => {
            let param_names: Vector<String> = params.iter().map(|pair| pair.0.clone()).collect();
            if !param_names.contains(&String::from(match_exp)) {
                let sub_free_vars = get_free_vars(&replace_with)?;
                for param in param_names {
                    if sub_free_vars.contains(&param) {
                        return Err(ClosureConvertError::from("Tried to substitute an expression with free variables into a lambda which will result in said free variables getting captured!"));
                    }
                }
                let sbody = substitute(&body, match_exp, replace_with)?;
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
        ExprKind::Record(bindings) => {
            let cbindings = bindings
                .iter()
                .map(|pair| {
                    substitute(&pair.1, match_exp, replace_with)
                        .and_then(|sexp| Ok((pair.0.clone(), sexp)))
                })
                .collect::<Result<Vector<(String, Expr)>, ClosureConvertError>>()?;
            Ok(Expr::new(ExprKind::Record(cbindings)))
        }
        ExprKind::RecordGet(record, key) => {
            substitute(&record, match_exp, replace_with).and_then(|srecord| {
                Ok(Expr::new(ExprKind::RecordGet(
                    Box::from(srecord),
                    key.clone(),
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
        ExprKind::Tuple(vals) => substitute_array(&vals, match_exp, replace_with)
            .and_then(|svals| Ok(Expr::new(ExprKind::Tuple(svals)))),
        ExprKind::TupleGet(tuple, key) => substitute(&tuple, match_exp, replace_with)
            .and_then(|stuple| Ok(Expr::new(ExprKind::TupleGet(Box::from(stuple), *key)))),
        ExprKind::Pack(val, sub, exist) => {
            substitute(&val, match_exp, replace_with).and_then(|sval| {
                Ok(Expr::new(ExprKind::Pack(
                    Box::from(sval),
                    sub.clone(),
                    exist.clone(),
                )))
            })
        }
        ExprKind::Unpack(var, package, typ_sub, body) => {
            substitute(&package, match_exp, replace_with).and_then(|spackage| {
                substitute(&body, match_exp, replace_with).and_then(|sbody| {
                    Ok(Expr::new(ExprKind::Unpack(
                        var.clone(),
                        Box::from(spackage),
                        typ_sub.clone(),
                        Box::from(sbody),
                    )))
                })
            })
        }
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
            let binding_exps: Vector<Expr> = bindings.iter().map(|pair| pair.1.clone()).collect();
            let binding_vars: Vector<String> = bindings.iter().map(|pair| pair.0.clone()).collect();
            let mut body_vars = get_free_vars(&body)?;
            body_vars.retain(|var| !binding_vars.contains(var));
            Ok(concat_vectors(
                body_vars,
                get_free_vars_array(&binding_exps)?,
            ))
        }
        ExprKind::Lambda(params, _ret_type, body) => get_free_vars_lambda(&params, &body),
        ExprKind::FnApp(func, args) => {
            get_free_vars_array(&concat_vectors(vector![*func.clone()], args.clone()))
        }
        ExprKind::Record(bindings) => {
            get_free_vars_array(&bindings.iter().map(|pair| pair.1.clone()).collect())
        }
        ExprKind::RecordGet(record, _key) => get_free_vars(&record),
        ExprKind::Begin(exps) => get_free_vars_array(&exps),
        ExprKind::Set(_var, val) => get_free_vars(&val),
        ExprKind::Cons(first, second) => get_free_vars(&first).and_then(|vars1| {
            get_free_vars(&second).and_then(|vars2| Ok(concat_vectors(vars1, vars2)))
        }),
        ExprKind::Car(val) => get_free_vars(val.as_ref()),
        ExprKind::Cdr(val) => get_free_vars(val.as_ref()),
        ExprKind::Tuple(vals) => get_free_vars_array(&vals),
        ExprKind::TupleGet(tuple, _key) => get_free_vars(&tuple),
        ExprKind::Pack(val, _sub, _exist) => get_free_vars(&val),
        ExprKind::Unpack(var, _package, _typ_sub, body) => {
            let mut body_vars = get_free_vars(&body)?;
            body_vars.retain(|body_var| body_var != var);
            Ok(body_vars)
        }
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
    let mut free_vars: Vector<String> = get_free_vars(body)?;
    free_vars.retain(|var| !param_vars.contains(var));
    Ok(free_vars)
}

pub fn closure_convert(exp: &Expr) -> Result<Expr, ClosureConvertError> {
    cc(exp, &TypeEnv::new())
}

/// Q: Why is a type environment needed for closure conversion?
/// A: When closure converting lambdas, it is necessary to keep track of types
/// (especially as we try to closure convert lambdas within other lambdas) of
/// outer lambdas so that we can properly generate the right type signatures
/// of record environments (i.e. the "envX" which becomes the first argument
/// of all new lambdas).
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
            let binding_type_map = bindings
                .iter()
                .map(|pair| match type_check(&pair.1) {
                    Ok(exp_typ) => Ok((pair.0.clone(), exp_typ)),
                    Err(e) => Err(ClosureConvertError(format!(
                        "Type checking error during closure conversion: {}",
                        e
                    ))),
                })
                .collect::<Result<Vector<(String, Type)>, ClosureConvertError>>()?;
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
        ExprKind::Tuple(exps) => {
            let cexps_wrapped: Result<Vector<Expr>, ClosureConvertError> =
                exps.iter().map(|subexp| cc(&subexp, env)).collect();
            cexps_wrapped.and_then(|cexps| Ok(Expr::new(ExprKind::Tuple(cexps))))
        }
        ExprKind::TupleGet(tuple, key) => cc(&tuple, env)
            .and_then(|ctuple| Ok(Expr::new(ExprKind::TupleGet(Box::from(ctuple), *key)))),
        ExprKind::Record(bindings) => cc_bindings(&bindings, env)
            .and_then(|cbindings| Ok(Expr::new(ExprKind::Record(cbindings)))),
        ExprKind::RecordGet(record, key) => cc(&record, env).and_then(|crecord| {
            Ok(Expr::new(ExprKind::RecordGet(
                Box::from(crecord),
                key.clone(),
            )))
        }),
        ExprKind::Pack(val, sub, exist) => cc(&val, env).and_then(|cval| {
            Ok(Expr::new(ExprKind::Pack(
                Box::from(cval),
                sub.clone(),
                exist.clone(),
            )))
        }),
        ExprKind::Unpack(var, package, typ_sub, body) => cc(&package, env).and_then(|cpackage| {
            cc(&body, env).and_then(|cbody| {
                Ok(Expr::new(ExprKind::Unpack(
                    var.clone(),
                    Box::from(cpackage),
                    typ_sub.clone(),
                    Box::from(cbody),
                )))
            })
        }),
        ExprKind::FnApp(func, args) => cc_fn_app(func, args, env),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    #[test]
    fn test_substitute_lambdas() {
        // [x -> s]x = s
        let exp = parse(&lexpr::from_str("x").unwrap()).unwrap();
        let match_exp = "x";
        let replace_with = parse(&lexpr::from_str("s").unwrap()).unwrap();
        let expected = parse(&lexpr::from_str("s").unwrap()).unwrap();
        assert_eq!(
            substitute(&exp, match_exp, &replace_with).unwrap(),
            expected
        );

        // [x -> s]y = y  if y != x
        let exp = parse(&lexpr::from_str("y").unwrap()).unwrap();
        let match_exp = "x";
        let replace_with = parse(&lexpr::from_str("s").unwrap()).unwrap();
        let expected = parse(&lexpr::from_str("y").unwrap()).unwrap();
        assert_eq!(
            substitute(&exp, match_exp, &replace_with).unwrap(),
            expected
        );

        // [x -> s](lambda y . t) = (lambda y . [x -> s]t)  if y != x
        let exp = parse(&lexpr::from_str("(lambda ((y : int)) : int (+ x y))").unwrap()).unwrap();
        let match_exp = "x";
        let replace_with = parse(&lexpr::from_str("s").unwrap()).unwrap();
        let expected =
            parse(&lexpr::from_str("(lambda ((y : int)) : int (+ s y))").unwrap()).unwrap();
        assert_eq!(
            substitute(&exp, match_exp, &replace_with).unwrap(),
            expected
        );

        // [x -> s](lambda y . t) = (lambda y . t)  if y == x
        let exp = parse(&lexpr::from_str("(lambda ((x : int)) : int (+ x y))").unwrap()).unwrap();
        let match_exp = "x";
        let replace_with = parse(&lexpr::from_str("s").unwrap()).unwrap();
        let expected =
            parse(&lexpr::from_str("(lambda ((x : int)) : int (+ x y))").unwrap()).unwrap();
        assert_eq!(
            substitute(&exp, match_exp, &replace_with).unwrap(),
            expected
        );

        // [x -> y](lambda x . x) = (lambda x . x)
        let exp = parse(&lexpr::from_str("(lambda ((x : int)) : int x)").unwrap()).unwrap();
        let match_exp = "x";
        let replace_with = parse(&lexpr::from_str("y").unwrap()).unwrap();
        let expected = parse(&lexpr::from_str("(lambda ((x : int)) : int x)").unwrap()).unwrap();
        assert_eq!(
            substitute(&exp, match_exp, &replace_with).unwrap(),
            expected
        );

        // [x -> z](lambda z . x) = (lambda w . z)
        // We do not currently support this, so we will just assert that it generates an error.
        let exp = parse(&lexpr::from_str("(lambda ((z : int)) : int x)").unwrap()).unwrap();
        let match_exp = "x";
        let replace_with = parse(&lexpr::from_str("z").unwrap()).unwrap();
        let expected =
            parse(&lexpr::from_str("(lambda ((temp0 : int)) : int z)").unwrap()).unwrap();
        assert_eq!(substitute(&exp, match_exp, &replace_with).is_err(), true);

        // [x -> (lambda z . z w)](lambda y . x) = (lambda y . (lambda z. z w))
        // This fails because our substitute method does not change type annotations etc.
        // let exp = parse(&lexpr::from_str("(lambda ((y : int)) : int x)").unwrap()).unwrap();
        // let match_exp = "x";
        // let replace_with =
        //     parse(&lexpr::from_str("(lambda ((z : (-> int int))) : int (z w))").unwrap()).unwrap();
        // let expected = parse(
        //     &lexpr::from_str(
        //         "(lambda ((y : int)) : (-> (-> int int) int)
        //     (lambda ((z : (-> int int))) : int (z w)))",
        //     )
        //     .unwrap(),
        // )
        // .unwrap();
        // assert_eq!(
        //     substitute(&exp, match_exp, &replace_with).unwrap(),
        //     expected
        // );
    }
}
