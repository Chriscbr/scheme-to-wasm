use crate::common::{Expr, ExprKind, Type, TypeEnv};
use crate::type_checker::type_check;
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

fn generate_id() -> u64 {
    let val = GENSYM_COUNT.load(Ordering::SeqCst);
    GENSYM_COUNT.fetch_add(1, Ordering::SeqCst);
    val
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

fn cc_type(typ: &Type) -> Result<Type, ClosureConvertError> {
    match typ {
        Type::Int => Ok(Type::Int),
        Type::Bool => Ok(Type::Bool),
        Type::Str => Ok(Type::Str),
        Type::List(base_typ) => {
            let cc_base_typ = cc_type(base_typ)?;
            Ok(Type::List(Box::from(cc_base_typ)))
        }
        Type::Func(in_typs, ret_typ) => {
            let mut cc_in_typs = cc_type_array(in_typs)?;
            let cc_ret_typ = cc_type(ret_typ)?;
            let typ_var_id = generate_id();
            let typ_var = Type::TypeVar(typ_var_id);
            cc_in_typs.push_front(typ_var.clone());
            let base_typ = Type::Tuple(vector![
                Type::Func(cc_in_typs, Box::from(cc_ret_typ),),
                typ_var.clone()
            ]);
            Ok(Type::Exists(typ_var_id, Box::from(base_typ)))
        }
        Type::Tuple(typs) => {
            let cc_typs = cc_type_array(typs)?;
            Ok(Type::Tuple(cc_typs))
        }
        Type::Record(bindings) => {
            let cc_bindings = bindings
                .iter()
                .map(|pair| Ok((pair.0.clone(), cc_type(&pair.1)?)))
                .collect::<Result<Vector<(String, Type)>, ClosureConvertError>>()?;
            Ok(Type::Record(cc_bindings))
        }
        Type::Exists(typ_var, base_typ) => {
            let cc_base_typ = cc_type(base_typ)?;
            Ok(Type::Exists(*typ_var, Box::from(cc_base_typ)))
        }
        Type::TypeVar(x) => Ok(Type::TypeVar(*x)),
        Type::Unknown => Ok(Type::Unknown),
    }
}

fn cc_type_array(typs: &Vector<Type>) -> Result<Vector<Type>, ClosureConvertError> {
    typs.iter().map(|typ| Ok(cc_type(typ)?)).collect()
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
                Expr::new(ExprKind::Id(env_name.clone())),
                var.clone(),
            )),
        )?;
    }

    let free_var_types: Vector<(String, Type)> = free_vars
        .iter()
        .cloned()
        .map(|var| {
            Ok((
                var.clone(),
                cc_type(env.find(&var).ok_or_else(|| {
                    "No type found for free variable during closure conversion."
                })?)?,
            ))
        })
        .collect::<Result<Vector<(String, Type)>, ClosureConvertError>>()?;

    // Construct new parameter list
    // Same as original parameter list, except an environment is appended to the beginning
    // ex. (lambda ((x : int) (y : int)) <body>)
    //  -> (lambda ((env : (record <free var types>)) (x : int) (y : int)) <body>)
    // In addition, types are closure converted as needed
    // (ex. function types are replaced with existential types)
    let mut new_params = params
        .iter()
        .cloned()
        .map(|pair| Ok((pair.0.clone(), cc_type(&pair.1)?)))
        .collect::<Result<Vector<(String, Type)>, ClosureConvertError>>()?;
    let record_typ = Type::Record(free_var_types.clone());
    new_params.push_front((env_name.clone(), record_typ.clone()));

    let new_ret_typ = cc_type(&ret_type.clone())?;

    let new_lambda = Expr::new(ExprKind::Lambda(
        new_params.clone(),
        new_ret_typ.clone(),
        new_body,
    ));

    let orig_param_typs = params.clone().iter().map(|pair| pair.1.clone()).collect();
    let new_lambda_typ = cc_type(&Type::Func(orig_param_typs, Box::from(ret_type.clone())))?;

    let new_closure = Expr::new(ExprKind::Tuple(vector![new_lambda, new_env]));
    Ok(Expr::new(ExprKind::Pack(
        new_closure,
        record_typ.clone(),
        new_lambda_typ,
    )))
}

fn cc_fn_app(
    func: &Expr,
    args: &Vector<Expr>,
    env: &TypeEnv<Type>,
) -> Result<Expr, ClosureConvertError> {
    let tuple_name = generate_var_name();
    let tuple_name_id = Expr::new(ExprKind::Id(tuple_name.clone()));
    let package = cc(func, env)?;
    let typ_var = Type::TypeVar(generate_id());
    let tuple_func = Expr::new(ExprKind::TupleGet(tuple_name_id.clone(), 0));
    let tuple_env = Expr::new(ExprKind::TupleGet(tuple_name_id, 1));
    let cc_args = args
        .iter()
        .map(|arg| cc(arg, env))
        .collect::<Result<Vector<Expr>, ClosureConvertError>>()?;
    let new_args = concat_vectors(vector![tuple_env], cc_args);
    let body = Expr::new(ExprKind::FnApp(tuple_func, new_args));
    Ok(Expr::new(ExprKind::Unpack(
        tuple_name, package, typ_var, body,
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
    match &*exp.kind {
        ExprKind::Binop(op, arg1, arg2) => {
            substitute(&arg1, match_exp, replace_with).and_then(|sarg1| {
                substitute(&arg2, match_exp, replace_with)
                    .and_then(|sarg2| Ok(Expr::new(ExprKind::Binop(*op, sarg1, sarg2))))
            })
        }
        ExprKind::If(pred, cons, alt) => {
            substitute(&pred, match_exp, replace_with).and_then(|spred| {
                substitute(&cons, match_exp, replace_with).and_then(|scons| {
                    substitute(&alt, match_exp, replace_with)
                        .and_then(|salt| Ok(Expr::new(ExprKind::If(spred, scons, salt))))
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
                .and_then(|sbody| Ok(Expr::new(ExprKind::Let(bindings_sub, sbody))))
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
                    sbody,
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
                    .and_then(|sargs| Ok(Expr::new(ExprKind::FnApp(sfunc, sargs))))
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
        ExprKind::RecordGet(record, key) => substitute(&record, match_exp, replace_with)
            .and_then(|srecord| Ok(Expr::new(ExprKind::RecordGet(srecord, key.clone())))),
        ExprKind::Begin(exps) => substitute_array(&exps, match_exp, replace_with)
            .and_then(|sexps| Ok(Expr::new(ExprKind::Begin(sexps)))),
        ExprKind::Set(var, val) => substitute(&val, match_exp, replace_with)
            .and_then(|sval| Ok(Expr::new(ExprKind::Set(var.clone(), sval)))),
        ExprKind::Cons(first, second) => {
            substitute(&first, match_exp, replace_with).and_then(|sfirst| {
                substitute(&second, match_exp, replace_with)
                    .and_then(|ssecond| Ok(Expr::new(ExprKind::Cons(sfirst, ssecond))))
            })
        }
        ExprKind::Car(val) => substitute(&val, match_exp, replace_with)
            .and_then(|sval| Ok(Expr::new(ExprKind::Car(sval)))),
        ExprKind::Cdr(val) => substitute(&val, match_exp, replace_with)
            .and_then(|sval| Ok(Expr::new(ExprKind::Cdr(sval)))),
        ExprKind::Tuple(vals) => substitute_array(&vals, match_exp, replace_with)
            .and_then(|svals| Ok(Expr::new(ExprKind::Tuple(svals)))),
        ExprKind::TupleGet(tuple, key) => substitute(&tuple, match_exp, replace_with)
            .and_then(|stuple| Ok(Expr::new(ExprKind::TupleGet(stuple, *key)))),
        ExprKind::Pack(val, sub, exist) => substitute(&val, match_exp, replace_with)
            .and_then(|sval| Ok(Expr::new(ExprKind::Pack(sval, sub.clone(), exist.clone())))),
        ExprKind::Unpack(var, package, typ_sub, body) => {
            substitute(&package, match_exp, replace_with).and_then(|spackage| {
                substitute(&body, match_exp, replace_with).and_then(|sbody| {
                    Ok(Expr::new(ExprKind::Unpack(
                        var.clone(),
                        spackage,
                        typ_sub.clone(),
                        sbody,
                    )))
                })
            })
        }
        ExprKind::IsNull(val) => substitute(&val, match_exp, replace_with)
            .and_then(|sval| Ok(Expr::new(ExprKind::IsNull(sval)))),
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
    match &*exp.kind {
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
            get_free_vars_array(&concat_vectors(vector![func.clone()], args.clone()))
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
        ExprKind::Car(val) => get_free_vars(&val),
        ExprKind::Cdr(val) => get_free_vars(&val),
        ExprKind::Tuple(vals) => get_free_vars_array(&vals),
        ExprKind::TupleGet(tuple, _key) => get_free_vars(&tuple),
        ExprKind::Pack(val, _sub, _exist) => get_free_vars(&val),
        ExprKind::Unpack(var, package, _typ_sub, body) => {
            let mut free_vars = concat_vectors(get_free_vars(&package)?, get_free_vars(&body)?);
            free_vars.retain(|free_var| free_var != var);
            Ok(free_vars)
        }
        ExprKind::IsNull(val) => get_free_vars(&val),
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
    match &*exp.kind {
        ExprKind::Num(x) => Ok(Expr::new(ExprKind::Num(*x))),
        ExprKind::Bool(x) => Ok(Expr::new(ExprKind::Bool(*x))),
        ExprKind::Str(x) => Ok(Expr::new(ExprKind::Str(x.clone()))),
        ExprKind::Id(x) => Ok(Expr::new(ExprKind::Id(x.clone()))),
        ExprKind::Binop(op, arg1, arg2) => cc(&arg1, env).and_then(|carg1| {
            cc(&arg2, env).and_then(|carg2| Ok(Expr::new(ExprKind::Binop(*op, carg1, carg2))))
        }),
        ExprKind::If(pred, cons, alt) => cc(&pred, env).and_then(|cpred| {
            cc(&cons, env).and_then(|ccons| {
                cc(&alt, env).and_then(|calt| Ok(Expr::new(ExprKind::If(cpred, ccons, calt))))
            })
        }),
        ExprKind::Let(bindings, body) => {
            // We need a map of the types for the bindings to ensure that we can properly
            // closure convert the body of the let expression
            let cbindings = cc_bindings(&bindings, env)?;
            let binding_type_map = cbindings
                .iter()
                .map(|pair| match type_check(&pair.1) {
                    Ok(exp_typ) => Ok((pair.0.clone(), exp_typ)),
                    Err(e) => Err(ClosureConvertError(format!(
                        "Type checking error during closure conversion: {}",
                        e
                    ))),
                })
                .collect::<Result<Vector<(String, Type)>, ClosureConvertError>>()?;
            cc(&body, &env.add_bindings(binding_type_map))
                .and_then(|cbody| Ok(Expr::new(ExprKind::Let(cbindings, cbody))))
        }
        ExprKind::Lambda(params, ret_typ, body) => cc_lambda(&params, &ret_typ, &body, env),
        ExprKind::Begin(exps) => {
            let cexps_wrapped: Result<Vector<Expr>, ClosureConvertError> =
                exps.iter().map(|subexp| cc(&subexp, env)).collect();
            cexps_wrapped.and_then(|cexps| Ok(Expr::new(ExprKind::Begin(cexps))))
        }
        ExprKind::Set(id, val) => {
            cc(&val, env).and_then(|cval| Ok(Expr::new(ExprKind::Set(id.clone(), cval))))
        }
        ExprKind::Cons(first, rest) => cc(&first, env).and_then(|cfirst| {
            cc(&rest, env).and_then(|crest| Ok(Expr::new(ExprKind::Cons(cfirst, crest))))
        }),
        ExprKind::Car(val) => cc(&val, env).and_then(|cval| Ok(Expr::new(ExprKind::Car(cval)))),
        ExprKind::Cdr(val) => cc(&val, env).and_then(|cval| Ok(Expr::new(ExprKind::Cdr(cval)))),
        ExprKind::IsNull(val) => {
            cc(&val, env).and_then(|cval| Ok(Expr::new(ExprKind::IsNull(cval))))
        }
        ExprKind::Null(typ) => Ok(Expr::new(ExprKind::Null(cc_type(&typ)?))),
        ExprKind::Tuple(exps) => {
            let cexps_wrapped: Result<Vector<Expr>, ClosureConvertError> =
                exps.iter().map(|subexp| cc(&subexp, env)).collect();
            cexps_wrapped.and_then(|cexps| Ok(Expr::new(ExprKind::Tuple(cexps))))
        }
        ExprKind::TupleGet(tuple, key) => {
            cc(&tuple, env).and_then(|ctuple| Ok(Expr::new(ExprKind::TupleGet(ctuple, *key))))
        }
        ExprKind::Record(bindings) => cc_bindings(&bindings, env)
            .and_then(|cbindings| Ok(Expr::new(ExprKind::Record(cbindings)))),
        ExprKind::RecordGet(record, key) => cc(&record, env)
            .and_then(|crecord| Ok(Expr::new(ExprKind::RecordGet(crecord, key.clone())))),
        ExprKind::Pack(val, sub, exist) => Ok(Expr::new(ExprKind::Pack(
            cc(&val, env)?,
            cc_type(&sub)?,
            cc_type(&exist)?,
        ))),
        ExprKind::Unpack(var, package, typ_sub, body) => Ok(Expr::new(ExprKind::Unpack(
            var.clone(),
            cc(&package, env)?,
            cc_type(&typ_sub)?,
            cc(&body, env)?,
        ))),
        ExprKind::FnApp(func, args) => cc_fn_app(&func, &args, env),
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
        let _expected =
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
