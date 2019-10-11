use crate::common::{type_contains_var, BinOp, Expr, ExprKind, Type, TypeEnv};
use im_rc::Vector;

#[derive(Clone, Debug)]
pub struct TypeCheckError(String);

impl From<&str> for TypeCheckError {
    fn from(message: &str) -> Self {
        TypeCheckError(String::from(message))
    }
}

impl std::fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "TypeCheckError: {}", self.0)
    }
}

// allows other errors to wrap this one
// see https://doc.rust-lang.org/rust-by-example/error/multiple_error_types/define_error_type.html
impl std::error::Error for TypeCheckError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        // Generic error, underlying cause isn't tracked.
        None
    }
}

//
// Helper functions
//

fn check_lambda_type_with_inputs(
    fn_type: &Type,
    param_types: &Vector<Type>,
) -> Result<Type, TypeCheckError> {
    match fn_type {
        Type::Func(arg_types, ret_type_boxed) => {
            let ret_type = ret_type_boxed.as_ref();
            if *arg_types == *param_types {
                Ok((*ret_type).clone())
            } else {
                Err(TypeCheckError::from(
                    "Argument types and parameter types of function application do not match.",
                ))
            }
        }
        _ => Err(TypeCheckError::from("Expected a function type.")),
    }
}

//
// Type checking functions
//

fn tc_binop_with_env(
    op: BinOp,
    arg1: &Expr,
    arg2: &Expr,
    env: &mut TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    let arg1_expect_typ: Type;
    let arg2_expect_typ: Type;
    let ret_typ: Type;
    match op {
        BinOp::Add | BinOp::Subtract | BinOp::Multiply | BinOp::Divide => {
            arg1_expect_typ = Type::Int;
            arg2_expect_typ = Type::Int;
            ret_typ = Type::Int;
        }
        BinOp::LessThan
        | BinOp::GreaterThan
        | BinOp::LessOrEqual
        | BinOp::GreaterOrEqual
        | BinOp::EqualTo => {
            arg1_expect_typ = Type::Int;
            arg2_expect_typ = Type::Int;
            ret_typ = Type::Bool;
        }
        BinOp::And | BinOp::Or => {
            arg1_expect_typ = Type::Bool;
            arg2_expect_typ = Type::Bool;
            ret_typ = Type::Bool;
        }
        BinOp::Concat => {
            arg1_expect_typ = Type::Str;
            arg2_expect_typ = Type::Str;
            ret_typ = Type::Str;
        }
    }
    tc_with_env(arg1, env).and_then(|arg1_typ| {
        tc_with_env(arg2, env).and_then(|arg2_typ| {
            if arg1_expect_typ != arg1_typ || arg2_expect_typ != arg2_typ {
                Err(TypeCheckError::from(
                    "Binary operation parameters do not match expected types.",
                ))
            } else {
                Ok(ret_typ)
            }
        })
    })
}

fn tc_if_with_env(
    predicate: &Expr,
    consequent: &Expr,
    alternate: &Expr,
    env: &mut TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    tc_with_env(predicate, env).and_then(|pred| {
        tc_with_env(consequent, env).and_then(|cons| {
            tc_with_env(alternate, env).and_then(|alt| {
                if pred != Type::Bool {
                    Err(TypeCheckError::from(
                        "Predicate in if expression does not evaluate to a boolean value.",
                    ))
                } else if cons != alt {
                    Err(TypeCheckError::from(
                        "Consequent and alternate values in if expression do not match types.",
                    ))
                } else {
                    Ok(cons)
                }
            })
        })
    })
}

fn tc_let_with_env(
    bindings: &Vector<(String, Expr)>,
    body: &Expr,
    env: &mut TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    let type_bindings: Result<Vector<(String, Type)>, TypeCheckError> = bindings
        .iter()
        .map(|pair| match tc_with_env(&pair.1, env) {
            Ok(typ) => Ok((pair.0.clone(), typ)),
            Err(e) => Err(e),
        })
        .collect();

    match type_bindings {
        Ok(val) => {
            // there ought to be an easier way to convert an im::vector into a slice
            let mut new_env = env.add_bindings(val);
            tc_with_env(body, &mut new_env)
        }
        Err(e) => Err(e),
    }
}

fn tc_lambda_with_env(
    params: &Vector<(String, Type)>,
    ret_typ: &Type,
    body: &Expr,
    env: &mut TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    // add arg types to the type environment for use in the body
    let mut new_env = env.add_bindings(params.clone());

    // type check lambda body
    tc_with_env(body, &mut new_env).and_then(|body_typ| {
        if *ret_typ == body_typ {
            let param_types: Vector<Type> = params.iter().map(|pair| pair.1.clone()).collect();
            Ok(Type::Func(param_types, Box::from(ret_typ.clone())))
        } else {
            Err(TypeCheckError::from(
                "Lambda expression body type does not match the expected return type.",
            ))
        }
    })
}

fn tc_begin_with_env(exps: &Vector<Expr>, env: &mut TypeEnv<Type>) -> Result<Type, TypeCheckError> {
    // Note: it's important that even though we only return the type of the
    // last expression within the 'begin' S-expression, we still want to
    // type-check the entire array in case any type errors pop up
    match tc_array_with_env(exps, env) {
        // an alternative for picking out the last element from exps is:
        // Ok(types) => Ok(types[types.len() - 1].clone()),
        Ok(mut types) => Ok(types.remove(types.len() - 1)),
        Err(e) => Err(e),
    }
}

// set! returns the value that is being assigned, since the language has no unit type
fn tc_set_bang_with_env(
    symbol: &str,
    exp: &Expr,
    env: &mut TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    let expected_type = match env.find(symbol) {
        Some(typ) => typ.clone(),
        None => {
            return Err(TypeCheckError::from(
                "Variable assignment cannot occur before it has been defined!",
            ))
        }
    };
    match tc_with_env(exp, env) {
        Ok(typ) => {
            if typ == expected_type {
                Ok(typ)
            } else {
                Err(TypeCheckError::from(
                    "Type of set! body does not match variable's initialized type.",
                ))
            }
        }
        Err(e) => Err(e),
    }
}

fn tc_cons_with_env(
    first: &Expr,
    rest: &Expr,
    env: &mut TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    let car_type = match tc_with_env(first, env) {
        Ok(typ) => typ,
        Err(e) => return Err(e),
    };
    let cdr_type = match tc_with_env(rest, env) {
        Ok(typ) => typ,
        Err(e) => return Err(e),
    };
    match cdr_type {
        Type::List(boxed_type) => {
            if *boxed_type == car_type {
                Ok(Type::List(boxed_type))
            } else {
                Err(TypeCheckError::from(
                    "Car of cons does not match type of cdr.",
                ))
            }
        }
        _ => Err(TypeCheckError::from(
            "Cdr of cons expression is not a list type.",
        )),
    }
}

fn tc_car_with_env(exp: &Expr, env: &mut TypeEnv<Type>) -> Result<Type, TypeCheckError> {
    match tc_with_env(exp, env) {
        Ok(lst_type) => match lst_type {
            Type::List(boxed_type) => Ok(*boxed_type),
            _ => Err(TypeCheckError::from(
                "Expression in car is not a list type.",
            )),
        },
        Err(e) => Err(e),
    }
}

fn tc_cdr_with_env(exp: &Expr, env: &mut TypeEnv<Type>) -> Result<Type, TypeCheckError> {
    match tc_with_env(exp, env) {
        Ok(lst_type) => match lst_type {
            Type::List(boxed_type) => Ok(Type::List(boxed_type)),
            _ => Err(TypeCheckError::from(
                "Expression in cdr is not a list type.",
            )),
        },
        Err(e) => Err(e),
    }
}

fn tc_tuple_with_env(exps: &Vector<Expr>, env: &mut TypeEnv<Type>) -> Result<Type, TypeCheckError> {
    tc_array_with_env(exps, env).and_then(|typs| Ok(Type::Tuple(typs)))
}

fn tc_tuple_get_with_env(
    tup: &Expr,
    key: &Expr,
    env: &mut TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    tc_with_env(tup, env).and_then(|tup_typ| match tup_typ {
        Type::Tuple(vec) => match key.kind {
            ExprKind::Num(x) => {
                if (x as usize) < vec.len() {
                    Ok(vec[x as usize].clone())
                } else {
                    Err(TypeCheckError::from(
                        "Value in tuple-ref is too large for the provided tuple.",
                    ))
                }
            }
            _ => Err(TypeCheckError::from(
                "Second expression in tuple-ref is not a number.",
            )),
        },
        _ => Err(TypeCheckError::from(
            "First expression in tuple-ref is not a tuple.",
        )),
    })
}

fn tc_record_with_env(
    bindings: &Vector<(String, Expr)>,
    env: &mut TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    let binding_types = bindings
        .iter()
        .map(|pair| match tc_with_env(&pair.1, env) {
            Ok(val) => Ok((pair.0.clone(), val)),
            Err(e) => Err(e),
        })
        .collect();
    match binding_types {
        Ok(val) => Ok(Type::Record(val)),
        Err(e) => Err(e),
    }
}

fn tc_record_get_with_env(
    record: &Expr,
    key: &str,
    env: &mut TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    tc_with_env(record, env).and_then(|record_typ| match record_typ {
        Type::Record(bindings) => {
            let matches: Vector<(String, Type)> = bindings
                .iter()
                .cloned()
                .filter(|pair| pair.0 == *key)
                .collect();
            if matches.is_empty() {
                return Err(TypeCheckError::from(
                    "Key in record-get not found in record.",
                ));
            }
            Ok(matches[0].1.clone())
        }
        _ => Err(TypeCheckError::from(
            "First expression in record-ref is not a record.",
        )),
    })
}

fn tc_apply_with_env(
    func: &Expr,
    args: &Vector<Expr>,
    env: &mut TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    match tc_with_env(func, env) {
        Ok(typ) => {
            let param_types = match tc_array_with_env(&args, env) {
                Ok(val) => val,
                Err(e) => return Err(e),
            };
            check_lambda_type_with_inputs(&typ, &param_types)
        }
        Err(e) => Err(e),
    }
}

// This always returns Type::Bool, but we still need to type check the inside.
fn tc_is_null_with_env(exp: &Expr, env: &mut TypeEnv<Type>) -> Result<Type, TypeCheckError> {
    match tc_with_env(exp, env) {
        Ok(_) => Ok(Type::Bool),
        Err(e) => Err(e),
    }
}

fn type_substitute(typ: &Type, type_var: u64, replace_with: &Type) -> Result<Type, TypeCheckError> {
    match typ {
        Type::Int => Ok(Type::Int),
        Type::Bool => Ok(Type::Bool),
        Type::Str => Ok(Type::Str),
        Type::List(inner_typ) => type_substitute(inner_typ, type_var, replace_with)
            .and_then(|styp| Ok(Type::List(Box::from(styp)))),
        Type::Func(in_typs, ret_typ) => {
            let in_typs_vec: Result<Vector<Type>, TypeCheckError> = in_typs
                .iter()
                .map(|inner_typ| type_substitute(inner_typ, type_var, replace_with))
                .collect();
            in_typs_vec.and_then(|sin_typs| {
                type_substitute(ret_typ, type_var, replace_with)
                    .and_then(|sret_typ| Ok(Type::Func(sin_typs, Box::from(sret_typ))))
            })
        }
        Type::Tuple(typs) => {
            let typs_vec: Result<Vector<Type>, TypeCheckError> = typs
                .iter()
                .map(|inner_typ| type_substitute(inner_typ, type_var, replace_with))
                .collect();
            typs_vec.and_then(|styps| Ok(Type::Tuple(styps)))
        }
        Type::Record(bindings) => {
            let bindings_vec: Result<Vector<(String, Type)>, TypeCheckError> = bindings
                .iter()
                .map(|pair| {
                    type_substitute(&pair.1, type_var, replace_with)
                        .and_then(|inner_typ| Ok((pair.0.clone(), inner_typ)))
                })
                .collect();
            bindings_vec.and_then(|sbindings| Ok(Type::Record(sbindings)))
        }
        Type::Exists(inner_type_var, inner_typ) => {
            if *inner_type_var == type_var {
                let new_inner_type_var = type_var + 1;
                match type_substitute(
                    dbg!(inner_typ),
                    dbg!(*inner_type_var),
                    dbg!(&Type::TypeVar(new_inner_type_var)),
                ) {
                    Ok(val) => type_substitute(dbg!(&val), dbg!(type_var), dbg!(replace_with))
                        .and_then(|sub_inner_typ| {
                            Ok(Type::Exists(new_inner_type_var, Box::from(sub_inner_typ)))
                        }),
                    Err(e) => Err(e),
                }
            } else {
                type_substitute(inner_typ, type_var, replace_with).and_then(|sub_inner_typ| {
                    Ok(Type::Exists(*inner_type_var, Box::from(sub_inner_typ)))
                })
            }
        }
        Type::TypeVar(x) => {
            if *x == type_var {
                Ok(replace_with.clone())
            } else {
                Ok(Type::TypeVar(*x))
            }
        }
        Type::Unknown => Ok(Type::Unknown),
    }
}

fn tc_pack_with_env(
    packed_exp: &Expr,
    sub: &Type,
    exist: &Type,
    env: &mut TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    if let Type::Exists(type_var, base_typ) = exist {
        // substitute "sub" for all occurrences of type_var (the quantified type) in exist
        let substituted_type = match type_substitute(base_typ, *type_var, sub) {
            Ok(val) => val,
            Err(e) => return Err(e),
        };
        // now check if the type of "substituted" matches the type of the packed expression
        tc_with_env(packed_exp, env).and_then(|packed_type| {
            if packed_type == substituted_type {
                Ok(exist.clone())
            } else {
                Err(TypeCheckError::from(
                    "Packed expression does not match existential type.",
                ))
            }
        })
    } else {
        Err(TypeCheckError::from(
            "Second argument in pack is not an existential type.",
        ))
    }
}

fn tc_unpack_with_env(
    var: &str,
    package: &Expr,
    typ_var: &Type,
    body: &Expr,
    env: &mut TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    // Validate that unpack has a valid type var
    let typ_var_value = match typ_var {
        Type::TypeVar(x) => *x,
        _ => {
            return Err(TypeCheckError::from(
                "Unpack expression does not contain valid unpack expression.",
            ))
        }
    };

    // Calculate the existential type of the package
    let package_typ = match tc_with_env(package, env) {
        Ok(val) => val,
        Err(e) => return Err(e),
    };
    // Get the type variable of the existential type
    let package_typ_var = match &package_typ {
        Type::Exists(inner_typ_var, _base_typ) => *inner_typ_var,
        _ => {
            return Err(TypeCheckError::from(
                "Package in unpack expression is not existentially typed.",
            ))
        }
    };
    // Get the base type of the existential type
    let package_base_typ: Type = match package_typ {
        Type::Exists(_inner_typ_var, base_typ) => *base_typ,
        _ => {
            return Err(TypeCheckError::from(
                "Package in unpack expression is not existentially typed.",
            ))
        }
    };
    // Substitute in the unpack type var for the type var in the base type
    let spackage_base_typ = match type_substitute(&package_base_typ, package_typ_var, typ_var) {
        Ok(val) => val,
        Err(e) => return Err(e),
    };
    match tc_with_env(
        dbg!(body),
        &mut env.add_binding((String::from(var), spackage_base_typ.clone())),
    ) {
        Ok(val) => {
            if type_contains_var(&val, typ_var_value) {
                Err(TypeCheckError::from(
                    "Scoping error: free type variable in type of body expression.",
                ))
            } else {
                Ok(val)
            }
        }
        Err(e) => Err(e),
    }
}

fn tc_array_with_env(
    values: &Vector<Expr>,
    env: &mut TypeEnv<Type>,
) -> Result<Vector<Type>, TypeCheckError> {
    values.iter().map(|val| tc_with_env(val, env)).collect()
}

pub fn tc_with_env(value: &Expr, env: &mut TypeEnv<Type>) -> Result<Type, TypeCheckError> {
    match &value.kind {
        ExprKind::Num(_) => Ok(Type::Int),
        ExprKind::Bool(_) => Ok(Type::Bool),
        ExprKind::Str(_) => Ok(Type::Str),
        ExprKind::Id(sym) => match env.find(sym.as_str()) {
            Some(val) => Ok(val.clone()),
            None => Err(TypeCheckError::from("Not a recognized function name.")),
        },
        ExprKind::Binop(op, arg1, arg2) => tc_binop_with_env(*op, &arg1, &arg2, env),
        ExprKind::If(pred, cons, alt) => tc_if_with_env(&pred, &cons, &alt, env),
        ExprKind::Let(bindings, body) => tc_let_with_env(&bindings, &body, env),
        ExprKind::Lambda(params, ret_typ, body) => {
            tc_lambda_with_env(&params, &ret_typ, &body, env)
        }
        ExprKind::Record(bindings) => tc_record_with_env(bindings, env),
        ExprKind::RecordGet(record, key) => tc_record_get_with_env(record, key, env),
        ExprKind::Begin(exps) => tc_begin_with_env(&exps, env),
        ExprKind::Set(sym, exp) => tc_set_bang_with_env(&sym, &exp, env),
        ExprKind::Cons(first, rest) => tc_cons_with_env(&first, &rest, env),
        ExprKind::Car(exp) => tc_car_with_env(&exp, env),
        ExprKind::Cdr(exp) => tc_cdr_with_env(&exp, env),
        ExprKind::IsNull(exp) => tc_is_null_with_env(&exp, env),
        ExprKind::Null(typ) => Ok(Type::List(Box::from(typ.clone()))),
        ExprKind::Tuple(exps) => tc_tuple_with_env(&exps, env),
        ExprKind::TupleGet(tup, key) => tc_tuple_get_with_env(&tup, &key, env),
        ExprKind::Pack(val, sub, exist) => tc_pack_with_env(&val, &sub, &exist, env),
        ExprKind::Unpack(var, package, typ_sub, body) => {
            tc_unpack_with_env(&var, &package, &typ_sub, &body, env)
        }
        ExprKind::FnApp(func, args) => tc_apply_with_env(&func, &args, env),
    }
}

pub fn type_check(value: &Expr) -> Result<Type, TypeCheckError> {
    tc_with_env(value, &mut TypeEnv::new())
}

// Only test private helper functions here;
// public API tests can go in tests/test_type_checker.rs
#[cfg(test)]
mod tests {
    use super::*;
    use im_rc::vector;

    #[test]
    fn test_type_substitute_idempotent() {
        // no substitution (simple)
        let typ = Type::Int;
        let type_var = 0;
        let replace_with = Type::Bool;
        assert_eq!(
            type_substitute(&typ, type_var, &replace_with).unwrap(),
            Type::Int
        );

        // no substitution (list)
        let typ = Type::List(Box::from(Type::Int));
        let type_var = 0;
        let replace_with = Type::Bool;
        assert_eq!(
            type_substitute(&typ, type_var, &replace_with).unwrap(),
            Type::List(Box::from(Type::Int))
        );

        // no substitution (function)
        let typ = Type::Func(vector![Type::Str, Type::Bool], Box::from(Type::Str));
        let type_var = 0;
        let replace_with = Type::Bool;
        assert_eq!(
            type_substitute(&typ, type_var, &replace_with).unwrap(),
            Type::Func(vector![Type::Str, Type::Bool], Box::from(Type::Str))
        );

        // no substitution (tuple)
        let typ = Type::Tuple(vector![Type::Str, Type::Bool, Type::Int]);
        let type_var = 0;
        let replace_with = Type::Bool;
        assert_eq!(
            type_substitute(&typ, type_var, &replace_with).unwrap(),
            Type::Tuple(vector![Type::Str, Type::Bool, Type::Int])
        );

        // no substitution (existential, different bound type)
        let typ = Type::Exists(1, Box::from(Type::TypeVar(1)));
        let type_var = 0;
        let replace_with = Type::Bool;
        assert_eq!(
            type_substitute(&typ, type_var, &replace_with).unwrap(),
            Type::Exists(1, Box::from(Type::TypeVar(1))),
        );

        // no substitution (existential, same bound type - inner typevar should get renamed)
        let typ = Type::Exists(0, Box::from(Type::TypeVar(0)));
        let type_var = 0;
        let replace_with = Type::Bool;
        assert_eq!(
            type_substitute(&typ, type_var, &replace_with).unwrap(),
            Type::Exists(1, Box::from(Type::TypeVar(1))),
        );

        // no substitution (different type var)
        let typ = Type::TypeVar(1);
        let type_var = 0;
        let replace_with = Type::Bool;
        assert_eq!(
            type_substitute(&typ, type_var, &replace_with).unwrap(),
            Type::TypeVar(1),
        );
    }

    #[test]
    fn test_type_substitute_happy() {
        // substitution (simple)
        let typ = Type::TypeVar(3);
        let type_var = 3;
        let replace_with = Type::Bool;
        assert_eq!(
            type_substitute(&typ, type_var, &replace_with).unwrap(),
            Type::Bool
        );

        // substitution (list)
        let typ = Type::List(Box::from(Type::TypeVar(3)));
        let type_var = 3;
        let replace_with = Type::Bool;
        assert_eq!(
            type_substitute(&typ, type_var, &replace_with).unwrap(),
            Type::List(Box::from(Type::Bool))
        );

        // substitution (function)
        let typ = Type::Func(
            vector![Type::TypeVar(3), Type::Bool],
            Box::from(Type::TypeVar(3)),
        );
        let type_var = 3;
        let replace_with = Type::Int;
        assert_eq!(
            type_substitute(&typ, type_var, &replace_with).unwrap(),
            Type::Func(vector![Type::Int, Type::Bool], Box::from(Type::Int))
        );

        // substitution (tuple)
        let typ = Type::Tuple(vector![Type::TypeVar(3), Type::Bool]);
        let type_var = 3;
        let replace_with = Type::Int;
        assert_eq!(
            type_substitute(&typ, type_var, &replace_with).unwrap(),
            Type::Tuple(vector![Type::Int, Type::Bool])
        );

        // substitution (existential)
        let typ = Type::Exists(
            1,
            Box::from(Type::Tuple(vector![Type::TypeVar(0), Type::TypeVar(1)])),
        );
        let type_var = 0;
        let replace_with = Type::Bool;
        assert_eq!(
            type_substitute(&typ, type_var, &replace_with).unwrap(),
            Type::Exists(
                1,
                Box::from(Type::Tuple(vector![Type::Bool, Type::TypeVar(1)]))
            ),
        );
    }
}
