use crate::common::{type_contains_var, type_substitute, BinOp, Expr, ExprKind, Type, TypeEnv};
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
    env: &TypeEnv<Type>,
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
    let arg1_typ = tc_with_env(arg1, env)?;
    let arg2_typ = tc_with_env(arg2, env)?;
    if arg1_expect_typ != arg1_typ || arg2_expect_typ != arg2_typ {
        Err(TypeCheckError::from(
            "Binary operation parameters do not match expected types.",
        ))
    } else {
        Ok(ret_typ)
    }
}

fn tc_if_with_env(
    predicate: &Expr,
    consequent: &Expr,
    alternate: &Expr,
    env: &TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    let pred_typ = tc_with_env(predicate, env)?;
    let cons_typ = tc_with_env(consequent, env)?;
    let alt_typ = tc_with_env(alternate, env)?;
    if pred_typ != Type::Bool {
        Err(TypeCheckError::from(
            "Predicate in if expression does not evaluate to a boolean value.",
        ))
    } else if cons_typ != alt_typ {
        Err(TypeCheckError::from(
            "Consequent and alternate values in if expression do not match types.",
        ))
    } else {
        Ok(cons_typ)
    }
}

fn tc_let_with_env(
    bindings: &Vector<(String, Expr)>,
    body: &Expr,
    env: &TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    let type_bindings: Vector<(String, Type)> = bindings
        .iter()
        .map(|pair| Ok((pair.0.clone(), tc_with_env(&pair.1, env)?)))
        .collect::<Result<Vector<(String, Type)>, TypeCheckError>>()?;
    let new_env = env.add_bindings(type_bindings);
    tc_with_env(body, &new_env)
}

fn tc_lambda_with_env(
    params: &Vector<(String, Type)>,
    ret_typ: &Type,
    body: &Expr,
    env: &TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    // Add arg types to the type environment for use in the body
    let new_env = env.add_bindings(params.clone());

    // Type check lambda body
    let body_typ = tc_with_env(body, &new_env)?;
    if *ret_typ == body_typ {
        let param_types: Vector<Type> = params.iter().map(|pair| pair.1.clone()).collect();
        Ok(Type::Func(param_types, Box::from(ret_typ.clone())))
    } else {
        Err(TypeCheckError::from(
            "Lambda expression body type does not match the expected return type.",
        ))
    }
}

fn tc_begin_with_env(exps: &Vector<Expr>, env: &TypeEnv<Type>) -> Result<Type, TypeCheckError> {
    // Note: even though we only return the type of the
    // last expression within the 'begin' S-expression, we still want to
    // type-check the entire array in case any type errors pop up
    let exp_typs = tc_array_with_env(exps, env)?;
    Ok(exp_typs.clone().remove(exp_typs.len() - 1))
}

// set! returns the value that is being assigned, since the language has no unit type
fn tc_set_bang_with_env(
    var: &str,
    new_val: &Expr,
    env: &TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    let expected_typ = env
        .find(var)
        .ok_or_else(|| "Variable assignment cannot occur before it has been defined!")?
        .clone();
    let new_val_typ = tc_with_env(new_val, env)?;
    if new_val_typ == expected_typ {
        Ok(new_val_typ)
    } else {
        Err(TypeCheckError::from(
            "Type of set! body does not match variable's initialized type.",
        ))
    }
}

fn tc_cons_with_env(
    first: &Expr,
    rest: &Expr,
    env: &TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    let car_type = tc_with_env(first, env)?;
    let cdr_type = tc_with_env(rest, env)?;
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

fn tc_car_with_env(pair: &Expr, env: &TypeEnv<Type>) -> Result<Type, TypeCheckError> {
    let pair_typ = tc_with_env(pair, env)?;
    match pair_typ {
        Type::List(boxed_type) => Ok(*boxed_type),
        _ => Err(TypeCheckError::from(
            "Expression in car is not a list type.",
        )),
    }
}

fn tc_cdr_with_env(pair: &Expr, env: &TypeEnv<Type>) -> Result<Type, TypeCheckError> {
    let pair_typ = tc_with_env(pair, env)?;
    match pair_typ {
        Type::List(boxed_type) => Ok(Type::List(Box::from(*boxed_type))),
        _ => Err(TypeCheckError::from(
            "Expression in car is not a list type.",
        )),
    }
}

fn tc_tuple_with_env(exps: &Vector<Expr>, env: &TypeEnv<Type>) -> Result<Type, TypeCheckError> {
    tc_array_with_env(exps, env).and_then(|typs| Ok(Type::Tuple(typs)))
}

fn tc_tuple_get_with_env(
    tup: &Expr,
    key: u64,
    env: &TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    match tc_with_env(tup, env)? {
        Type::Tuple(vec) => {
            if (key as usize) < vec.len() {
                Ok(vec[key as usize].clone())
            } else {
                Err(TypeCheckError::from(
                    "Value in tuple-ref is too large for the provided tuple.",
                ))
            }
        }
        _ => Err(TypeCheckError::from(
            "First expression in tuple-ref is not a tuple.",
        )),
    }
}

fn tc_record_with_env(
    bindings: &Vector<(String, Expr)>,
    env: &TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    let binding_types = bindings
        .iter()
        .map(|pair| Ok((pair.0.clone(), tc_with_env(&pair.1, env)?)))
        .collect::<Result<Vector<(String, Type)>, TypeCheckError>>()?;
    Ok(Type::Record(binding_types))
}

fn tc_record_get_with_env(
    record: &Expr,
    key: &str,
    env: &TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    match tc_with_env(record, env)? {
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
    }
}

fn tc_apply_with_env(
    func: &Expr,
    args: &Vector<Expr>,
    env: &TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    let func_typ = tc_with_env(func, env)?;
    let param_types = tc_array_with_env(&args, env)?;
    check_lambda_type_with_inputs(&func_typ, &param_types)
}

// This always returns Type::Bool, but we still need to type check the inside.
fn tc_is_null_with_env(exp: &Expr, env: &TypeEnv<Type>) -> Result<Type, TypeCheckError> {
    tc_with_env(exp, env)?;
    Ok(Type::Bool)
}

fn tc_pack_with_env(
    packed_exp: &Expr,
    sub: &Type,
    exist: &Type,
    env: &TypeEnv<Type>,
) -> Result<Type, TypeCheckError> {
    if let Type::Exists(type_var, base_typ) = exist {
        // substitute "sub" for all occurrences of type_var (the quantified type) in exist
        let substituted_typ = type_substitute(base_typ, *type_var, sub);
        // now check if the type of "substituted" matches the type of the packed expression
        let packed_typ = tc_with_env(packed_exp, env)?;
        if packed_typ == substituted_typ {
            Ok(exist.clone())
        } else {
            Err(TypeCheckError::from(
                "Packed expression does not match existential type.",
            ))
        }
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
    env: &TypeEnv<Type>,
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
    let package_typ = tc_with_env(package, env)?;

    // Extract fields from the existential type
    let package_typ_var: u64;
    let package_base_typ: Type;
    match &package_typ {
        Type::Exists(inner_typ_var, base_typ) => {
            package_typ_var = *inner_typ_var;
            package_base_typ = (**base_typ).clone();
        }
        _ => {
            return Err(TypeCheckError::from(
                "Package in unpack expression is not existentially typed.",
            ))
        }
    }

    // Substitute in the unpack type var for the type var in the base type
    let spackage_base_typ = type_substitute(&package_base_typ, package_typ_var, typ_var);
    let body_typ = tc_with_env(
        body,
        &env.add_binding((String::from(var), spackage_base_typ.clone())),
    )?;
    if type_contains_var(&body_typ, typ_var_value) {
        return Err(TypeCheckError::from(
            "Scoping error: free type variable in type of body expression.",
        ));
    }
    Ok(body_typ)
}

fn tc_array_with_env(
    values: &Vector<Expr>,
    env: &TypeEnv<Type>,
) -> Result<Vector<Type>, TypeCheckError> {
    values.iter().map(|val| tc_with_env(val, env)).collect()
}

pub fn tc_with_env(value: &Expr, env: &TypeEnv<Type>) -> Result<Type, TypeCheckError> {
    match &*value.kind {
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
        ExprKind::Record(bindings) => tc_record_with_env(&bindings, env),
        ExprKind::RecordGet(record, key) => tc_record_get_with_env(&record, &key, env),
        ExprKind::Begin(exps) => tc_begin_with_env(&exps, env),
        ExprKind::Set(sym, exp) => tc_set_bang_with_env(&sym, &exp, env),
        ExprKind::Cons(first, rest) => tc_cons_with_env(&first, &rest, env),
        ExprKind::Car(exp) => tc_car_with_env(&exp, env),
        ExprKind::Cdr(exp) => tc_cdr_with_env(&exp, env),
        ExprKind::IsNull(exp) => tc_is_null_with_env(&exp, env),
        ExprKind::Null(typ) => Ok(Type::List(Box::from(typ.clone()))),
        ExprKind::Tuple(exps) => tc_tuple_with_env(&exps, env),
        ExprKind::TupleGet(tup, key) => tc_tuple_get_with_env(&tup, *key, env),
        ExprKind::Pack(val, sub, exist) => tc_pack_with_env(&val, &sub, &exist, env),
        ExprKind::Unpack(var, package, typ_sub, body) => {
            tc_unpack_with_env(&var, &package, &typ_sub, &body, env)
        }
        ExprKind::FnApp(func, args) => tc_apply_with_env(&func, &args, env),
    }
}

pub fn type_check(value: &Expr) -> Result<Type, TypeCheckError> {
    tc_with_env(value, &TypeEnv::new())
}
