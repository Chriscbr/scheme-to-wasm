use crate::common::{BinOp, Expr, ExprKind, Prog, TypeEnv};
use crate::types::{type_contains_var, type_var_substitute, Type};
use im_rc::{vector, Vector};

// TODO: Update type checker / Expr class to annotate individual nodes of
// expression tree with appropriate types.

#[derive(Clone, Debug)]
pub struct TypeCheckError(String);

// Allows other errors to wrap this one
impl std::error::Error for TypeCheckError {}

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
) -> Result<Expr, TypeCheckError> {
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
    let arg1 = tc_with_env(arg1, env)?;
    let arg1_typ = arg1
        .checked_type
        .clone()
        .ok_or_else(|| TypeCheckError::from("Arg1 does not have type annotation."))?;
    let arg2 = tc_with_env(arg2, env)?;
    let arg2_typ = arg2
        .checked_type
        .clone()
        .ok_or_else(|| TypeCheckError::from("Arg2 does not have type annotation."))?;
    if arg1_expect_typ != arg1_typ || arg2_expect_typ != arg2_typ {
        Err(TypeCheckError::from(
            "Binary operation parameters do not match expected types.",
        ))
    } else {
        Ok(Expr::new(Some(ret_typ), ExprKind::Binop(op, arg1, arg2)))
    }
}

fn tc_if_with_env(
    predicate: &Expr,
    consequent: &Expr,
    alternate: &Expr,
    env: &TypeEnv<Type>,
) -> Result<Expr, TypeCheckError> {
    let pred = tc_with_env(predicate, env)?;
    let pred_typ = pred
        .checked_type
        .clone()
        .ok_or_else(|| TypeCheckError::from("Predicate does not have type annotation."))?;
    let cons = tc_with_env(consequent, env)?;
    let cons_typ = cons
        .checked_type
        .clone()
        .ok_or_else(|| TypeCheckError::from("Consequent does not have type annotation."))?;
    let alt = tc_with_env(alternate, env)?;
    let alt_typ = alt
        .checked_type
        .clone()
        .ok_or_else(|| TypeCheckError::from("Alternate does not have type annotation."))?;
    if pred_typ != Type::Bool {
        Err(TypeCheckError::from(
            "Predicate in if expression does not evaluate to a boolean value.",
        ))
    } else if cons_typ != alt_typ {
        Err(TypeCheckError::from(
            "Consequent and alternate values in if expression do not match types.",
        ))
    } else {
        Ok(Expr::new(Some(cons_typ), ExprKind::If(pred, cons, alt)))
    }
}

fn tc_let_with_env(
    bindings: &Vector<(String, Expr)>,
    body: &Expr,
    env: &TypeEnv<Type>,
) -> Result<Expr, TypeCheckError> {
    let type_bindings: Vector<(String, Type)> = bindings
        .iter()
        .map(|pair| {
            Ok((
                pair.0.clone(),
                tc_with_env(&pair.1, env)?.checked_type.ok_or_else(|| {
                    TypeCheckError::from("Type binding does not have type annotation.")
                })?,
            ))
        })
        .collect::<Result<Vector<(String, Type)>, TypeCheckError>>()?;
    let new_env = env.add_bindings(type_bindings);
    tc_with_env(body, &new_env)
}

fn tc_lambda_with_env(
    params: &Vector<(String, Type)>,
    ret_type: &Type,
    body: &Expr,
    env: &TypeEnv<Type>,
) -> Result<Expr, TypeCheckError> {
    // Add arg types to the type environment for use in the body
    let new_env = env.add_bindings(params.clone());

    // Type check lambda body
    let body = tc_with_env(body, &new_env)?;
    let body_type = body
        .checked_type
        .clone()
        .ok_or_else(|| TypeCheckError::from("Body does not have type annotation."))?;
    if *ret_type == body_type {
        let param_types: Vector<Type> = params.iter().map(|pair| pair.1.clone()).collect();
        let lambda_typ = Type::Func(param_types, Box::new(ret_type.clone()));
        Ok(Expr::new(
            Some(lambda_typ),
            ExprKind::Lambda(params.clone(), ret_type.clone(), body),
        ))
    } else {
        Err(TypeCheckError::from(
            "Lambda expression body type does not match the expected return type.",
        ))
    }
}

fn tc_begin_with_env(exps: &Vector<Expr>, env: &TypeEnv<Type>) -> Result<Expr, TypeCheckError> {
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
) -> Result<Expr, TypeCheckError> {
    let expected_typ = env
        .find(var)
        .ok_or_else(|| "Variable assignment cannot occur before it has been defined!")?
        .clone();
    let new_val = tc_with_env(new_val, env)?;
    let new_val_typ = new_val
        .checked_type
        .clone()
        .ok_or_else(|| TypeCheckError::from("New val does not have type annotation."))?;
    if new_val_typ == expected_typ {
        Ok(Expr::new(
            Some(new_val_typ),
            ExprKind::Set(String::from(var), new_val),
        ))
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
) -> Result<Expr, TypeCheckError> {
    let car = tc_with_env(first, env)?;
    let car_type = car
        .checked_type
        .clone()
        .ok_or_else(|| TypeCheckError::from("Car does not have type annotation."))?;
    let cdr = tc_with_env(rest, env)?;
    let cdr_type = cdr
        .checked_type
        .clone()
        .ok_or_else(|| TypeCheckError::from("Cdr does not have type annotation."))?;
    match cdr_type {
        Type::List(boxed_type) => {
            if *boxed_type == car_type {
                Ok(Expr::new(
                    Some(Type::List(boxed_type)),
                    ExprKind::Cons(car, cdr),
                ))
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

fn tc_car_with_env(pair: &Expr, env: &TypeEnv<Type>) -> Result<Expr, TypeCheckError> {
    let pair = tc_with_env(pair, env)?;
    let pair_typ = pair
        .checked_type
        .clone()
        .ok_or_else(|| TypeCheckError::from("Pair does not have type annotation."))?;
    match pair_typ {
        Type::List(boxed_type) => Ok(Expr::new(Some(*boxed_type), ExprKind::Car(pair))),
        _ => Err(TypeCheckError::from(
            "Expression in car is not a list type.",
        )),
    }
}

fn tc_cdr_with_env(pair: &Expr, env: &TypeEnv<Type>) -> Result<Expr, TypeCheckError> {
    let pair = tc_with_env(pair, env)?;
    let pair_typ = pair
        .checked_type
        .clone()
        .ok_or_else(|| TypeCheckError::from("Pair does not have type annotation."))?;
    match pair_typ {
        Type::List(boxed_type) => Ok(Expr::new(
            Some(Type::List(Box::new(*boxed_type))),
            ExprKind::Cdr(pair),
        )),
        _ => Err(TypeCheckError::from(
            "Expression in car is not a list type.",
        )),
    }
}

fn tc_tuple_with_env(exps: &Vector<Expr>, env: &TypeEnv<Type>) -> Result<Expr, TypeCheckError> {
    let typed_exps = tc_array_with_env(exps, env)?;
    let inner_types = typed_exps
        .iter()
        .map(|typed_exp| {
            typed_exp
                .checked_type
                .clone()
                .ok_or_else(|| TypeCheckError::from("tuple element does not have type annotation."))
        })
        .collect::<Result<Vector<Type>, TypeCheckError>>()?;
    Ok(Expr::new(
        Some(Type::Tuple(inner_types)),
        ExprKind::Tuple(typed_exps),
    ))
}

fn tc_tuple_get_with_env(
    tup: &Expr,
    key: u64,
    env: &TypeEnv<Type>,
) -> Result<Expr, TypeCheckError> {
    let tup = tc_with_env(tup, env)?;
    let tup_type = tup
        .checked_type
        .clone()
        .ok_or_else(|| TypeCheckError::from("Tuple does not have type annotation."))?;
    match tup_type {
        Type::Tuple(vec) => {
            if (key as usize) < vec.len() {
                let elem_type = vec[key as usize].clone();
                Ok(Expr::new(Some(elem_type), ExprKind::TupleGet(tup, key)))
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
) -> Result<Expr, TypeCheckError> {
    let typed_bindings = bindings
        .iter()
        .map(|pair| Ok((pair.0.clone(), tc_with_env(&pair.1, env)?)))
        .collect::<Result<Vector<(String, Expr)>, TypeCheckError>>()?;
    let bindings_type = typed_bindings
        .iter()
        .map(|pair| {
            Ok((
                pair.0.clone(),
                pair.1.checked_type.clone().ok_or_else(|| {
                    TypeCheckError::from("Binding does not have type annotation.")
                })?,
            ))
        })
        .collect::<Result<Vector<(String, Type)>, TypeCheckError>>()?;
    Ok(Expr::new(
        Some(Type::Record(bindings_type)),
        ExprKind::Record(typed_bindings),
    ))
}

fn tc_record_get_with_env(
    record: &Expr,
    key: &str,
    env: &TypeEnv<Type>,
) -> Result<Expr, TypeCheckError> {
    let typed_record = tc_with_env(record, env)?;
    let record_type = typed_record
        .checked_type
        .clone()
        .ok_or_else(|| TypeCheckError::from("Record does not have type annotation."))?;
    match record_type {
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
            let value_type = matches[0].1.clone();
            Ok(Expr::new(
                Some(value_type),
                ExprKind::RecordGet(typed_record, String::from(key)),
            ))
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
) -> Result<Expr, TypeCheckError> {
    let func = tc_with_env(func, env)?;
    let func_typ = func
        .checked_type
        .clone()
        .ok_or_else(|| TypeCheckError::from("Function does not have type annotation."))?;
    let params = tc_array_with_env(&args, env)?;
    let param_types = params
        .iter()
        .map(|typed_exp| {
            Ok(typed_exp
                .checked_type
                .clone()
                .ok_or_else(|| TypeCheckError::from("Parameter does not have type annotation."))?)
        })
        .collect::<Result<Vector<Type>, TypeCheckError>>()?;
    let lambda_type = check_lambda_type_with_inputs(&func_typ, &param_types)?;
    Ok(Expr::new(Some(lambda_type), ExprKind::FnApp(func, params)))
}

fn tc_is_null_with_env(exp: &Expr, env: &TypeEnv<Type>) -> Result<Expr, TypeCheckError> {
    let typed_exp = tc_with_env(exp, env)?;
    Ok(Expr::new(Some(Type::Bool), ExprKind::IsNull(typed_exp)))
}

fn tc_pack_with_env(
    packed_exp: &Expr,
    sub: &Type,
    exist: &Type,
    env: &TypeEnv<Type>,
) -> Result<Expr, TypeCheckError> {
    if let Type::Exists(type_var, base_typ) = exist {
        // substitute "sub" for all occurrences of type_var (the quantified type) in exist
        let substituted_typ = type_var_substitute(base_typ, *type_var, sub);
        // now check if the type of "substituted" matches the type of the packed expression
        let packed_exp = tc_with_env(packed_exp, env)?;
        let packed_typ = packed_exp.checked_type.clone().ok_or_else(|| {
            TypeCheckError::from("Packed expression does not have type annotation.")
        })?;
        if packed_typ == substituted_typ {
            Ok(Expr::new(
                Some(exist.clone()),
                ExprKind::Pack(packed_exp, sub.clone(), exist.clone()),
            ))
        // Ok(exist.clone())
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
    typ_var: u64,
    body: &Expr,
    env: &TypeEnv<Type>,
) -> Result<Expr, TypeCheckError> {
    // Calculate the existential type of the package
    let package = tc_with_env(package, env)?;
    let package_typ = package
        .checked_type
        .clone()
        .ok_or_else(|| TypeCheckError::from("Package does not have type annotation."))?;

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
    let spackage_base_typ =
        type_var_substitute(&package_base_typ, package_typ_var, &Type::TypeVar(typ_var));
    let body = tc_with_env(
        body,
        &env.add_binding((String::from(var), spackage_base_typ)),
    )?;
    let body_typ = body
        .checked_type
        .clone()
        .ok_or_else(|| TypeCheckError::from("Body does not have type annotation."))?;
    if type_contains_var(&body_typ, typ_var) {
        return Err(TypeCheckError::from(
            "Scoping error: free type variable in type of body expression.",
        ));
    }
    Ok(Expr::new(
        Some(body_typ),
        ExprKind::Unpack(String::from(var), package, typ_var, body),
    ))
}

fn tc_array_with_env(
    values: &Vector<Expr>,
    env: &TypeEnv<Type>,
) -> Result<Vector<Expr>, TypeCheckError> {
    values.iter().map(|val| tc_with_env(val, env)).collect()
}

pub fn tc_with_env(value: &Expr, env: &TypeEnv<Type>) -> Result<Expr, TypeCheckError> {
    match &*value.kind {
        ExprKind::Num(x) => Ok(Expr::new(Some(Type::Int), ExprKind::Num(*x))),
        ExprKind::Bool(x) => Ok(Expr::new(Some(Type::Bool), ExprKind::Bool(*x))),
        ExprKind::Str(x) => Ok(Expr::new(Some(Type::Str), ExprKind::Str(x.clone()))),
        ExprKind::Id(sym) => {
            let typ = match env.find(sym.as_str()) {
                Some(val) => Ok(val.clone()),
                None => Err(TypeCheckError(format!(
                    "Not a recognized function name: {}.",
                    sym
                ))),
            }?;
            Ok(Expr {
                checked_type: Some(typ),
                kind: Box::new(ExprKind::Id(sym.clone())),
            })
        }
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
        ExprKind::Null(typ) => Ok(Expr {
            checked_type: Some(Type::List(Box::new(typ.clone()))),
            kind: Box::new(ExprKind::Null(typ.clone())),
        }),
        ExprKind::Tuple(exps) => tc_tuple_with_env(&exps, env),
        ExprKind::TupleGet(tup, key) => tc_tuple_get_with_env(&tup, *key, env),
        ExprKind::Pack(val, sub, exist) => tc_pack_with_env(&val, &sub, &exist, env),
        ExprKind::Unpack(var, package, typ_sub, body) => {
            tc_unpack_with_env(&var, &package, *typ_sub, &body, env)
        }
        ExprKind::FnApp(func, args) => tc_apply_with_env(&func, &args, env),
    }
}

pub fn type_check(value: &Expr) -> Result<Expr, TypeCheckError> {
    tc_with_env(value, &TypeEnv::new())
}

pub fn type_check_prog(prog: &Prog) -> Result<Prog, TypeCheckError> {
    let mut env = TypeEnv::new();
    let mut typed_fns: Vector<(String, Expr)> = vector![];
    for def in prog.fns.iter() {
        let typed_fn = tc_with_env(&def.1, &env)?;
        env = env.add_binding((
            def.0.clone(),
            typed_fn
                .checked_type
                .clone()
                .ok_or_else(|| TypeCheckError::from("Function does not have type annotation."))?,
        ));
        typed_fns.push_back((def.0.clone(), typed_fn));
    }
    let prog_exp = tc_with_env(&prog.exp, &env)?;
    Ok(Prog {
        fns: typed_fns,
        exp: prog_exp,
    })
}
