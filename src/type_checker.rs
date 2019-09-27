use crate::parser::{BinOp, Expr, Type};
use im_rc::Vector;

#[derive(Default)]
pub struct TypeEnv {
    bindings: Vector<(String, Type)>,
}

// New values are appended to the front of the frame
impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv {
            bindings: Vector::new(),
        }
    }

    /// Returns a new environment extended with the provided binding.
    pub fn add_binding(&mut self, new_binding: (String, Type)) -> TypeEnv {
        let mut bindings = self.bindings.clone();
        bindings.push_front(new_binding);
        TypeEnv { bindings }
    }

    /// Returns a new environment extended with the provided bindings.
    pub fn add_bindings(&mut self, new_bindings: Vector<(String, Type)>) -> TypeEnv {
        let mut bindings = self.bindings.clone();
        for binding in new_bindings {
            bindings.push_front(binding);
        }
        TypeEnv { bindings }
    }

    pub fn find(&mut self, key: &str) -> Option<&Type> {
        for pair in self.bindings.iter() {
            if pair.0 == key {
                return Some(&pair.1);
            }
        }
        None
    }
}

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

fn check_type_arrays_equal(arr1: &Vector<Type>, arr2: &Vector<Type>) -> bool {
    if arr1.len() != arr2.len() {
        return false;
    }
    arr1.iter().zip(arr2.iter()).all(|(typ1, typ2)| match typ1 {
        Type::Func(arg_types1, ret_type_boxed1) => match typ2 {
            Type::Func(arg_types2, ret_type_boxed2) => {
                check_type_arrays_equal(arg_types1, arg_types2)
                    && (*ret_type_boxed1 == *ret_type_boxed2)
            }
            _ => false,
        },
        _ => typ1 == typ2,
    })
}

fn check_lambda_type_with_inputs(
    fn_type: &Type,
    param_types: &Vector<Type>,
) -> Result<Type, TypeCheckError> {
    match fn_type {
        Type::Func(arg_types, ret_type_boxed) => {
            let ret_type = ret_type_boxed.as_ref();
            if check_type_arrays_equal(arg_types, &param_types) {
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
    op: &BinOp,
    arg1: &Expr,
    arg2: &Expr,
    env: &mut TypeEnv,
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
    env: &mut TypeEnv,
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
    env: &mut TypeEnv,
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
    env: &mut TypeEnv,
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

fn tc_begin_with_env(exps: &Vector<Expr>, env: &mut TypeEnv) -> Result<Type, TypeCheckError> {
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
    env: &mut TypeEnv,
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

fn tc_cons_with_env(first: &Expr, rest: &Expr, env: &mut TypeEnv) -> Result<Type, TypeCheckError> {
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

fn tc_car_with_env(exp: &Expr, env: &mut TypeEnv) -> Result<Type, TypeCheckError> {
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

fn tc_cdr_with_env(exp: &Expr, env: &mut TypeEnv) -> Result<Type, TypeCheckError> {
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

fn tc_apply_with_env(
    func: &Expr,
    args: &Vector<Expr>,
    env: &mut TypeEnv,
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
fn tc_is_null_with_env(exp: &Expr, env: &mut TypeEnv) -> Result<Type, TypeCheckError> {
    match tc_with_env(exp, env) {
        Ok(_) => Ok(Type::Bool),
        Err(e) => Err(e),
    }
}

fn tc_array_with_env(
    values: &Vector<Expr>,
    env: &mut TypeEnv,
) -> Result<Vector<Type>, TypeCheckError> {
    values.iter().map(|val| tc_with_env(val, env)).collect()
}

pub fn tc_with_env(value: &Expr, env: &mut TypeEnv) -> Result<Type, TypeCheckError> {
    match value {
        Expr::Num(_) => Ok(Type::Int),
        Expr::Bool(_) => Ok(Type::Bool),
        Expr::Str(_) => Ok(Type::Str),
        Expr::Sym(sym) => match env.find(sym) {
            Some(val) => Ok(val.clone()),
            None => Err(TypeCheckError::from("Not a recognized function name.")),
        },
        Expr::Binop(op, arg1, arg2) => tc_binop_with_env(&op, arg1, arg2, env),
        Expr::If(pred, cons, alt) => tc_if_with_env(pred, cons, alt, env),
        Expr::Let(bindings, body) => tc_let_with_env(bindings, body, env),
        Expr::Lambda(params, ret_typ, body) => tc_lambda_with_env(params, ret_typ, body, env),
        Expr::Begin(exps) => tc_begin_with_env(exps, env),
        Expr::Set(sym, exp) => tc_set_bang_with_env(sym, exp, env),
        Expr::Cons(first, rest) => tc_cons_with_env(first, rest, env),
        Expr::Car(exp) => tc_car_with_env(exp, env),
        Expr::Cdr(exp) => tc_cdr_with_env(exp, env),
        Expr::IsNull(exp) => tc_is_null_with_env(exp, env),
        Expr::Null(typ) => Ok(Type::List(Box::from(typ.clone()))),
        Expr::FnApp(func, args) => tc_apply_with_env(func, args, env),
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
    fn test_check_type_arrays_equal_happy() {
        let types1 = vector![Type::Int, Type::Bool, Type::Str];
        let types2 = vector![Type::Int, Type::Bool, Type::Str];
        assert_eq!(check_type_arrays_equal(&types1, &types2), true);

        let types1 = vector![Type::Func(vector![], Box::from(Type::Int))];
        let types2 = vector![Type::Func(vector![], Box::from(Type::Int))];
        assert_eq!(check_type_arrays_equal(&types1, &types2), true);

        let types1 = vector![Type::Func(
            vector![Type::Int, Type::Int],
            Box::from(Type::Bool),
        )];
        let types2 = vector![Type::Func(
            vector![Type::Int, Type::Int],
            Box::from(Type::Bool),
        )];
        assert_eq!(check_type_arrays_equal(&types1, &types2), true);
    }

    #[test]
    fn test_check_type_arrays_equal_sad() {
        // types reordered
        let types1 = vector![Type::Int, Type::Bool, Type::Str];
        let types2 = vector![Type::Int, Type::Str, Type::Bool];
        assert_eq!(check_type_arrays_equal(&types1, &types2), false);

        // arr1 shorter
        let types1 = vector![Type::Int];
        let types2 = vector![Type::Int, Type::Str, Type::Bool];
        assert_eq!(check_type_arrays_equal(&types1, &types2), false);

        // arr2 shorter
        let types1 = vector![Type::Int, Type::Bool, Type::Str];
        let types2 = vector![Type::Int];
        assert_eq!(check_type_arrays_equal(&types1, &types2), false);

        // return types differ
        let types1 = vector![Type::Func(
            vector![Type::Int, Type::Int],
            Box::from(Type::Bool),
        )];
        let types2 = vector![Type::Func(
            vector![Type::Int, Type::Int],
            Box::from(Type::Str),
        )];
        assert_eq!(check_type_arrays_equal(&types1, &types2), false);

        // input types differ
        let types1 = vector![Type::Func(
            vector![Type::Int, Type::Int],
            Box::from(Type::Bool),
        )];
        let types2 = vector![Type::Func(
            vector![Type::Int, Type::Str],
            Box::from(Type::Bool),
        )];
        assert_eq!(check_type_arrays_equal(&types1, &types2), false);
    }
}
