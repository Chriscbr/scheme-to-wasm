use std::collections::HashMap;
use std::hash::BuildHasher;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Str,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeCheckError;

impl std::fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "TypeCheckError")
    }
}

impl std::error::Error for TypeCheckError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        // Generic error, underlying cause isn't tracked.
        None
    }
}

// Assume that the let is only defining one variable
fn tc_let_with_env<S: BuildHasher>(
    rest_exp: &[lexpr::Value],
    env: &mut HashMap<String, Type, S>,
) -> Result<Type, TypeCheckError> {
    if rest_exp.len() != 2 {
        return Err(TypeCheckError);
    }
    // ([x 23]) as vec of 1 element
    let bindings = &rest_exp[0];
    let body = &rest_exp[1];

    // ([x 23]) as vec of 1 element
    let bindings = match bindings.to_vec() {
        Some(vec) => vec,
        None => return Err(TypeCheckError),
    };

    // [x 23] as lexpr value
    let binding = &bindings[0];

    // [x 23] as vec of 2 elements
    let binding = match binding.to_vec() {
        Some(vec) => vec,
        None => return Err(TypeCheckError),
    };
    let var_name = match binding[0].as_symbol() {
        Some(val) => val,
        None => return Err(TypeCheckError),
    };
    let exp_type = match tc_with_env(&binding[1], env) {
        Ok(typ) => typ,
        Err(e) => return Err(e),
    };
    env.insert(String::from(var_name), exp_type);
    tc_with_env(body, env)
}

fn tc_if_with_env<S: BuildHasher>(
    rest_exp: &[lexpr::Value],
    env: &mut HashMap<String, Type, S>,
) -> Result<Type, TypeCheckError> {
    // this if statement should be a one liner somehow
    if rest_exp.len() != 3 {
        return Err(TypeCheckError);
    }
    let predicate = match tc_with_env(&rest_exp[0], env) {
        Ok(typ) => typ,
        Err(e) => return Err(e),
    };
    let consequent = match tc_with_env(&rest_exp[1], env) {
        Ok(typ) => typ,
        Err(e) => return Err(e),
    };
    let alternate = match tc_with_env(&rest_exp[2], env) {
        Ok(typ) => typ,
        Err(e) => return Err(e),
    };
    if predicate != Type::Bool || consequent != alternate {
        Err(TypeCheckError)
    } else {
        Ok(consequent)
    }
}

fn tc_binop_with_env<S: BuildHasher>(
    rest_exp: &[lexpr::Value],
    in1_typ: Type,
    in2_typ: Type,
    out_typ: Type,
    env: &mut HashMap<String, Type, S>,
) -> Result<Type, TypeCheckError> {
    if rest_exp.len() != 2 {
        return Err(TypeCheckError);
    };
    let in1 = match tc_with_env(&rest_exp[0], env) {
        Ok(typ) => typ,
        Err(e) => return Err(e),
    };
    let in2 = match tc_with_env(&rest_exp[1], env) {
        Ok(typ) => typ,
        Err(e) => return Err(e),
    };
    if in1 != in1_typ || in2 != in2_typ {
        Err(TypeCheckError)
    } else {
        Ok(out_typ)
    }
}

fn tc_with_env<S: BuildHasher>(
    value: &lexpr::Value,
    env: &mut HashMap<String, Type, S>,
) -> Result<Type, TypeCheckError> {
    match value {
        lexpr::Value::Number(_) => Ok(Type::Int),
        lexpr::Value::Bool(_) => Ok(Type::Bool),
        lexpr::Value::String(_) => Ok(Type::Str),
        lexpr::Value::Cons(_) => {
            if !value.is_list() {
                return Err(TypeCheckError);
            }
            let lst = match value.to_vec() {
                Some(vec) => vec,
                None => return Err(TypeCheckError),
            };
            let lst_parts = lst.split_at(1);
            let first = lst_parts.0;
            let rest = lst_parts.1;

            // current language does not have any structures in grammar
            // where S-expression starts with a non-symbol
            let operator = match first[0].as_symbol() {
                Some(val) => val,
                None => return Err(TypeCheckError),
            };
            match operator {
                "+" | "*" | "-" | "/" => {
                    tc_binop_with_env(&rest, Type::Int, Type::Int, Type::Int, env)
                }
                ">" | "<" | ">=" | "<=" | "=" => {
                    tc_binop_with_env(&rest, Type::Int, Type::Int, Type::Bool, env)
                }
                "let" => tc_let_with_env(&rest, env),
                "if" => tc_if_with_env(&rest, env),
                _ => Err(TypeCheckError),
            }
        }
        lexpr::Value::Symbol(x) => match &x[..] {
            "true" => Ok(Type::Bool),
            "false" => Ok(Type::Bool),
            s => match env.get(s) {
                Some(val) => Ok(val.clone()),
                None => Err(TypeCheckError),
            },
        },
        _ => Err(TypeCheckError),
    }
}

pub fn type_check(value: &lexpr::Value) -> Result<Type, TypeCheckError> {
    tc_with_env(value, &mut HashMap::new())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn typecheck_prims() {
        let exp = lexpr::from_str("3").unwrap();
        assert_eq!(type_check(&exp).unwrap(), Type::Int);

        let exp = lexpr::from_str("-497").unwrap();
        assert_eq!(type_check(&exp).unwrap(), Type::Int);

        let exp = lexpr::from_str("#t").unwrap();
        assert_eq!(type_check(&exp).unwrap(), Type::Bool);

        let exp = lexpr::from_str("#f").unwrap();
        assert_eq!(type_check(&exp).unwrap(), Type::Bool);

        let exp = lexpr::from_str("true").unwrap();
        assert_eq!(type_check(&exp).unwrap(), Type::Bool);

        let exp = lexpr::from_str("false").unwrap();
        assert_eq!(type_check(&exp).unwrap(), Type::Bool);

        let exp = lexpr::from_str("\"true\"").unwrap();
        assert_eq!(type_check(&exp).unwrap(), Type::Str);

        let exp = lexpr::from_str("\"foo\"").unwrap();
        assert_eq!(type_check(&exp).unwrap(), Type::Str);

        let exp = lexpr::from_str("\"\"").unwrap();
        assert_eq!(type_check(&exp).unwrap(), Type::Str);
    }

    #[test]
    fn typecheck_binops_happy() {
        let exp = lexpr::from_str("(+ 3 5)").unwrap();
        assert_eq!(type_check(&exp).unwrap(), Type::Int);

        let exp = lexpr::from_str("(* 3 5)").unwrap();
        assert_eq!(type_check(&exp).unwrap(), Type::Int);

        let exp = lexpr::from_str("(- 3 5)").unwrap();
        assert_eq!(type_check(&exp).unwrap(), Type::Int);

        let exp = lexpr::from_str("(/ 3 5)").unwrap();
        assert_eq!(type_check(&exp).unwrap(), Type::Int);

        let exp = lexpr::from_str("(+ (* 4 5) (- 5 2))").unwrap();
        assert_eq!(type_check(&exp).unwrap(), Type::Int);
    }

    #[test]
    fn typecheck_binops_sad() {
        let exp = lexpr::from_str("(+ 3 true)").unwrap();
        assert_eq!(type_check(&exp).unwrap_err(), TypeCheckError);

        let exp = lexpr::from_str("(* 3 true)").unwrap();
        assert_eq!(type_check(&exp).unwrap_err(), TypeCheckError);

        let exp = lexpr::from_str(r#"(- false "hello")"#).unwrap();
        assert_eq!(type_check(&exp).unwrap_err(), TypeCheckError);

        let exp = lexpr::from_str(r#"(/ "foo" 3)"#).unwrap();
        assert_eq!(type_check(&exp).unwrap_err(), TypeCheckError);
    }

    #[test]
    fn typecheck_let_happy() {
        let exp = lexpr::from_str("(let ([x 23]) (+ x 24))").unwrap();
        assert_eq!(type_check(&exp).unwrap(), Type::Int);
    }

    #[test]
    fn typecheck_let_sad() {
        // one variable missing
        let exp = lexpr::from_str("(let ([x 23]) (+ x y))").unwrap();
        assert_eq!(type_check(&exp).unwrap_err(), TypeCheckError);
    }

    #[test]
    fn typecheck_if_happy() {
        let exp = lexpr::from_str(r#"(if (< 3 4) 1 -1)"#).unwrap();
        assert_eq!(type_check(&exp).unwrap(), Type::Int);
    }

    #[test]
    fn typecheck_if_sad() {
        // invalid predicate
        let exp = lexpr::from_str(r#"(if 3 4 5)"#).unwrap();
        assert_eq!(type_check(&exp).unwrap_err(), TypeCheckError);

        // consequent and alternate do not match
        let exp = lexpr::from_str(r#"(if (< 3 4) "hello" 5)"#).unwrap();
        assert_eq!(type_check(&exp).unwrap_err(), TypeCheckError);
    }
}
