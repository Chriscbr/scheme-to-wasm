use std::collections::HashMap;
use std::hash::BuildHasher;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Str,
}

#[derive(Clone, Debug)]
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

pub fn type_check(value: &lexpr::Value) -> Result<Type, TypeCheckError> {
    type_check_with_env(value, &mut HashMap::new())
}

pub fn type_check_with_env<S: BuildHasher>(value: &lexpr::Value, env: &mut HashMap<String, Type, S>) -> Result<Type, TypeCheckError> {
    match value {
        lexpr::Value::Number(_) => Ok(Type::Int),
        lexpr::Value::Bool(_) => Ok(Type::Bool),
        lexpr::Value::String(_) => Ok(Type::Str),
        lexpr::Value::Cons(_) => {
            if !value.is_list() {
                return Err(TypeCheckError);
            }
            let lst = value.to_vec().unwrap();
            let lst_parts = lst.split_at(1);
            let first = lst_parts.0;
            let rest = lst_parts.1;
            if !(first[0]).is_symbol() {
                return Err(TypeCheckError);
            }
            let operator = first[0].as_symbol().unwrap();
            match operator {
                "+" | "*" | "-" | "/" => {
                    let e1 = type_check_with_env(&rest[0], env).unwrap();
                    let e2 = type_check_with_env(&rest[1], env).unwrap();
                    if e1 == Type::Int && e2 == Type::Int {
                        Ok(Type::Int)
                    } else {
                        Err(TypeCheckError)
                    }
                },
                "let" => {
                    let let_list = &rest[0]; // ([x 23]) as value
                    // assume there is just one variable defined in let def list
                    if !let_list.is_list() {
                        return Err(TypeCheckError);
                    }
                    let let_list = let_list.to_vec().unwrap(); // ([x 23]) as vec
                    let first_def = &let_list[0]; // [x 23] as value
                    if !first_def.is_list() {
                        return Err(TypeCheckError);
                    }
                    let first_def = first_def.to_vec().unwrap(); // [x 23] as vec
                    if !first_def[0].is_symbol() {
                        return Err(TypeCheckError);
                    }
                    // let symbol = 
                    let exp_type = type_check_with_env(&first_def[1], env).unwrap();
                    env.insert(String::from(first_def[0].as_symbol().unwrap()), exp_type);
                    let body = &rest[1]; // (+ x 3)
                    dbg!(body);
                    // dbg!(env);
                    type_check_with_env(body, dbg!(env))
                }
                _ => Err(TypeCheckError)
            }
        },
        lexpr::Value::Symbol(x) => {
            match &x[..] {
                "true" => Ok(Type::Bool),
                "false" => Ok(Type::Bool),
                s => {
                    if env.contains_key(s) {
                        Ok(env.get(s).unwrap().clone())
                    } else {
                        Err(TypeCheckError)
                    }
                }
            }
        }
        _ => Err(TypeCheckError)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn typecheck_prims() {
        let exp = lexpr::from_str("3").unwrap();
        assert_eq!(type_check(&exp).unwrap(), Type::Int);

        let exp = lexpr::from_str("-4").unwrap();
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
    fn typecheck_binops() {
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
    fn typecheck_let() {
        let exp = lexpr::from_str("(let ([x 23]) (+ x 24))").unwrap();
        assert_eq!(type_check(&exp).unwrap(), Type::Int);
    }
}
