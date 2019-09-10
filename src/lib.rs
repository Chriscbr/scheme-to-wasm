#[derive(Debug, PartialEq)]
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
                    let e1 = type_check(&rest[0]).unwrap();
                    let e2 = type_check(&rest[1]).unwrap();
                    if e1 == Type::Int && e2 == Type::Int {
                        return Ok(Type::Int);
                    } else {
                        return Err(TypeCheckError);
                    }
                },
                _ => Err(TypeCheckError)
            }
        },
        lexpr::Value::Symbol(x) => {
            match &x[..] {
                "true" => Ok(Type::Bool),
                "false" => Ok(Type::Bool),
                _ => Err(TypeCheckError)
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
    }
}
