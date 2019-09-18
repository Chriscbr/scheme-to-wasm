use crate::Type;
use im_rc::{vector, Vector};

#[derive(Clone, Debug)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    LessOrEqual,
    GreaterOrEqual,
    EqualTo,
    And,
    Or,
    Concat,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Binop(Op, Box<Expr>, Box<Expr>),        // operator, arg1, arg2
    If(Box<Expr>, Box<Expr>, Box<Expr>),    // pred, consequent, alternate
    Let(Vector<(String, Expr)>, Box<Expr>), // variable bindings, body
    Lambda(Vector<(String, Type)>, Type, Box<Expr>), // arg names/types, return type, body
    Begin(Vector<Expr>),
    Set(Box<Expr>, Box<Expr>),
    Cons(Box<Expr>),
    Car(Box<Expr>),
    Cdr(Box<Expr>),
    IsNull(Box<Expr>),
    Null(Type),
    FnApp(Box<Expr>, Vector<Expr>), // func, arguments
    Symbol(String),
    Number(i64),
    Boolean(bool),
    Str(String),
}

#[derive(Clone, Debug)]
pub struct ParseError(String);

impl From<&str> for ParseError {
    fn from(message: &str) -> Self {
        ParseError(String::from(message))
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "ParseError: {}", self.0)
    }
}

// allows other errors to wrap this one
// see https://doc.rust-lang.org/rust-by-example/error/multiple_error_types/define_error_type.html
impl std::error::Error for ParseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        // Generic error, underlying cause isn't tracked.
        None
    }
}

//
// Helper functions
//

fn convert_annotation_to_type(annotation: &lexpr::Value) -> Result<Type, ParseError> {
    match annotation {
        lexpr::Value::Symbol(val) => match val.as_ref() {
            "int" => Ok(Type::Int),
            "bool" => Ok(Type::Bool),
            "string" => Ok(Type::Str),
            _ => Err(ParseError::from(
                "Type annotation not recognized as a valid type.",
            )),
        },
        lexpr::Value::Cons(_) => {
            // an array, ex. [Symbol(->), Symbol(int), Symbol(int), Symbol(bool)]
            let lst_vec = match annotation.to_vec() {
                Some(vec) => vec,
                None => {
                    return Err(ParseError::from(
                        "Type annotation for function is not a valid list.",
                    ))
                }
            };
            // ensure that the function annotation has at least -> and a return type as elements
            if lst_vec.is_empty() {
                return Err(ParseError::from("Type annotation is missing values."));
            }
            match lst_vec[0].as_symbol() {
                Some("->") => {
                    if lst_vec.len() < 2 {
                        return Err(ParseError::from(
                            "Type annotation for function is missing values.",
                        ));
                    }
                    let input_types: Result<Vec<Type>, ParseError> = lst_vec
                        [1..(lst_vec.len() - 1)]
                        .iter()
                        .map(|val| convert_annotation_to_type(val))
                        .collect();
                    let input_types_unwrapped = match input_types {
                        Ok(val) => val,
                        Err(e) => return Err(e),
                    };
                    let return_type = match convert_annotation_to_type(&lst_vec[lst_vec.len() - 1])
                    {
                        Ok(val) => val,
                        Err(e) => return Err(e),
                    };
                    Ok(Type::Func(
                        Vector::from(input_types_unwrapped),
                        Box::from(return_type),
                    ))
                }
                Some("list") => {
                    if lst_vec.len() != 2 {
                        return Err(ParseError::from(
                            "Type annotation for list has incorrect number of values.",
                        ));
                    }
                    let lst_type = match convert_annotation_to_type(&lst_vec[1]) {
                        Ok(typ) => typ,
                        Err(e) => return Err(e),
                    };
                    Ok(Type::List(Box::from(lst_type)))
                }
                _ => Err(ParseError::from(
                    r#"Type annotation for function does not have "->" or "list" as first symbol."#,
                )),
            }
        }
        _ => Err(ParseError::from(
            "Type annotation is invalid or is missing.",
        )),
    }
}

fn unwrap_lambda_args(args: &lexpr::Value) -> Result<Vector<(String, Type)>, ParseError> {
    let arg_list = match args.to_vec() {
        Some(vec) => vec,
        None => {
            return Err(ParseError::from(
                "Lambda arguments are not in a valid list.",
            ))
        }
    };
    arg_list
        .iter()
        .map(|arg| {
            // [x : int] as a vec
            let arg_vec = match arg.to_vec() {
                Some(vec) => vec,
                None => return Err(ParseError::from("Lambda argument is not a valid list.")),
            };
            if arg_vec.len() != 3 {
                return Err(ParseError::from(
                    "Lambda argument is missing values or contains extra values.",
                ));
            }

            // check there is a separator
            let separator = match arg_vec[1].as_symbol() {
                Some(val) => val,
                None => {
                    return Err(ParseError::from(
                        "Lambda argument does not contain the correct : separator.",
                    ))
                }
            };
            if separator != ":" {
                return Err(ParseError::from(
                    "Lambda argument does not contain the correct : separator.",
                ));
            }

            let arg_name = match arg_vec[0].as_symbol() {
                Some(val) => val,
                None => {
                    return Err(ParseError::from(
                        "Lambda argument does not have a valid name.",
                    ))
                }
            };
            let arg_type = match convert_annotation_to_type(&arg_vec[2]) {
                Ok(typ) => typ,
                Err(e) => return Err(e),
            };
            Ok((String::from(arg_name), arg_type))
        })
        .collect()
}

//
// Parsing functions
//

fn parse_binop(op: &str, arg1: &lexpr::Value, arg2: &lexpr::Value) -> Result<Expr, ParseError> {
    let exp1 = match parse(arg1) {
        Ok(val) => Box::from(val),
        Err(e) => return Err(e),
    };
    let exp2 = match parse(arg2) {
        Ok(val) => Box::from(val),
        Err(e) => return Err(e),
    };
    let operator = match op {
        "and" => Op::And,
        "or" => Op::Or,
        "+" => Op::Add,
        "-" => Op::Subtract,
        "*" => Op::Multiply,
        "/" => Op::Divide,
        "<" => Op::LessThan,
        ">" => Op::GreaterThan,
        "<=" => Op::LessOrEqual,
        ">=" => Op::GreaterOrEqual,
        "=" => Op::EqualTo,
        "concat" => Op::Concat,
        _ => return Err(ParseError::from("Unrecognized binary operator.")),
    };
    Ok(Expr::Binop(operator, exp1, exp2))
}

fn parse_if(rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    if rest.len() == 3 {
        parse(&rest[0]).and_then(|predicate| {
            parse(&rest[1]).and_then(|consequent| {
                parse(&rest[2]).and_then(|alternate| {
                    Ok(Expr::If(
                        Box::from(predicate),
                        Box::from(consequent),
                        Box::from(alternate),
                    ))
                })
            })
        })
    } else {
        Err(ParseError::from(
            "If expression has incorrect number of arguments.",
        ))
    }
}

fn parse_let(rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    if rest.len() != 2 {
        return Err(ParseError::from(
            "Let expression has incorrect number of arguments.",
        ));
    }
    // Assert that the bindings is a proper list
    let bindings = match rest[0].to_vec() {
        Some(vec) => vec,
        None => {
            return Err(ParseError::from(
                "Let expression bindings are not in a proper list.",
            ))
        }
    };

    let parsed_bindings: Result<Vector<(String, Expr)>, ParseError> = Vector::from(bindings)
        .iter()
        .map(|binding| {
            let binding_vec = match binding.to_vec() {
                Some(vec) => vec,
                None => return Err(ParseError::from("Let binding is not a valid list.")),
            };
            if binding_vec.len() != 2 {
                return Err(ParseError::from(
                    "Let binding is missing values or contains extra values.",
                ));
            }

            binding_vec[0]
                .as_symbol()
                .ok_or_else(|| ParseError::from("Let binding does not have a valid name."))
                .and_then(|binding_name| {
                    parse(&binding_vec[1])
                        .and_then(|binding_val| Ok((String::from(binding_name), binding_val)))
                })
        })
        .collect();
    parsed_bindings.and_then(|bindings_vec| {
        parse(&rest[1]).and_then(|body_expr| Ok(Expr::Let(bindings_vec, Box::from(body_expr))))
    })
}

fn parse_lambda(rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    if rest.len() != 4 {
        return Err(ParseError::from(
            "Lambda expression has incorrect number of arguments. Perhaps you are missing the return type?",
        ));
    }
    let args = match unwrap_lambda_args(&rest[0]) {
        Ok(val) => val,
        Err(e) => return Err(e),
    };

    // check there is a separator
    let separator = match rest[1].as_symbol() {
        Some(val) => val,
        None => return Err(ParseError::from("Lambda expression does not have a separator between the arguments list and return type.")),
    };
    if separator != ":" {
        return Err(ParseError::from("Lambda expression does not have the correct separator : between the arguments list and return type."));
    }

    // get the (annotated) return type
    let ret_type = match convert_annotation_to_type(&rest[2]) {
        Ok(typ) => typ,
        Err(e) => return Err(e),
    };
    let body = match parse(&rest[3]) {
        Ok(typ) => typ,
        Err(e) => return Err(e),
    };

    Ok(Expr::Lambda(args, ret_type, Box::from(body)))
}

pub fn parse(value: &lexpr::Value) -> Result<Expr, ParseError> {
    match value {
        lexpr::Value::Number(x) => match x.as_i64() {
            Some(val) => Ok(Expr::Number(val)),
            None => Err(ParseError::from(
                "Invalid number found (must be a 64-bit integer).",
            )),
        },
        lexpr::Value::Bool(x) => Ok(Expr::Boolean(*x)),
        lexpr::Value::String(x) => Ok(Expr::Str((*x).to_string())),
        lexpr::Value::Cons(_) => {
            let lst = match value.to_vec() {
                Some(vec) => vec,
                None => return Err(ParseError::from("Cons expression is not a valid list.")),
            };
            // our language currently does not assign () to any meaning
            if lst.is_empty() {
                return Err(ParseError::from("Empty list found."));
            }
            let lst_parts = lst.split_at(1);
            let first = &(lst_parts.0)[0];
            let rest = lst_parts.1;

            match first.as_symbol() {
                Some(val) => match val {
                    "and" | "or" | "+" | "*" | "-" | "/" | ">" | "<" | ">=" | "<=" | "="
                    | "concat" => {
                        if rest.len() == 2 {
                            parse_binop(val, &rest[0], &rest[1])
                        } else {
                            Err(ParseError::from(
                                "Binary operator has incorrect number of sub-expressions.",
                            ))
                        }
                    }
                    "if" => parse_if(&rest),
                    "let" => parse_let(&rest),
                    "lambda" => parse_lambda(&rest),
                    // TODO: implement
                    // "begin" =>
                    // "set!" =>
                    // "cons" =>
                    // "car" =>
                    // "cdr" =>
                    // "null?" =>
                    // "null" =>
                    // expr => // function application
                    _ => Err(ParseError::from("TODO: Unimplemented.")),
                },
                // TODO: change to handle function application case
                None => Err(ParseError::from("TODO: Unimplemented.")),
            }
        }
        lexpr::Value::Symbol(x) => match &x[..] {
            "true" => Ok(Expr::Boolean(true)),
            "false" => Ok(Expr::Boolean(false)),
            symbol => Ok(Expr::Symbol(symbol.to_string())),
        },
        _ => Err(ParseError::from("Unrecognized form of expression found.")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_convert_annotations() {
        let exp = lexpr::from_str("int").unwrap();
        assert_eq!(convert_annotation_to_type(&exp).unwrap(), Type::Int);

        let exp = lexpr::from_str("bool").unwrap();
        assert_eq!(convert_annotation_to_type(&exp).unwrap(), Type::Bool);

        let exp = lexpr::from_str("string").unwrap();
        assert_eq!(convert_annotation_to_type(&exp).unwrap(), Type::Str);

        let exp = lexpr::from_str("(list int)").unwrap();
        assert_eq!(
            convert_annotation_to_type(&exp).unwrap(),
            Type::List(Box::from(Type::Int))
        );

        let exp = lexpr::from_str("(list (list int))").unwrap();
        assert_eq!(
            convert_annotation_to_type(&exp).unwrap(),
            Type::List(Box::from(Type::List(Box::from(Type::Int))))
        );

        let exp = lexpr::from_str("(-> int)").unwrap();
        assert_eq!(
            convert_annotation_to_type(&exp).unwrap(),
            Type::Func(vector![], Box::from(Type::Int))
        );

        let exp = lexpr::from_str("(-> int int)").unwrap();
        assert_eq!(
            convert_annotation_to_type(&exp).unwrap(),
            Type::Func(vector![Type::Int], Box::from(Type::Int))
        );

        let exp = lexpr::from_str("(-> string int bool)").unwrap();
        assert_eq!(
            convert_annotation_to_type(&exp).unwrap(),
            Type::Func(vector![Type::Str, Type::Int], Box::from(Type::Bool))
        );

        let exp = lexpr::from_str("(-> (-> int int bool) int int bool)").unwrap();
        assert_eq!(
            convert_annotation_to_type(&exp).unwrap(),
            Type::Func(
                vector![
                    Type::Func(vector![Type::Int, Type::Int], Box::from(Type::Bool)),
                    Type::Int,
                    Type::Int
                ],
                Box::from(Type::Bool)
            )
        );
    }
}
