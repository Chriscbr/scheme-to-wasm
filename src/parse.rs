use crate::common::{BinOp, Expr, ExprKind};
use crate::types::Type;
use im_rc::Vector;
use std::num::ParseIntError;

#[derive(Clone, Debug)]
pub struct ParseError(String);

// Allows other errors to wrap this one
impl std::error::Error for ParseError {}

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

// Implement the conversion from `ParseIntError` to `ParseError`.
// This will be automatically called by `?` if a `ParseIntError`
// needs to be converted into a `ParseError`.
impl From<ParseIntError> for ParseError {
    fn from(err: ParseIntError) -> ParseError {
        ParseError(format!("ParseIntError: {}", err))
    }
}

fn check_separator(value: &lexpr::Value, expected: char) -> bool {
    match value.as_symbol() {
        Some(sep) => sep.len() == 1 && sep.chars().nth(0).unwrap() == expected,
        None => false,
    }
}

pub fn parse_type(annotation: &lexpr::Value) -> Result<Type, ParseError> {
    match annotation {
        lexpr::Value::Symbol(val) => match val.as_ref() {
            "int" => Ok(Type::Int),
            "bool" => Ok(Type::Bool),
            "string" => Ok(Type::Str),
            "unknown" => Ok(Type::Unknown),
            val => match val.chars().next() {
                Some('T') => Ok(Type::TypeVar(
                    val[1..val.len()]
                        .chars()
                        .collect::<String>()
                        .parse::<u64>()?,
                )),
                _ => Err(ParseError::from(
                    "Type annotation not recognized as a valid type.",
                )),
            },
        },
        lexpr::Value::Cons(_) => {
            let lst_vec = annotation
                .to_vec()
                .ok_or_else(|| "Type annotation is not a valid list.")?;
            // ensure that the function annotation has at least -> and a return type as elements
            if lst_vec.is_empty() {
                return Err(ParseError::from("Type annotation is missing values."));
            }
            match lst_vec[0].as_symbol() {
                Some("->") => parse_func_annotation(lst_vec),
                Some("list") => parse_list_annotation(lst_vec),
                Some("tuple") => parse_tuple_annotation(lst_vec),
                Some("record") => parse_record_annotation(lst_vec),
                Some("exists") => parse_exists_annotation(lst_vec),
                _ => Err(ParseError::from(
                    r#"Type annotation does not have "->", "tuple", or "list" as first symbol."#,
                )),
            }
        }
        _ => Err(ParseError::from(
            "Type annotation is invalid or is missing.",
        )),
    }
}

fn parse_func_annotation(lst_vec: Vec<lexpr::Value>) -> Result<Type, ParseError> {
    if lst_vec.len() < 2 {
        return Err(ParseError::from(
            "Type annotation for function is missing values.",
        ));
    }
    let input_types: Vec<Type> = lst_vec[1..(lst_vec.len() - 1)]
        .iter()
        .map(|val| parse_type(val))
        .collect::<Result<Vec<Type>, ParseError>>()?;
    let return_type = parse_type(&lst_vec[lst_vec.len() - 1])?;
    Ok(Type::Func(Vector::from(input_types), Box::new(return_type)))
}

fn parse_list_annotation(lst_vec: Vec<lexpr::Value>) -> Result<Type, ParseError> {
    if lst_vec.len() != 2 {
        return Err(ParseError::from(
            "Type annotation for list has incorrect number of values.",
        ));
    }
    let lst_type = parse_type(&lst_vec[1])?;
    Ok(Type::List(Box::new(lst_type)))
}

fn parse_tuple_annotation(lst_vec: Vec<lexpr::Value>) -> Result<Type, ParseError> {
    let tuple_types: Vec<Type> = lst_vec[1..(lst_vec.len())]
        .iter()
        .map(|val| parse_type(val))
        .collect::<Result<Vec<Type>, ParseError>>()?;
    Ok(Type::Tuple(Vector::from(tuple_types)))
}

fn parse_record_annotation(lst_vec: Vec<lexpr::Value>) -> Result<Type, ParseError> {
    let record_types: Vec<(String, Type)> = lst_vec[1..(lst_vec.len())]
        .iter()
        .map(|exp| match exp.to_vec() {
            Some(binding) => {
                if binding.len() != 3 {
                    return Err(ParseError::from(
                        "Record type binding has incorrect number of values.",
                    ));
                }
                let label = String::from(
                    binding[0]
                        .as_symbol()
                        .ok_or_else(|| "Record type label is not a valid name.")?,
                );
                if !check_separator(&binding[1], ':') {
                    return Err(ParseError::from(
                        "Record type annotation does not contain the correct : separator.",
                    ));
                }
                let typ = parse_type(&binding[2])?;

                Ok((label, typ))
            }
            None => Err(ParseError::from(
                "Record type binding is not a proper list of values.",
            )),
        })
        .collect::<Result<Vec<(String, Type)>, ParseError>>()?;
    Ok(Type::Record(Vector::from(record_types)))
}

fn parse_exists_annotation(lst_vec: Vec<lexpr::Value>) -> Result<Type, ParseError> {
    if lst_vec.len() != 3 {
        return Err(ParseError::from(
            "Type annotation for existential type has incorrect number of values.",
        ));
    }
    let type_var_str = lst_vec[1].as_symbol().ok_or_else(|| "Type annotation for existential type does not have a valid type variable in its first argument.")?;
    let type_var_num = match type_var_str.chars().next() {
        Some('T') => type_var_str[1..type_var_str.len()].chars().collect::<String>().parse::<u64>()?,
        _ => {
            return Err(ParseError::from(
                "Type annotation for existential type does not have a proper type variable starting with T.",
            ))
        }
    };
    let lst_type = parse_type(&lst_vec[2])?;
    Ok(Type::Exists(type_var_num, Box::new(lst_type)))
}

fn parse_array(exps: &[lexpr::Value]) -> Result<Vector<Expr>, ParseError> {
    exps.iter().map(|exp| parse(exp)).collect()
}

fn parse_binop(op: &str, rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    if rest.len() != 2 {
        return Err(ParseError::from(
            "Binary operator has incorrect number of sub-expressions.",
        ));
    }
    let exp1 = parse(&rest[0])?;
    let exp2 = parse(&rest[1])?;
    let operator = match op {
        "and" => BinOp::And,
        "or" => BinOp::Or,
        "+" => BinOp::Add,
        "-" => BinOp::Subtract,
        "*" => BinOp::Multiply,
        "/" => BinOp::Divide,
        "<" => BinOp::LessThan,
        ">" => BinOp::GreaterThan,
        "<=" => BinOp::LessOrEqual,
        ">=" => BinOp::GreaterOrEqual,
        "=" => BinOp::EqualTo,
        "concat" => BinOp::Concat,
        _ => return Err(ParseError::from("Unrecognized binary operator.")),
    };
    Ok(Expr::new(ExprKind::Binop(operator, exp1, exp2)))
}

fn parse_if(rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    if rest.len() != 3 {
        return Err(ParseError::from(
            "If expression has incorrect number of arguments.",
        ));
    }
    let predicate = parse(&rest[0])?;
    let consequent = parse(&rest[1])?;
    let alternate = parse(&rest[2])?;
    Ok(Expr::new(ExprKind::If(predicate, consequent, alternate)))
}

fn parse_let(rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    if rest.len() != 2 {
        return Err(ParseError::from(
            "Let expression has incorrect number of arguments.",
        ));
    }
    let bindings = rest[0]
        .to_vec()
        .ok_or_else(|| "Let expression bindings are not in a proper list.")?;
    let bindings_vec: Vector<(String, Expr)> = bindings
        .iter()
        .map(|binding| {
            let binding_vec = binding
                .to_vec()
                .ok_or_else(|| "Let binding is not a valid list.")?;
            if binding_vec.len() != 2 {
                return Err(ParseError::from(
                    "Let binding is missing values or contains extra values.",
                ));
            }
            let binding_name = binding_vec[0]
                .as_symbol()
                .ok_or_else(|| "Let binding does not have a valid name.")?;
            let binding_val = parse(&binding_vec[1])?;
            Ok((String::from(binding_name), binding_val))
        })
        .collect::<Result<Vector<(String, Expr)>, ParseError>>()?;
    let body = parse(&rest[1])?;
    Ok(Expr::new(ExprKind::Let(bindings_vec, body)))
}

fn parse_lambda(rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    if rest.len() != 4 {
        return Err(ParseError::from(
            "Lambda expression has incorrect number of arguments. Perhaps you are missing the return type?",
        ));
    }
    let args = unwrap_lambda_args(&rest[0])?;
    if !check_separator(&rest[1], ':') {
        return Err(ParseError::from("Lambda expression does not have the correct separator : between the arguments list and return type."));
    }
    let ret_type = parse_type(&rest[2])?;
    let body = parse(&rest[3])?;
    Ok(Expr::new(ExprKind::Lambda(args, ret_type, body)))
}

fn unwrap_lambda_args(args: &lexpr::Value) -> Result<Vector<(String, Type)>, ParseError> {
    let arg_list = args
        .to_vec()
        .ok_or_else(|| "Lambda arguments are not in a valid list.")?;
    arg_list
        .iter()
        .map(|arg| {
            // [x : int] as a vec
            let arg_vec = arg
                .to_vec()
                .ok_or_else(|| "Lambda argument is not a valid list.")?;
            if arg_vec.len() != 3 {
                return Err(ParseError::from(
                    "Lambda argument is missing values or contains extra values.",
                ));
            }
            let arg_name = arg_vec[0]
                .as_symbol()
                .ok_or_else(|| "Lambda argument does not have a valid name.")?;
            if !check_separator(&arg_vec[1], ':') {
                return Err(ParseError::from(
                    "Lambda argument does not contain the correct : separator.",
                ));
            }
            let arg_type = parse_type(&arg_vec[2])?;
            Ok((String::from(arg_name), arg_type))
        })
        .collect()
}

fn parse_make_record(rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    let bindings_vec: Vector<(String, Expr)> = rest
        .iter()
        .map(|binding| {
            let binding_vec = binding
                .to_vec()
                .ok_or_else(|| "Value in make-record expression is not a valid list.")?;
            if binding_vec.len() != 2 {
                return Err(ParseError::from(
                    "Value in make-record expression is incomplete or contains extra values.",
                ));
            }
            let binding_name = binding_vec[0]
                .as_symbol()
                .ok_or_else(|| "Make-record binding does not have a valid name.")?;
            let binding_val = parse(&binding_vec[1])?;
            Ok((String::from(binding_name), binding_val))
        })
        .collect::<Result<Vector<(String, Expr)>, ParseError>>()?;
    Ok(Expr::new(ExprKind::Record(bindings_vec)))
}

fn parse_get_record(rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    if rest.len() != 2 {
        return Err(ParseError::from(
            "Env-ref expression has incorrect number of arguments.",
        ));
    }
    let bindings = parse(&rest[0])?;
    let key = rest[1]
        .as_symbol()
        .ok_or_else(|| "Env-ref key is not a valid identifier.")?;
    Ok(Expr::new(ExprKind::RecordGet(bindings, String::from(key))))
}

fn parse_begin(rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    if rest.is_empty() {
        return Err(ParseError::from("Begin expression has no arguments."));
    }
    let exps = parse_array(&rest)?;
    Ok(Expr::new(ExprKind::Begin(exps)))
}

fn parse_set_bang(rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    if rest.len() != 2 {
        return Err(ParseError::from(
            "Set expression has incorrect number of arguments.",
        ));
    }
    let var = rest[0]
        .as_symbol()
        .ok_or_else(|| "Set expression does not have a symbol as its first argument.")?;
    let new_val = parse(&rest[1])?;
    Ok(Expr::new(ExprKind::Set(String::from(var), new_val)))
}

fn parse_cons(rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    if rest.len() != 2 {
        return Err(ParseError::from(
            "Cons expression has incorrect number of arguments.",
        ));
    }
    let first = parse(&rest[0])?;
    let second = parse(&rest[1])?;
    Ok(Expr::new(ExprKind::Cons(first, second)))
}

fn parse_car(rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    if rest.len() != 1 {
        return Err(ParseError::from(
            "Car expression has incorrect number of arguments.",
        ));
    }
    let pair = parse(&rest[0])?;
    Ok(Expr::new(ExprKind::Car(pair)))
}

fn parse_cdr(rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    if rest.len() != 1 {
        return Err(ParseError::from(
            "Cdr expression has incorrect number of arguments.",
        ));
    }
    let pair = parse(&rest[0])?;
    Ok(Expr::new(ExprKind::Cdr(pair)))
}

fn parse_is_null(rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    if rest.len() != 1 {
        return Err(ParseError::from(
            "Null? expression has incorrect number of arguments.",
        ));
    }
    let val = parse(&rest[0])?;
    Ok(Expr::new(ExprKind::IsNull(val)))
}

fn parse_null(rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    if rest.len() != 1 {
        return Err(ParseError::from(
            "Null expression has incorrect number of arguments.",
        ));
    }
    let val = parse_type(&rest[0])?;
    Ok(Expr::new(ExprKind::Null(val)))
}

fn parse_func(first: &lexpr::Value, rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    let func = parse(first)?;
    let args = parse_array(rest)?;
    Ok(Expr::new(ExprKind::FnApp(func, args)))
}

fn parse_make_tuple(rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    let exps = parse_array(&rest)?;
    Ok(Expr::new(ExprKind::Tuple(exps)))
}

fn parse_get_tuple(rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    if rest.len() != 2 {
        return Err(ParseError::from(
            "Tuple-ref expression has incorrect number of arguments.",
        ));
    }
    let tuple = parse(&rest[0])?;
    let key = rest[1]
        .as_u64()
        .ok_or_else(|| "Second argument in tuple-ref is not an integer.")?;
    Ok(Expr::new(ExprKind::TupleGet(tuple, key)))
}

fn parse_pack(rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    if rest.len() != 3 {
        return Err(ParseError::from(
            "Pack expression has incorrect number of arguments.",
        ));
    }
    let package = parse(&rest[0])?;
    let typ_sub = parse_type(&rest[1])?;
    let exist_typ = parse_type(&rest[2])?;
    Ok(Expr::new(ExprKind::Pack(package, typ_sub, exist_typ)))
}

fn parse_unpack(rest: &[lexpr::Value]) -> Result<Expr, ParseError> {
    if rest.len() != 2 {
        return Err(ParseError::from(
            "Unpack expression has incorrect number of arguments.",
        ));
    }
    let inner_lst: Vec<lexpr::Value> = rest[0]
        .to_vec()
        .ok_or_else(|| "First argument in unpack expression is malformed.")?;
    if inner_lst.len() != 3 {
        return Err(ParseError::from(
            "First argument in unpack expression has incorrect number of values.",
        ));
    }
    let var_name = String::from(inner_lst[0].as_symbol().ok_or_else(|| {
        "Unpack expression does not contain an identifier to bind the packed expression to."
    })?);
    let package: Expr = parse(&inner_lst[1])?;
    let typ_var_symbol = inner_lst[2]
        .as_symbol()
        .ok_or_else(|| "Third argument in unpack is not a type variable.")?;
    let typ_var = match typ_var_symbol.chars().next() {
        Some('T') => typ_var_symbol[1..typ_var_symbol.len()]
            .chars()
            .collect::<String>()
            .parse::<u64>()?,
        _ => {
            return Err(ParseError::from(
                "Third argument in unpack is not a valid type variable.",
            ))
        }
    };
    let body: Expr = parse(&rest[1])?;
    Ok(Expr::new(ExprKind::Unpack(
        var_name, package, typ_var, body,
    )))
}

pub fn parse(value: &lexpr::Value) -> Result<Expr, ParseError> {
    match value {
        lexpr::Value::Number(x) => match x.as_i64() {
            Some(val) => {
                if val >= i32::min_value() as i64 && val <= i32::max_value() as i64 {
                    Ok(Expr::new(ExprKind::Num(val as i32)))
                } else {
                    Err(ParseError::from(
                        "Invalid number found (must be a 32-bit integer).",
                    ))
                }
            }
            None => Err(ParseError::from(
                "Invalid number found (must be a 32-bit integer).",
            )),
        },
        lexpr::Value::Bool(x) => Ok(Expr::new(ExprKind::Bool(*x))),
        lexpr::Value::String(x) => Ok(Expr::new(ExprKind::Str((*x).to_string()))),
        lexpr::Value::Cons(_) => {
            let lst = value
                .to_vec()
                .ok_or_else(|| "Cons expression is not a valid list.")?;
            // The source language currently does not assign () to any meaning
            if lst.is_empty() {
                return Err(ParseError::from("Empty list found."));
            }

            // We decide how to parse a list based on the first element in the expression;
            // in most cases, just the rest of the vector (i.e. the arguments) will get passed
            // to the individual parsing functions
            let lst_parts = lst.split_at(1);
            let first = &(lst_parts.0)[0];
            let rest = lst_parts.1;

            match first.as_symbol() {
                Some(val) => match val {
                    "and" | "or" | "+" | "*" | "-" | "/" | ">" | "<" | ">=" | "<=" | "="
                    | "concat" => parse_binop(val, &rest),
                    "if" => parse_if(&rest),
                    "let" => parse_let(&rest),
                    "lambda" => parse_lambda(&rest),
                    "make-record" => parse_make_record(&rest),
                    "record-ref" => parse_get_record(&rest),
                    "begin" => parse_begin(&rest),
                    "set!" => parse_set_bang(&rest),
                    "cons" => parse_cons(&rest),
                    "car" => parse_car(&rest),
                    "cdr" => parse_cdr(&rest),
                    "null?" => parse_is_null(&rest),
                    "null" => parse_null(&rest),
                    "make-tuple" => parse_make_tuple(&rest),
                    "tuple-ref" => parse_get_tuple(&rest),
                    "pack" => parse_pack(&rest),
                    "unpack" => parse_unpack(&rest),
                    _ => parse_func(&first, &rest),
                },
                None => parse_func(&first, &rest),
            }
        }
        lexpr::Value::Symbol(x) => match &x[..] {
            "true" => Ok(Expr::new(ExprKind::Bool(true))),
            "false" => Ok(Expr::new(ExprKind::Bool(false))),
            symbol => Ok(Expr::new(ExprKind::Id(symbol.to_string()))),
        },
        _ => Err(ParseError::from("Unrecognized form of expression found.")),
    }
}
