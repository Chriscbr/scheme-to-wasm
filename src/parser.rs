use crate::Type;
use im_rc::Vector;

pub enum Operator {
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

pub enum Expr {
    Binop(Operator, Box<Expr>, Box<Expr>), // operator, arg1, arg2
    If(Box<Expr>, Box<Expr>, Box<Expr>),   // pred, consequent, alternate
    Let(Vector<(Expr, Expr)>, Box<Expr>),  // variable bindings, body
    Lambda(Vector<(Expr, Type)>, Type, Box<Expr>), // arg names/types, return type, body
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

// TODO: write unit tests
pub fn parse_binop(op: &str, arg1: &lexpr::Value, arg2: &lexpr::Value) -> Result<Expr, ParseError> {
    let exp1 = match parse(arg1) {
        Ok(val) => Box::from(val),
        Err(e) => return Err(e),
    };
    let exp2 = match parse(arg2) {
        Ok(val) => Box::from(val),
        Err(e) => return Err(e),
    };
    let operator = match op {
        "and" => Operator::And,
        "or" => Operator::Or,
        "+" => Operator::Add,
        "-" => Operator::Subtract,
        "*" => Operator::Multiply,
        "/" => Operator::Divide,
        "<" => Operator::LessThan,
        ">" => Operator::GreaterThan,
        "<=" => Operator::LessOrEqual,
        ">=" => Operator::GreaterOrEqual,
        "=" => Operator::EqualTo,
        "concat" => Operator::Concat,
        _ => return Err(ParseError::from("Unrecognized binary operator.")),
    };
    Ok(Expr::Binop(operator, exp1, exp2))
}

// TODO: write unit tests
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
                    "if" => {
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
                    } // TODO: implement
                    // "let" =>
                    // "lambda" =>
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
