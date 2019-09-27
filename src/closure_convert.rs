use crate::parser::{BinOp, Expr, Type};
use im_rc::Vector;

#[derive(Clone, Debug)]
pub struct ClosureConvertError(String);

impl From<&str> for ClosureConvertError {
    fn from(message: &str) -> Self {
        ClosureConvertError(String::from(message))
    }
}

impl std::fmt::Display for ClosureConvertError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "ClosureConvertError: {}", self.0)
    }
}

// allows other errors to wrap this one
// see https://doc.rust-lang.org/rust-by-example/error/multiple_error_types/define_error_type.html
impl std::error::Error for ClosureConvertError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        // Generic error, underlying cause isn't tracked.
        None
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum CType {
    Int,
    Bool,
    Str,
    List(Box<CType>),
    Func(Vector<CType>, Box<CType>), // array of input types, and a return type
    Env(Vector<CType>),
}

#[derive(Clone, Debug, PartialEq)]
/// Represents a closure-converted expression.
pub enum CExpr {
    // operator, arg1, arg2
    Binop(BinOp, Box<CExpr>, Box<CExpr>),

    // predicate, consequent, alternate
    If(Box<CExpr>, Box<CExpr>, Box<CExpr>),

    // variable bindings, body
    Let(Vector<(String, CExpr)>, Box<CExpr>),

    // arg names/types (environment should be first argument), return type, body
    Lambda(Vector<(String, CType)>, CType, Box<CExpr>),

    // func, arguments
    FnApp(Box<CExpr>, Vector<CExpr>),

    // lambda, environment
    Closure(Box<CExpr>, Box<CExpr>),

    // environment mapping
    Env(Vector<(String, CExpr)>),

    EnvGet(Box<Expr>, String),

    Begin(Vector<CExpr>),
    Set(String, Box<CExpr>),
    Cons(Box<CExpr>, Box<CExpr>),
    Car(Box<CExpr>),
    Cdr(Box<CExpr>),
    IsNull(Box<CExpr>),
    Null(CType),

    Sym(String),
    Num(i64),
    Bool(bool),
    Str(String),
}

pub fn closure_convert(exp: &Expr) -> Result<CExpr, ClosureConvertError> {
    Err(ClosureConvertError::from("Unimplemented."))
}
