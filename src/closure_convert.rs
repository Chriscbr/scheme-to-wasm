use crate::parser::{BinOp, Expr, Type};
use im_rc::Vector;
use std::borrow::Borrow;

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

    // lambda, environment
    Closure(Box<CExpr>, Box<CExpr>),

    // func, arguments
    ClosureApp(Box<CExpr>, Vector<CExpr>),

    // environment mapping
    Env(Vector<(String, CExpr)>),

    // environment, key
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

fn closure_convert_type(typ: &Type) -> Result<CType, ClosureConvertError> {
    match typ {
        Type::Int => Ok(CType::Int),
        Type::Bool => Ok(CType::Bool),
        Type::Str => Ok(CType::Str),
        Type::List(x) => {
            closure_convert_type(x).and_then(|ctype| Ok(CType::List(Box::from(ctype))))
        },
        Type::Func(arg_types, ret_type) => {
            let carg_types: Result<Vector<CType>, ClosureConvertError> = arg_types
                .iter()
                .map(|arg_typ| closure_convert_type(arg_typ))
                .collect();

            carg_types.and_then(|carg_types| {
                closure_convert_type((*ret_type).borrow())
                    .and_then(|cret_type| Ok(CType::Func(carg_types, Box::from(cret_type))))
            })
        },
    }
}

fn closure_convert_bindings(
    bindings: &Vector<(String, Expr)>,
) -> Result<Vector<(String, CExpr)>, ClosureConvertError> {
    bindings
        .iter()
        .map(|pair| closure_convert(&pair.1).and_then(|cexp| Ok((pair.0.clone(), cexp))))
        .collect()
}

pub fn closure_convert(exp: &Expr) -> Result<CExpr, ClosureConvertError> {
    match exp {
        Expr::Num(x) => Ok(CExpr::Num(*x)),
        Expr::Bool(x) => Ok(CExpr::Bool(*x)),
        Expr::Str(x) => Ok(CExpr::Str(x.clone())),
        Expr::Sym(x) => Ok(CExpr::Sym(x.clone())),
        Expr::Binop(op, arg1, arg2) => closure_convert(arg1).and_then(|carg1| {
            closure_convert(arg2)
                .and_then(|carg2| Ok(CExpr::Binop(*op, Box::from(carg1), Box::from(carg2))))
        }),
        Expr::If(pred, cons, alt) => closure_convert(pred).and_then(|cpred| {
            closure_convert(cons).and_then(|ccons| {
                closure_convert(alt).and_then(|calt| {
                    Ok(CExpr::If(
                        Box::from(cpred),
                        Box::from(ccons),
                        Box::from(calt),
                    ))
                })
            })
        }),
        Expr::Let(bindings, body) => closure_convert_bindings(bindings).and_then(|cbindings| {
            closure_convert(body).and_then(|cbody| Ok(CExpr::Let(cbindings, Box::from(cbody))))
        }),
        // TODO: Lambda
        Expr::Lambda(params, ret_typ, body) => Err(ClosureConvertError::from("Unimplemented.")),
        Expr::Begin(exps) => {
            let cexps: Result<Vector<CExpr>, ClosureConvertError> = exps
                .iter()
                .map(|subexp| closure_convert(&subexp).and_then(|csubexp| Ok(csubexp)))
                .collect();
            cexps.and_then(|cexps| Ok(CExpr::Begin(cexps)))
        },
        Expr::Set(sym, val) => {
            return closure_convert(val)
                .and_then(|cval| Ok(CExpr::Set(sym.clone(), Box::from(cval))));
        },
        Expr::Cons(first, rest) => closure_convert(first).and_then(|cfirst| {
            closure_convert(rest)
                .and_then(|crest| Ok(CExpr::Cons(Box::from(cfirst), Box::from(crest))))
        }),
        Expr::Car(val) => closure_convert(val).and_then(|cval| Ok(CExpr::Car(Box::from(cval)))),
        Expr::Cdr(val) => closure_convert(val).and_then(|cval| Ok(CExpr::Cdr(Box::from(cval)))),
        Expr::IsNull(val) => {
            return closure_convert(val).and_then(|cval| Ok(CExpr::IsNull(Box::from(cval))));
        },
        Expr::Null(typ) => closure_convert_type(typ).and_then(|ctyp| Ok(CExpr::Null(ctyp))),
        // TODO: FnApp
        Expr::FnApp(func, args) => Err(ClosureConvertError::from("Unimplemented.")),
    }
}
