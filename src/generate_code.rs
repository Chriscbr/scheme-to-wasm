use crate::common::{ExprKind, Prog};
use proc_macro2::TokenStream;
use quote::quote;

#[derive(Clone, Debug)]
pub struct GenerateCodeError(String);

impl From<&str> for GenerateCodeError {
    fn from(message: &str) -> Self {
        GenerateCodeError(String::from(message))
    }
}

impl std::fmt::Display for GenerateCodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "GenerateCodeError: {}", self.0)
    }
}

// allows other errors to wrap this one
// see https://doc.rust-lang.org/rust-by-example/error/multiple_error_types/define_error_type.html
impl std::error::Error for GenerateCodeError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        // Generic error, underlying cause isn't tracked.
        None
    }
}

pub fn generate_code(prog: &Prog) -> Result<TokenStream, GenerateCodeError> {
    match &*prog.exp.kind {
        ExprKind::Binop(op, exp1, exp2) => unimplemented!(),
        ExprKind::If(pred, cons, alt) => unimplemented!(),
        ExprKind::Let(bindings, body) => unimplemented!(),
        ExprKind::Lambda(params, ret_type, body) => unimplemented!(),
        ExprKind::FnApp(func, args) => unimplemented!(),
        ExprKind::Record(bindings) => unimplemented!(),
        ExprKind::RecordGet(record, key) => unimplemented!(),
        ExprKind::Begin(exps) => unimplemented!(),
        ExprKind::Set(var_name, exp) => unimplemented!(),
        ExprKind::Cons(first, second) => unimplemented!(),
        ExprKind::Car(exp) => unimplemented!(),
        ExprKind::Cdr(exp) => unimplemented!(),
        ExprKind::IsNull(exp) => unimplemented!(),
        ExprKind::Null(typ) => unimplemented!(),
        ExprKind::Tuple(exps) => unimplemented!(),
        ExprKind::TupleGet(tup, key) => unimplemented!(),
        ExprKind::Pack(val, sub, exist) => unimplemented!(),
        ExprKind::Unpack(var, package, typ_sub, body) => unimplemented!(),
        ExprKind::Id(val) => unimplemented!(),
        ExprKind::Num(val) => Ok({
            quote! {
                h.alloc(#val)
            }
        }),
        ExprKind::Bool(val) => unimplemented!(),
        ExprKind::Str(val) => unimplemented!(),
    }
}
