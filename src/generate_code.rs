use crate::common::{BinOp, Expr, ExprKind, Prog};
use proc_macro2::TokenStream;
use quote::quote;

#[derive(Clone, Debug)]
pub struct GenerateCodeError(String);

// Allows other errors to wrap this one
impl std::error::Error for GenerateCodeError {}

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

pub fn generate_code_exp(exp: &Expr) -> Result<TokenStream, GenerateCodeError> {
    match &*exp.kind {
        ExprKind::Binop(op, exp1, exp2) => Ok({
            let code1 = generate_code_exp(exp1)?;
            let code2 = generate_code_exp(exp2)?;
            match op {
                BinOp::Add => quote! { (#code1 + #code2) },
                BinOp::Subtract => quote! { (#code1 - #code2) },
                BinOp::Multiply => quote! { (#code1 * #code2) },
                BinOp::Divide => quote! { (#code1 / #code2) },
                BinOp::LessThan => quote! { BoolVal::from(#code1 < #code2) },
                BinOp::GreaterThan => quote! { BoolVal::from(#code1 > #code2) },
                BinOp::LessOrEqual => quote! { BoolVal::from(#code1 <= #code2) },
                BinOp::GreaterOrEqual => quote! { BoolVal::from(#code1 >= #code2) },
                BinOp::EqualTo => quote! { BoolVal::from(#code1 == #code2) },

                // && and || cannot be operator overloaded in Rust
                // so a special form, i.e. some native Rust syntax with short
                // circuiting behavior needs to be used
                BinOp::And => quote! {
                    { if { #code1.get_value() } { #code2 } else { BoolVal::from(false) } }
                },
                BinOp::Or => quote! {
                    { if { #code1.get_value() } { BoolVal::from(true) } else { #code2 } }
                },
                BinOp::Concat => quote! { concat(#code1, #code2) },
            }
        }),
        ExprKind::If(pred, cons, alt) => Ok({
            let pred_code = generate_code_exp(pred)?;
            let cons_code = generate_code_exp(cons)?;
            let alt_code = generate_code_exp(alt)?;
            quote! { {
                if { #pred_code.get_value() } {
                    #cons_code
                } else {
                    #alt_code
                }
            } }
        }),
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
                IntVal::from(#val)
            }
        }),
        ExprKind::Bool(val) => Ok({
            quote! {
                BoolVal::from(#val)
            }
        }),
        ExprKind::Str(val) => Ok({
            quote! {
                StrVal::from(String::from(#val))
            }
        }),
    }
}

// TODO: use prog.fns
pub fn generate_code_prog(prog: &Prog) -> Result<TokenStream, GenerateCodeError> {
    generate_code_exp(&prog.exp)
}
