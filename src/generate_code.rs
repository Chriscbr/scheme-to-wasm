use crate::common::{BinOp, Expr, ExprKind, Prog};
use crate::types::Type;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

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

// TODO: clean up / remove function if unneeded
pub fn generate_code_typ(typ: &Type) -> Result<TokenStream, GenerateCodeError> {
    match typ {
        Type::Int => Ok(quote! { IntVal }),
        Type::Bool => Ok(quote! { BoolVal }),
        Type::Str => Ok(quote! { StrVal }),
        Type::List(inner_typ) => Ok({
            let inner_code = generate_code_typ(inner_typ)?;
            quote! {
                ListVal<#inner_code>
            }
        }),
        Type::Func(typs, ret_typ) => unimplemented!(),
        Type::Tuple(typs) => unimplemented!(),
        Type::Record(fields) => unimplemented!(),
        Type::Exists(bound_var, inner_typ) => unimplemented!(),
        Type::TypeVar(x) => unimplemented!(),
        Type::Unknown => panic!("Trying to convert an unknown type to Rust!"),
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
                BinOp::LessThan => quote! { BoolVal::new(#code1 < #code2) },
                BinOp::GreaterThan => quote! { BoolVal::new(#code1 > #code2) },
                BinOp::LessOrEqual => quote! { BoolVal::new(#code1 <= #code2) },
                BinOp::GreaterOrEqual => quote! { BoolVal::new(#code1 >= #code2) },
                BinOp::EqualTo => quote! { BoolVal::new(#code1 == #code2) },

                // && and || cannot be operator overloaded in Rust
                // so a special form, i.e. some native Rust syntax with short
                // circuiting behavior needs to be used
                BinOp::And => quote! {
                    { if { #code1.get_value() } { #code2 } else { BoolVal::new(false) } }
                },
                BinOp::Or => quote! {
                    { if { #code1.get_value() } { BoolVal::new(true) } else { #code2 } }
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
        ExprKind::Let(bindings, body) => Ok({
            let binding_stmts = bindings
                .iter()
                .map(|bind| {
                    let bind_exp_code = generate_code_exp(&bind.1)?;
                    let name = format_ident!("{}", bind.0);
                    Ok(quote! {
                        let #name = #bind_exp_code;
                    })
                })
                .collect::<Result<Vec<TokenStream>, GenerateCodeError>>()?;
            let body_code = generate_code_exp(body)?;
            quote! { {
                #(#binding_stmts)*
                { #body_code }
            } }
        }),
        ExprKind::Lambda(params, ret_type, body) => unimplemented!(),
        ExprKind::FnApp(func, args) => unimplemented!(),
        ExprKind::Record(bindings) => unimplemented!(),
        ExprKind::RecordGet(record, key) => unimplemented!(),
        ExprKind::Begin(exps) => unimplemented!(),
        ExprKind::Set(var_name, exp) => unimplemented!(),
        ExprKind::Cons(car, cdr) => Ok({
            let car_code = generate_code_exp(car)?;
            let cdr_code = generate_code_exp(cdr)?;
            quote! { {
                ListVal::Cons(#car_code, Box::from(#cdr_code))
            }}
        }),
        ExprKind::Car(exp) => Ok({
            let exp_code = generate_code_exp(exp)?;
            quote! {
                #exp_code.get_car()
            }
        }),
        ExprKind::Cdr(exp) => Ok({
            let exp_code = generate_code_exp(exp)?;
            quote! {
                #exp_code.get_cdr()
            }
        }),
        ExprKind::IsNull(exp) => Ok({
            let exp_code = generate_code_exp(exp)?;
            quote! {
                #exp_code.is_null()
            }
        }),
        ExprKind::Null(typ) => Ok({
            let _typ_code = generate_code_typ(typ)?;
            // Rust does not require use to parameterize ListVal<T> since it
            // can be inferred.
            quote! {
                ListVal::Null
            }
        }),
        ExprKind::Tuple(exps) => unimplemented!(),
        ExprKind::TupleGet(tup, key) => unimplemented!(),
        ExprKind::Pack(val, sub, exist) => unimplemented!(),
        ExprKind::Unpack(var, package, typ_sub, body) => unimplemented!(),
        ExprKind::Id(val) => Ok({
            let name = format_ident!("{}", val);
            quote! {
                #name
            }
        }),
        ExprKind::Num(val) => Ok({
            quote! {
                IntVal::new(#val)
            }
        }),
        ExprKind::Bool(val) => Ok({
            quote! {
                BoolVal::new(#val)
            }
        }),
        ExprKind::Str(val) => Ok({
            quote! {
                StrVal::new(String::from(#val))
            }
        }),
    }
}

// TODO: use prog.fns
pub fn generate_code_prog(prog: &Prog) -> Result<TokenStream, GenerateCodeError> {
    generate_code_exp(&prog.exp)
}
