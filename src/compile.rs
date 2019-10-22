use crate::closure_convert::closure_convert;
use crate::generate_code::generate_code;
use crate::lambda_lift::lambda_lift;
use crate::parse::parse;
use crate::type_check::type_check;
use proc_macro2::TokenStream;

// TODO: rework to produce some kind of fallback if any step fails,
// e.g. as done in
// https://docs.rs/lexpr-macros/0.2.0/src/lexpr_macros/lib.rs.html
pub fn compile(input: lexpr::Value) -> TokenStream {
    let exp = parse(&input).unwrap();
    type_check(&exp).unwrap();
    let cc_exp = closure_convert(&exp).unwrap();
    type_check(&cc_exp).unwrap();
    let prog = lambda_lift(&cc_exp).unwrap();
    generate_code(&prog).unwrap()
}
