use crate::closure_convert::closure_convert;
use crate::common::{Expr, Prog, TypedExpr};
use crate::lambda_lift::lambda_lift;
use crate::pack_elim::pack_elim_prog;
use crate::record_elim::record_elim_prog;
use crate::type_check::{type_check, type_check_prog};

/// Perform a complete compilation from an Expr to a Prog - in other words, all
/// all compiler passes before code generation.
///
/// Parsing the original input string (code) into an Expr must be handled
/// separately, using `parse::parse()`.
///
/// TODO: We could return a custom "wrapping" error type, as defined above,
/// but I'm not sure if this is entirely needed, or what is best form.
pub fn compile_exp(exp: &Expr) -> Result<Prog<TypedExpr>, Box<dyn std::error::Error>> {
    type_check(&exp)?; // the type information is not currently used for closure conversion
    let cc_exp = closure_convert(&exp)?;
    let prog = lambda_lift(&cc_exp)?;
    let typed_prog = type_check_prog(&prog)?;
    let re_typed_prog = record_elim_prog(&typed_prog)?;
    let pe_typed_prog = pack_elim_prog(&re_typed_prog)?;
    Ok(pe_typed_prog)
}
