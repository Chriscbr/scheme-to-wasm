use crate::closure_convert::closure_convert;
use crate::common::{Expr, Prog, TypedExpr};
use crate::lambda_lift::lambda_lift;
use crate::record_elim::record_elim_prog;
use crate::type_check::{type_check, type_check_prog};

/// Perform a complete compilation from an Expr to a Prog - in other words, all
/// all compiler passes before code generation.
///
/// Parsing the original input string (code) into an Expr must be handled
/// separately, using `parse::parse()`.
///
/// TODO: We could return a custom "wrapping" error type instead of
/// Box<dyn Error>, but I'm not sure if this is necessary or what is
/// best form.
pub fn compile_exp(exp: &Expr) -> Result<Prog<TypedExpr>, Box<dyn std::error::Error>> {
    // the type information is not currently used for closure conversion, but
    // we want to type check just to catch errors early on
    type_check(&exp)?;

    let cc_exp = closure_convert(&exp)?;
    let prog = lambda_lift(&cc_exp)?;
    let typed_prog = type_check_prog(&prog)?;
    let re_typed_prog = record_elim_prog(&typed_prog)?;
    Ok(re_typed_prog)
}
