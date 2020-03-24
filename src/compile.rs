use crate::closure_convert::{closure_convert, ClosureConvertError};
use crate::common::{Expr, Prog, TypedExpr};
use crate::generate_code::CodeGenerateError;
use crate::lambda_lift::{lambda_lift, LambdaLiftError};
use crate::pack_elim::pack_elim_prog;
use crate::record_elim::{record_elim_prog, RecordElimError};
use crate::type_check::{type_check, type_check_prog, TypeCheckError};

// #[derive(Clone, Debug)]
// pub struct CompilationError(Box<std::error::Error>);

// #[derive(Clone, Debug)]
// pub enum CompilationError {
//     TypeCheck(TypeCheckError),
//     ClosureConvert(ClosureConvertError),
//     LambdaLift(LambdaLiftError),
//     RecordElim(RecordElimError),
//     CodeGenerate(CodeGenerateError),
// }

// // Allows other errors to wrap this one
// impl std::error::Error for CompilationError {
//     fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
//         // Generic error, underlying cause isn't tracked.
//         // None
//         match self {
//             Self::TypeCheck(err) => Some(err),
//             Self::ClosureConvert(err) => Some(err),
//             Self::LambdaLift(err) => Some(err),
//             Self::RecordElim(err) => Some(err),
//             Self::CodeGenerate(err) => Some(err),
//         }
//     }
// }

// impl std::fmt::Display for CompilationError {
//     fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//         match self {
//             Self::TypeCheck(err) => write!(f, "CompilationError: {}", err),
//             Self::ClosureConvert(err) => write!(f, "CompilationError: {}", err),
//             Self::LambdaLift(err) => write!(f, "CompilationError: {}", err),
//             Self::RecordElim(err) => write!(f, "CompilationError: {}", err),
//             Self::CodeGenerate(err) => write!(f, "CompilationError: {}", err),
//         }
//     }
// }

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
