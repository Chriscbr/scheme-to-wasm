use crate::common::{generate_func_name, Expr, ExprKind, Prog};
use im_rc::{vector, Vector};

#[derive(Clone, Debug)]
pub struct LambdaLiftError(String);

// Allows other errors to wrap this one
impl std::error::Error for LambdaLiftError {}

impl From<&str> for LambdaLiftError {
    fn from(message: &str) -> Self {
        LambdaLiftError(String::from(message))
    }
}

impl std::fmt::Display for LambdaLiftError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "LambdaLiftError: {}", self.0)
    }
}

fn ll_array(
    exps: &Vector<Expr>,
    fns: &mut Vector<(String, Expr)>,
) -> Result<Vector<Expr>, LambdaLiftError> {
    exps.iter()
        .map(|exp| ll(&exp, fns))
        .collect::<Result<Vector<Expr>, LambdaLiftError>>()
}

fn ll(exp: &Expr, fns: &mut Vector<(String, Expr)>) -> Result<Expr, LambdaLiftError> {
    match &*exp.kind {
        ExprKind::Num(_) => Ok(exp.clone()),
        ExprKind::Bool(_) => Ok(exp.clone()),
        ExprKind::Str(_) => Ok(exp.clone()),
        ExprKind::Id(_) => Ok(exp.clone()),
        ExprKind::Binop(op, exp1, exp2) => {
            let lexp1 = ll(&exp1, fns)?;
            let lexp2 = ll(&exp2, fns)?;
            Ok(Expr::new(ExprKind::Binop(*op, lexp1, lexp2)))
        }
        ExprKind::If(pred, cons, alt) => {
            let lpred = ll(&pred, fns)?;
            let lcons = ll(&cons, fns)?;
            let lalt = ll(&alt, fns)?;
            Ok(Expr::new(ExprKind::If(lpred, lcons, lalt)))
        }
        ExprKind::Let(bindings, body) => {
            let lbindings = bindings
                .iter()
                .map(|binding| {
                    let lexp = ll(&binding.1, fns)?;
                    Ok((binding.0.clone(), lexp))
                })
                .collect::<Result<Vector<(String, Expr)>, LambdaLiftError>>()?;
            let lbody = ll(&body, fns)?;
            Ok(Expr::new(ExprKind::Let(lbindings, lbody)))
        }
        ExprKind::Lambda(params, ret_typ, body) => {
            let lbody = ll(body, fns)?;
            let new_lambda = Expr::new(ExprKind::Lambda(params.clone(), ret_typ.clone(), lbody));
            let func_name = generate_func_name();
            fns.push_back((func_name.clone(), new_lambda));
            Ok(Expr::new(ExprKind::Id(func_name)))
        }
        ExprKind::FnApp(func, args) => {
            let lfunc = ll(&func, fns)?;
            let largs = ll_array(&args, fns)?;
            Ok(Expr::new(ExprKind::FnApp(lfunc, largs)))
        }
        ExprKind::Record(bindings) => {
            let lbindings = bindings
                .iter()
                .map(|binding| {
                    let lexp = ll(&binding.1, fns)?;
                    Ok((binding.0.clone(), lexp))
                })
                .collect::<Result<Vector<(String, Expr)>, LambdaLiftError>>()?;
            Ok(Expr::new(ExprKind::Record(lbindings)))
        }
        ExprKind::RecordGet(record, key) => {
            let lrecord = ll(&record, fns)?;
            Ok(Expr::new(ExprKind::RecordGet(lrecord, key.clone())))
        }
        ExprKind::Begin(exps) => {
            let lexps = ll_array(&exps, fns)?;
            Ok(Expr::new(ExprKind::Begin(lexps)))
        }
        ExprKind::Set(var_name, exp) => {
            let lexp = ll(&exp, fns)?;
            Ok(Expr::new(ExprKind::Set(var_name.clone(), lexp)))
        }
        ExprKind::Cons(first, second) => {
            let lfirst = ll(&first, fns)?;
            let lsecond = ll(&second, fns)?;
            Ok(Expr::new(ExprKind::Cons(lfirst, lsecond)))
        }
        ExprKind::Car(exp) => {
            let lexp = ll(&exp, fns)?;
            Ok(Expr::new(ExprKind::Car(lexp)))
        }
        ExprKind::Cdr(exp) => {
            let lexp = ll(&exp, fns)?;
            Ok(Expr::new(ExprKind::Cdr(lexp)))
        }
        ExprKind::IsNull(exp) => {
            let lexp = ll(&exp, fns)?;
            Ok(Expr::new(ExprKind::IsNull(lexp)))
        }
        ExprKind::Null(_typ) => Ok(exp.clone()),
        ExprKind::Tuple(exps) => {
            let lexps = ll_array(&exps, fns)?;
            Ok(Expr::new(ExprKind::Tuple(lexps)))
        }
        ExprKind::TupleGet(tup, key) => {
            let ltup = ll(&tup, fns)?;
            Ok(Expr::new(ExprKind::TupleGet(ltup, *key)))
        }
        ExprKind::Pack(val, sub, exist) => {
            let lval = ll(&val, fns)?;
            Ok(Expr::new(ExprKind::Pack(lval, sub.clone(), exist.clone())))
        }
        ExprKind::Unpack(var, package, type_sub, body) => {
            let lpackage = ll(&package, fns)?;
            let lbody = ll(&body, fns)?;
            Ok(Expr::new(ExprKind::Unpack(
                var.clone(),
                lpackage,
                *type_sub,
                lbody,
            )))
        }
    }
}

pub fn lambda_lift(exp: &Expr) -> Result<Prog<Expr>, LambdaLiftError> {
    let mut fns: Vector<(String, Expr)> = vector![];
    let lifted_exp = ll(exp, &mut fns)?;
    Ok(Prog {
        fns,
        exp: lifted_exp,
    })
}
