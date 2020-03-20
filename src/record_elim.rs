// TODO: implement a more modular / functional way of handling compiler pass
// ExprKind case matching... should be able to define a function which just
// does transformations on the cases I am interested in, and does some form
// of default recursion in other cases

use crate::common::{ExprKind, Prog, TypedExpr};
use crate::type_check::check_lambda_type_with_inputs;
use crate::types::Type;
use im_rc::Vector;

#[derive(Clone, Debug)]
pub struct RecordElimError(String);

// Allows other errors to wrap this one
impl std::error::Error for RecordElimError {}

impl From<&str> for RecordElimError {
    fn from(message: &str) -> Self {
        RecordElimError(String::from(message))
    }
}

impl std::fmt::Display for RecordElimError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "RecordElimError: {}", self.0)
    }
}

fn rc_type(typ: &Type) -> Result<Type, RecordElimError> {
    match typ {
        Type::Int => Ok(Type::Int),
        Type::Bool => Ok(Type::Bool),
        Type::Str => Ok(Type::Str),
        Type::List(base_type) => {
            let rc_base_type = rc_type(base_type)?;
            Ok(Type::List(Box::new(rc_base_type)))
        }
        Type::Func(in_types, ret_type) => {
            let rc_in_types = rc_type_array(in_types)?;
            let rc_ret_type = rc_type(ret_type)?;
            Ok(Type::Func(rc_in_types, Box::new(rc_ret_type)))
        }
        Type::Tuple(types) => {
            let rc_types = rc_type_array(types)?;
            Ok(Type::Tuple(rc_types))
        }
        Type::Record(bindings) => {
            let mut rc_bindings = bindings
                .iter()
                .map(|pair| Ok((pair.0.clone(), rc_type(&pair.1)?)))
                .collect::<Result<Vec<(String, Type)>, RecordElimError>>()?;
            rc_bindings.sort_unstable_by(|a, b| a.0.cmp(&b.0));
            let type_vec: Vec<Type> = rc_bindings
                .iter()
                .map(|(_field, typ)| typ.clone())
                .collect();
            Ok(Type::Tuple(Vector::from(type_vec)))
        }
        Type::Exists(type_var, base_type) => {
            let rc_base_type = rc_type(base_type)?;
            Ok(Type::Exists(*type_var, Box::new(rc_base_type)))
        }
        Type::TypeVar(x) => Ok(Type::TypeVar(*x)),
        Type::Unknown => Ok(Type::Unknown),
    }
}

fn rc_type_array(types: &Vector<Type>) -> Result<Vector<Type>, RecordElimError> {
    types.iter().map(|typ| Ok(rc_type(typ)?)).collect()
}

/// Convert an expression into one without record or record-ref expressions.
///
/// Conversion is performed by replacing records with corresponding tuples,
/// and replacing record-ref with tuple-ref expressions.
///
/// Expression must be type checked (annotated with types) before being passed
/// in. After conversion, the output expression of this function will have all
/// type annotations removed, so it should be re-type-checked.
pub fn record_elim_exp(exp: &TypedExpr) -> Result<TypedExpr, RecordElimError> {
    rc(exp)
}

/// Converts a program into one without record or record-ref expressions.
///
/// See `record_elim_exp` for more specific details.
pub fn record_elim_prog(prog: &Prog<TypedExpr>) -> Result<Prog<TypedExpr>, RecordElimError> {
    let rexp = rc(&prog.exp)?;
    let rfns = prog
        .fns
        .iter()
        .map(|(name, func)| Ok((name.clone(), rc(&func)?)))
        .collect::<Result<Vector<(String, TypedExpr)>, RecordElimError>>()?;
    Ok(Prog {
        exp: rexp,
        fns: rfns,
    })
}

fn rc_bindings(
    bindings: &Vector<(String, TypedExpr)>,
) -> Result<Vector<(String, TypedExpr)>, RecordElimError> {
    bindings
        .iter()
        .map(|pair| rc(&pair.1).and_then(|cexp| Ok((pair.0.clone(), cexp))))
        .collect()
}

fn rc(exp: &TypedExpr) -> Result<TypedExpr, RecordElimError> {
    match &*exp.kind {
        ExprKind::Num(x) => Ok(TypedExpr::new(Type::Int, ExprKind::Num(*x))),
        ExprKind::Bool(x) => Ok(TypedExpr::new(Type::Bool, ExprKind::Bool(*x))),
        ExprKind::Str(x) => Ok(TypedExpr::new(Type::Str, ExprKind::Str(x.clone()))),
        ExprKind::Id(x) => Ok(TypedExpr::new(rc_type(&exp.typ)?, ExprKind::Id(x.clone()))),
        ExprKind::Binop(op, arg1, arg2) => {
            let rarg1 = rc(&arg1)?;
            let rarg2 = rc(&arg2)?;
            Ok(TypedExpr::new(
                exp.typ.clone(),
                ExprKind::Binop(*op, rarg1, rarg2),
            ))
        }
        ExprKind::If(pred, cons, alt) => {
            let rpred = rc(&pred)?;
            let rcons = rc(&cons)?;
            let ralt = rc(&alt)?;
            Ok(TypedExpr::new(
                rcons.typ.clone(),
                ExprKind::If(rpred, rcons, ralt),
            ))
        }
        ExprKind::Let(bindings, body) => {
            let rbindings = rc_bindings(&bindings)?;
            let rbody = rc(&body)?;
            Ok(TypedExpr::new(
                rbody.typ.clone(),
                ExprKind::Let(rbindings, rbody),
            ))
        }
        ExprKind::Lambda(params, ret_type, body) => {
            let rparams = params
                .iter()
                .map(|pair| {
                    let rtype = rc_type(&pair.1);
                    match rtype {
                        Ok(val) => Ok((pair.0.clone(), val)),
                        Err(e) => Err(e),
                    }
                })
                .collect::<Result<Vector<(String, Type)>, RecordElimError>>()?;
            let rbody = rc(&body)?;
            let rret_type = rc_type(&ret_type)?;
            let param_types: Vector<Type> = rparams.iter().map(|pair| pair.1.clone()).collect();
            let lambda_type = Type::Func(param_types, Box::new(rret_type.clone()));
            Ok(TypedExpr::new(
                lambda_type,
                ExprKind::Lambda(rparams, rret_type, rbody),
            ))
        }
        ExprKind::Begin(exps) => {
            let rexps = exps
                .iter()
                .map(|subexp| rc(&subexp))
                .collect::<Result<Vector<TypedExpr>, RecordElimError>>()?;
            let mut inner_types = rexps
                .iter()
                .map(|rexp| rexp.typ.clone())
                .collect::<Vector<Type>>();
            Ok(TypedExpr::new(
                inner_types.remove(inner_types.len() - 1),
                ExprKind::Begin(rexps),
            ))
        }
        ExprKind::Set(id, val) => {
            let rval = rc(&val)?;
            Ok(TypedExpr::new(
                rval.typ.clone(),
                ExprKind::Set(id.clone(), rval),
            ))
        }
        ExprKind::Cons(first, rest) => {
            let rfirst = rc(&first)?;
            let rrest = rc(&rest)?;
            Ok(TypedExpr::new(
                rrest.typ.clone(),
                ExprKind::Cons(rfirst, rrest),
            ))
        }
        ExprKind::Car(val) => {
            let rval = rc(&val)?;
            match rval.typ.clone() {
                Type::List(boxed_type) => Ok(TypedExpr::new(*boxed_type, ExprKind::Car(rval))),
                _ => Err(RecordElimError::from(
                    "Expression in car is not a list type.",
                )),
            }
        }
        ExprKind::Cdr(val) => {
            let rval = rc(&val)?;
            Ok(TypedExpr::new(rval.typ.clone(), ExprKind::Cdr(rval)))
        }
        ExprKind::IsNull(val) => {
            let rval = rc(&val)?;
            Ok(TypedExpr::new(Type::Bool, ExprKind::IsNull(rval)))
        }
        ExprKind::Null(typ) => Ok(TypedExpr::new(
            Type::List(Box::new(typ.clone())),
            ExprKind::Null(rc_type(&typ)?),
        )),
        ExprKind::Tuple(exps) => {
            let rexps = exps
                .iter()
                .map(|subexp| rc(&subexp))
                .collect::<Result<Vector<TypedExpr>, RecordElimError>>()?;
            let inner_types = rexps
                .iter()
                .map(|typed_exp| typed_exp.typ.clone())
                .collect::<Vector<Type>>();
            Ok(TypedExpr::new(
                Type::Tuple(inner_types),
                ExprKind::Tuple(rexps),
            ))
        }
        ExprKind::TupleGet(tuple, key) => {
            let rtuple = rc(&tuple)?;
            match rtuple.typ.clone() {
                Type::Tuple(vec) => {
                    if (*key as usize) < vec.len() {
                        let elem_type = vec[*key as usize].clone();
                        Ok(TypedExpr::new(elem_type, ExprKind::TupleGet(rtuple, *key)))
                    } else {
                        Err(RecordElimError::from(
                            "Value in tuple-ref is too large for the provided tuple.",
                        ))
                    }
                }
                _ => Err(RecordElimError::from(
                    "First expression in tuple-ref is not a tuple.",
                )),
            }
        }
        ExprKind::Record(bindings) => {
            let rbindings = rc_bindings(&bindings)?;
            let mut bindings_vec: Vec<(String, TypedExpr)> = rbindings.iter().cloned().collect();
            bindings_vec.sort_unstable_by(|a, b| a.0.cmp(&b.0));
            let exp_vec: Vector<TypedExpr> = bindings_vec
                .iter()
                .map(|(_field, exp)| exp.clone())
                .collect();
            let inner_types = exp_vec
                .iter()
                .map(|typed_exp| typed_exp.typ.clone())
                .collect::<Vector<Type>>();
            Ok(TypedExpr::new(
                Type::Tuple(inner_types),
                ExprKind::Tuple(exp_vec),
            ))
        }
        ExprKind::RecordGet(record, key) => {
            let tuple = rc(&record)?;
            let tuple_index = get_field_index(record, key)?;
            match tuple.typ.clone() {
                Type::Tuple(vec) => {
                    if (tuple_index as usize) < vec.len() {
                        let elem_type = vec[tuple_index as usize].clone();
                        Ok(TypedExpr::new(
                            elem_type,
                            ExprKind::TupleGet(tuple, tuple_index),
                        ))
                    } else {
                        Err(RecordElimError::from(
                            "Value in tuple-ref is too large for the provided tuple.",
                        ))
                    }
                }
                _ => Err(RecordElimError::from(
                    "First expression in record-ref (after conversion) is not a tuple.",
                )),
            }
        }
        ExprKind::Pack(val, sub, exist) => {
            let rval = rc(&val)?;
            let rsub = rc_type(&sub)?;
            let rexist = rc_type(&exist)?;
            Ok(TypedExpr::new(
                rexist.clone(),
                ExprKind::Pack(rval, rsub, rexist),
            ))
        }
        ExprKind::Unpack(var, package, typ_sub, body) => {
            let rpackage = rc(&package)?;
            let rbody = rc(&body)?;
            Ok(TypedExpr::new(
                rbody.typ.clone(),
                ExprKind::Unpack(var.clone(), rpackage, *typ_sub, rbody),
            ))
        }
        ExprKind::FnApp(func, args) => {
            let rfunc = rc(&func)?;
            let rargs = args
                .iter()
                .map(|arg| rc(&arg))
                .collect::<Result<Vector<TypedExpr>, RecordElimError>>()?;
            let rargs_types = rargs
                .iter()
                .map(|rarg| rarg.typ.clone())
                .collect::<Vector<Type>>();
            let apply_type = match check_lambda_type_with_inputs(&rfunc.typ, &rargs_types) {
                Ok(val) => val,
                Err(e) => {
                    return Err(RecordElimError(format!(
                        "Error in check_lambda_type_with_inputs: {}",
                        e
                    )))
                }
            };
            Ok(TypedExpr::new(apply_type, ExprKind::FnApp(rfunc, rargs)))
        }
    }
}

fn get_field_index(record: &TypedExpr, field: &str) -> Result<u32, RecordElimError> {
    let mut fields_vec: Vec<(String, Type)> = match &record.typ {
        Type::Record(fields) => Ok(fields.iter().cloned().collect()),
        _ => Err(RecordElimError::from(
            "Type annotation for record has incorrect type.",
        )),
    }?;
    fields_vec.sort_unstable_by(|a, b| a.0.cmp(&b.0));
    fields_vec
        .iter()
        .position(|pair| pair.0 == field)
        .map(|val| val as u32)
        .ok_or_else(|| RecordElimError::from("Field not found in record."))
}
