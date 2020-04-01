/// This module contains an assortment of functions for transforming Type,
/// Expr, and TypedExpr structs that aim to eliminate the need for
/// re-implementing recursion on these data structures.
use crate::common::{ExprKind, Prog, TypedExpr};
use crate::type_check::validate_lambda_type;
use crate::types::Type;

use im_rc::Vector;

/// Performs a transformation on a type annotation, provided a function for
/// transforming individual types for a handful of cases.
///
/// For example, the passed-in function `transform_type` could perform a
/// transformation on just Type::String cases, switching it to some other type,
/// by returning Some(Type), and then punt on the rest of the cases by
/// returning None. This way all of the manual logic for how to recurse into
/// the children of different type annotations does not have to repeated for
/// different type transformations.
pub fn transform_type_recursive<E, G>(typ: &Type, transform_type: G) -> Result<Type, E>
where
    E: std::error::Error,
    G: Fn(&Type) -> Option<Result<Type, E>> + Copy,
{
    if let Some(ttype) = transform_type(&typ) {
        return ttype;
    }
    match typ {
        Type::Int => Ok(Type::Int),
        Type::Bool => Ok(Type::Bool),
        Type::Str => Ok(Type::Str),
        Type::List(base_type) => {
            let tbase_type = transform_type_recursive(base_type, transform_type)?;
            Ok(Type::List(Box::new(tbase_type)))
        }
        Type::Func(in_types, ret_type) => {
            let tin_types = transform_type_array(in_types, transform_type)?;
            let tret_type = transform_type_recursive(ret_type, transform_type)?;
            Ok(Type::Func(tin_types, Box::new(tret_type)))
        }
        Type::Tuple(types) => {
            let ttypes = transform_type_array(types, transform_type)?;
            Ok(Type::Tuple(ttypes))
        }
        Type::Record(bindings) => {
            let tbindings = bindings
                .iter()
                .map(|(name, inner_type)| {
                    Ok((
                        name.clone(),
                        transform_type_recursive(inner_type, transform_type)?,
                    ))
                })
                .collect::<Result<Vector<(String, Type)>, E>>()?;
            Ok(Type::Record(tbindings))
        }
        Type::Exists(type_var, base_type) => {
            let tbase_type = transform_type_recursive(base_type, transform_type)?;
            Ok(Type::Exists(*type_var, Box::new(tbase_type)))
        }
        Type::TypeVar(x) => Ok(Type::TypeVar(*x)),
        Type::Unknown => Ok(Type::Unknown),
    }
}

fn transform_type_array<E, G>(types: &Vector<Type>, transform_type: G) -> Result<Vector<Type>, E>
where
    E: std::error::Error,
    G: Fn(&Type) -> Option<Result<Type, E>> + Copy,
{
    types
        .iter()
        .map(|typ| Ok(transform_type_recursive(typ, transform_type)?))
        .collect()
}

/// Performs a transformation on a typed AST, provided a function for
/// transforming expressions and a function for transforming types.
///
/// In general, when choosing how to construct a `transform_exp` to pass in,
/// the function should choose some ExprKind cases to transform, and
/// some ExprKind cases to leave as normal (but still recurse on). For example,
/// the Record Elimination compiler pass will have special logic to return
/// Some(Result) for the cases ExprKind::Record and ExprKind::RecordGet, but
/// in all other cases it will return None.
///
/// By doing so, we will still recursively apply the desired transformation
/// everywhere in the tree, but the individualized logic for recursing into
/// children of different kinds of ExprKind nodes does not have to be
/// duplicated for every compiler pass.
///
/// Technical note:
/// The error type parameter E must also implement From<&'a str> currently
/// in order to ensure we can construct errors that are that not specific
/// to the particular transformation, e.g. in the case that we can't
/// type check an expression like (tuple-ref (make-tuple 3 4 5) 10) since 10
/// is out of the range of 0..2. It's possible we could substitute this
/// with a custom trait that all compiler passes must implement, but for now
/// this solution is the most simple and reasonable.
pub fn transform_typed_exp_recursive<'a, E, F, G>(
    exp: &TypedExpr,
    transform_exp: F,
    transform_type: G,
) -> Result<TypedExpr, E>
where
    E: std::error::Error + From<&'a str>,
    F: Fn(&TypedExpr) -> Option<Result<TypedExpr, E>> + Copy,
    G: Fn(&Type) -> Option<Result<Type, E>> + Copy,
{
    // If the user's custom `transform_exp` function has a special way to
    // transform the provided node, then let's return that value.
    if let Some(transformed_exp) = transform_exp(&exp) {
        return transformed_exp;
    }
    // Otherwise, recurse normally according to the individual structures.
    match &*exp.kind {
        ExprKind::Num(x) => Ok(TypedExpr::new(Type::Int, ExprKind::Num(*x))),
        ExprKind::Bool(x) => Ok(TypedExpr::new(Type::Bool, ExprKind::Bool(*x))),
        ExprKind::Str(x) => Ok(TypedExpr::new(Type::Str, ExprKind::Str(x.clone()))),
        ExprKind::Id(x) => Ok(TypedExpr::new(
            transform_type_recursive(&exp.typ, transform_type)?,
            ExprKind::Id(x.clone()),
        )),
        ExprKind::Binop(op, arg1, arg2) => {
            let targ1 = transform_typed_exp_recursive(arg1, transform_exp, transform_type)?;
            let targ2 = transform_typed_exp_recursive(arg2, transform_exp, transform_type)?;
            Ok(TypedExpr::new(
                // I suppose we can recurse here (opposed to just cloning
                // exp.typ) in case there are binops in the future that
                // return non-primitive types.
                transform_type_recursive(&exp.typ, transform_type)?,
                ExprKind::Binop(*op, targ1, targ2),
            ))
        }
        ExprKind::If(pred, cons, alt) => {
            let tpred = transform_typed_exp_recursive(pred, transform_exp, transform_type)?;
            let tcons = transform_typed_exp_recursive(cons, transform_exp, transform_type)?;
            let talt = transform_typed_exp_recursive(alt, transform_exp, transform_type)?;
            Ok(TypedExpr::new(
                tcons.typ.clone(),
                ExprKind::If(tpred, tcons, talt),
            ))
        }
        ExprKind::Let(bindings, body) => {
            let tbindings = bindings
                .iter()
                .map(|(name, subexp)| {
                    let tsubexp =
                        transform_typed_exp_recursive(subexp, transform_exp, transform_type)?;
                    Ok((name.clone(), tsubexp))
                })
                .collect::<Result<Vector<(String, TypedExpr)>, E>>()?;
            let tbody = transform_typed_exp_recursive(body, transform_exp, transform_type)?;
            Ok(TypedExpr::new(
                tbody.typ.clone(),
                ExprKind::Let(tbindings, tbody),
            ))
        }
        ExprKind::Lambda(params, ret_type, body) => {
            let tparams = params
                .iter()
                .map(|(name, typ)| {
                    let ttype = transform_type_recursive(typ, transform_type)?;
                    Ok((name.clone(), ttype))
                })
                .collect::<Result<Vector<(String, Type)>, E>>()?;
            let tbody = transform_typed_exp_recursive(body, transform_exp, transform_type)?;
            let tret_type = transform_type_recursive(ret_type, transform_type)?;
            let param_types: Vector<Type> =
                tparams.iter().map(|(_name, typ)| typ.clone()).collect();
            let lambda_type = Type::Func(param_types, Box::new(tret_type.clone()));
            Ok(TypedExpr::new(
                lambda_type,
                ExprKind::Lambda(tparams, tret_type, tbody),
            ))
        }
        ExprKind::Begin(exps) => {
            let texps = exps
                .iter()
                .map(|subexp| transform_typed_exp_recursive(subexp, transform_exp, transform_type))
                .collect::<Result<Vector<TypedExpr>, E>>()?;
            let mut inner_types = texps
                .iter()
                .map(|rexp| rexp.typ.clone())
                .collect::<Vector<Type>>();
            Ok(TypedExpr::new(
                inner_types.remove(inner_types.len() - 1),
                ExprKind::Begin(texps),
            ))
        }
        ExprKind::Set(id, val) => {
            let tval = transform_typed_exp_recursive(val, transform_exp, transform_type)?;
            Ok(TypedExpr::new(
                tval.typ.clone(),
                ExprKind::Set(id.clone(), tval),
            ))
        }
        ExprKind::Cons(first, rest) => {
            let tfirst = transform_typed_exp_recursive(first, transform_exp, transform_type)?;
            let trest = transform_typed_exp_recursive(rest, transform_exp, transform_type)?;
            Ok(TypedExpr::new(
                trest.typ.clone(),
                ExprKind::Cons(tfirst, trest),
            ))
        }
        ExprKind::Car(val) => {
            let tval = transform_typed_exp_recursive(val, transform_exp, transform_type)?;
            match tval.typ.clone() {
                Type::List(boxed_type) => Ok(TypedExpr::new(*boxed_type, ExprKind::Car(tval))),
                _ => Err(E::from("Expression in car is not a list type.")),
            }
        }
        ExprKind::Cdr(val) => {
            let tval = transform_typed_exp_recursive(val, transform_exp, transform_type)?;
            Ok(TypedExpr::new(tval.typ.clone(), ExprKind::Cdr(tval)))
        }
        ExprKind::IsNull(val) => {
            let tval = transform_typed_exp_recursive(val, transform_exp, transform_type)?;
            Ok(TypedExpr::new(Type::Bool, ExprKind::IsNull(tval)))
        }
        ExprKind::Null(typ) => {
            let ttyp = transform_type_recursive(typ, transform_type)?;
            Ok(TypedExpr::new(
                Type::List(Box::new(typ.clone())),
                ExprKind::Null(ttyp),
            ))
        }
        ExprKind::Tuple(exps) => {
            let texps = exps
                .iter()
                .map(|subexp| transform_typed_exp_recursive(subexp, transform_exp, transform_type))
                .collect::<Result<Vector<TypedExpr>, E>>()?;
            let inner_types = texps
                .iter()
                .map(|typed_exp| typed_exp.typ.clone())
                .collect::<Vector<Type>>();
            Ok(TypedExpr::new(
                Type::Tuple(inner_types),
                ExprKind::Tuple(texps),
            ))
        }
        ExprKind::TupleGet(tuple, key) => {
            let ttuple = transform_typed_exp_recursive(tuple, transform_exp, transform_type)?;
            match ttuple.typ.clone() {
                Type::Tuple(vec) => {
                    if (*key as usize) < vec.len() {
                        let elem_type = vec[*key as usize].clone();
                        Ok(TypedExpr::new(elem_type, ExprKind::TupleGet(ttuple, *key)))
                    } else {
                        Err(E::from("Key in tuple-ref is too large for the tuple."))
                    }
                }
                _ => Err(E::from("First expression in tuple-ref is not a tuple.")),
            }
        }
        ExprKind::Record(bindings) => {
            let tbindings = bindings
                .iter()
                .map(|(name, subexp)| {
                    let tsubexp =
                        transform_typed_exp_recursive(subexp, transform_exp, transform_type)?;
                    Ok((name.clone(), tsubexp))
                })
                .collect::<Result<Vector<(String, TypedExpr)>, E>>()?;
            let types_vec: Vector<(String, Type)> = tbindings
                .iter()
                .map(|(field, exp)| (field.clone(), exp.typ.clone()))
                .collect();
            Ok(TypedExpr::new(
                Type::Record(types_vec),
                ExprKind::Record(tbindings),
            ))
        }
        ExprKind::RecordGet(record, key) => {
            let trecord = transform_typed_exp_recursive(record, transform_exp, transform_type)?;
            let tkey_type = match trecord.typ.clone() {
                Type::Record(fields) => {
                    let matches: Vector<(String, Type)> = fields
                        .iter()
                        .cloned()
                        .filter(|pair| pair.0 == *key)
                        .collect();
                    if matches.is_empty() {
                        return Err(E::from("Key in record-get not found in record."));
                    }
                    matches[0].1.clone()
                }
                _ => return Err(E::from("Non-record expression within record-get.")),
            };
            Ok(TypedExpr::new(
                tkey_type,
                ExprKind::RecordGet(trecord, key.clone()),
            ))
        }
        ExprKind::Pack(val, sub, exist) => {
            let tval = transform_typed_exp_recursive(val, transform_exp, transform_type)?;
            let tsub = transform_type_recursive(sub, transform_type)?;
            let texist = transform_type_recursive(exist, transform_type)?;
            Ok(TypedExpr::new(
                texist.clone(),
                ExprKind::Pack(tval, tsub, texist),
            ))
        }
        ExprKind::Unpack(var, package, type_sub, body) => {
            let tpackage = transform_typed_exp_recursive(package, transform_exp, transform_type)?;
            let tbody = transform_typed_exp_recursive(body, transform_exp, transform_type)?;
            Ok(TypedExpr::new(
                tbody.typ.clone(),
                ExprKind::Unpack(var.clone(), tpackage, *type_sub, tbody),
            ))
        }
        ExprKind::FnApp(func, args) => {
            let tfunc = transform_typed_exp_recursive(func, transform_exp, transform_type)?;
            let targs = args
                .iter()
                .map(|arg| transform_typed_exp_recursive(arg, transform_exp, transform_type))
                .collect::<Result<Vector<TypedExpr>, E>>()?;
            let targs_types = targs
                .iter()
                .map(|arg| arg.typ.clone())
                .collect::<Vector<Type>>();
            let apply_type = match validate_lambda_type(&tfunc.typ, &targs_types) {
                Ok(val) => val,
                Err(_e) => {
                    // let message = format!("Error in validate_lambda_type: {}", e);
                    return Err(E::from("Error in validate_lambda_type."));
                }
            };
            Ok(TypedExpr::new(apply_type, ExprKind::FnApp(tfunc, targs)))
        }
    }
}

/// Converts a program into one without record or record-ref expressions.
///
/// See `record_elim_exp` for more specific details.
pub fn transform_typed_prog_recursive<'a, E, F, G>(
    prog: &Prog<TypedExpr>,
    transform_exp: F,
    transform_type: G,
) -> Result<Prog<TypedExpr>, E>
where
    E: std::error::Error + From<&'a str>,
    F: Fn(&TypedExpr) -> Option<Result<TypedExpr, E>> + Copy,
    G: Fn(&Type) -> Option<Result<Type, E>> + Copy,
{
    let texp = transform_typed_exp_recursive(&prog.exp, transform_exp, transform_type)?;
    let tfns = prog
        .fns
        .iter()
        .map(|(name, func)| {
            Ok((
                name.clone(),
                transform_typed_exp_recursive(&func, transform_exp, transform_type)?,
            ))
        })
        .collect::<Result<Vector<(String, TypedExpr)>, E>>()?;
    Ok(Prog {
        exp: texp,
        fns: tfns,
    })
}
