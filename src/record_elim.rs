// TODO: implement a more modular / functional way of handling compiler pass
// ExprKind case matching... should be able to define a function which just
// does transformations on the cases I am interested in, and does some form
// of default recursion in other cases

use crate::common::{
    transform_type_recursive, transform_typed_exp_recursive, transform_typed_prog_recursive,
    ExprKind, Prog, TypedExpr,
};
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

/// Convert an expression into one without record or record-ref expressions.
///
/// Conversion is performed by replacing records with corresponding tuples,
/// and replacing record-ref with tuple-ref expressions.
///
/// Expression must be type checked (annotated with types) before being passed
/// in. After conversion, the output expression of this function will have all
/// type annotations removed, so it should be re-type-checked.
pub fn record_elim_exp(exp: &TypedExpr) -> Result<TypedExpr, RecordElimError> {
    transform_typed_exp_recursive(exp, re_helper, re_type_helper)
}

/// Converts a program into one without record or record-ref expressions.
///
/// See `record_elim_exp` for more specific details.
pub fn record_elim_prog(prog: &Prog<TypedExpr>) -> Result<Prog<TypedExpr>, RecordElimError> {
    transform_typed_prog_recursive(prog, re_helper, re_type_helper)
}

fn re_type(typ: &Type) -> Result<Type, RecordElimError> {
    transform_type_recursive(typ, re_type_helper)
}

fn re_type_helper(typ: &Type) -> Option<Result<Type, RecordElimError>> {
    match typ {
        Type::Record(bindings) => {
            let re_bindings = bindings
                .iter()
                .map(|(name, inner_type)| Ok((name.clone(), re_type(inner_type)?)))
                .collect::<Result<Vec<(String, Type)>, RecordElimError>>();
            // We can't immediately unwrap the results in the line above since
            // this function returns an Option of result. We could resolve this
            // by using _another_ helper function that just returns a
            // Result<Type, ..> just for the convenience of using the ?
            // operator, but that seems unnecessary.
            let mut re_bindings = match re_bindings {
                Ok(vec) => vec,
                Err(e) => return Some(Err(e)),
            };
            re_bindings.sort_unstable_by(|a, b| a.0.cmp(&b.0));
            let type_vec: Vec<Type> = re_bindings
                .iter()
                .map(|(_field, typ)| typ.clone())
                .collect();
            Some(Ok(Type::Tuple(Vector::from(type_vec))))
        }
        _ => None,
    }
}

fn re_helper(exp: &TypedExpr) -> Option<Result<TypedExpr, RecordElimError>> {
    match &*exp.kind {
        ExprKind::Record(bindings) => {
            let rbindings = bindings
                .iter()
                .map(|(name, subexp)| {
                    let tsubexp = record_elim_exp(subexp)?;
                    Ok((name.clone(), tsubexp))
                })
                .collect::<Result<Vector<(String, TypedExpr)>, RecordElimError>>();
            let rbindings = match rbindings {
                Ok(vec) => vec,
                Err(e) => return Some(Err(e)),
            };
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
            Some(Ok(TypedExpr::new(
                Type::Tuple(inner_types),
                ExprKind::Tuple(exp_vec),
            )))
        }
        ExprKind::RecordGet(record, key) => {
            let tuple = match record_elim_exp(&record) {
                Ok(val) => val,
                Err(e) => return Some(Err(e)),
            };
            let tuple_index = match get_field_index(record, key) {
                Ok(val) => val,
                Err(e) => return Some(Err(e)),
            };
            match tuple.typ.clone() {
                Type::Tuple(vec) => {
                    if (tuple_index as usize) < vec.len() {
                        let elem_type = vec[tuple_index as usize].clone();
                        Some(Ok(TypedExpr::new(
                            elem_type,
                            ExprKind::TupleGet(tuple, tuple_index),
                        )))
                    } else {
                        Some(Err(RecordElimError::from(
                            "Key in tuple-ref is too large for the provided tuple.",
                        )))
                    }
                }
                _ => Some(Err(RecordElimError::from(
                    "First expression in record-ref (after conversion) is not a tuple.",
                ))),
            }
        }
        _ => None,
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
