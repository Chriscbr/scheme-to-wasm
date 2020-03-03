use crate::common::{Expr, ExprKind};
use crate::type_check::type_check;
use crate::types::Type;
use im_rc::Vector;

#[derive(Clone, Debug)]
pub struct RecordConvertError(String);

// Allows other errors to wrap this one
impl std::error::Error for RecordConvertError {}

impl From<&str> for RecordConvertError {
    fn from(message: &str) -> Self {
        RecordConvertError(String::from(message))
    }
}

impl std::fmt::Display for RecordConvertError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "RecordConvertError: {}", self.0)
    }
}

fn rc_type(typ: &Type) -> Result<Type, RecordConvertError> {
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
                .collect::<Result<Vec<(String, Type)>, RecordConvertError>>()?;
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

fn rc_type_array(types: &Vector<Type>) -> Result<Vector<Type>, RecordConvertError> {
    types.iter().map(|typ| Ok(rc_type(typ)?)).collect()
}

pub fn record_convert(exp: &Expr) -> Result<Expr, RecordConvertError> {
    rc(exp)
}

fn rc_bindings(
    bindings: &Vector<(String, Expr)>,
) -> Result<Vector<(String, Expr)>, RecordConvertError> {
    bindings
        .iter()
        .map(|pair| rc(&pair.1).and_then(|cexp| Ok((pair.0.clone(), cexp))))
        .collect()
}

fn rc(exp: &Expr) -> Result<Expr, RecordConvertError> {
    match &*exp.kind {
        ExprKind::Num(x) => Ok(Expr::new(None, ExprKind::Num(*x))),
        ExprKind::Bool(x) => Ok(Expr::new(None, ExprKind::Bool(*x))),
        ExprKind::Str(x) => Ok(Expr::new(None, ExprKind::Str(x.clone()))),
        ExprKind::Id(x) => Ok(Expr::new(None, ExprKind::Id(x.clone()))),
        ExprKind::Binop(op, arg1, arg2) => {
            let rarg1 = rc(&arg1)?;
            let rarg2 = rc(&arg2)?;
            Ok(Expr::new(None, ExprKind::Binop(*op, rarg1, rarg2)))
        }
        ExprKind::If(pred, cons, alt) => {
            let rpred = rc(&pred)?;
            let rcons = rc(&cons)?;
            let ralt = rc(&alt)?;
            Ok(Expr::new(None, ExprKind::If(rpred, rcons, ralt)))
        }
        ExprKind::Let(bindings, body) => {
            let rbindings = rc_bindings(&bindings)?;
            let rbody = rc(&body)?;
            Ok(Expr::new(None, ExprKind::Let(rbindings, rbody)))
        }
        ExprKind::Lambda(params, ret_type, body) => {
            let rbody = rc(&body)?;
            Ok(Expr::new(
                None,
                ExprKind::Lambda(params.clone(), ret_type.clone(), rbody),
            ))
        }
        ExprKind::Begin(exps) => {
            let rexps = exps
                .iter()
                .map(|subexp| rc(&subexp))
                .collect::<Result<Vector<Expr>, RecordConvertError>>()?;
            Ok(Expr::new(None, ExprKind::Begin(rexps)))
        }
        ExprKind::Set(id, val) => {
            let rval = rc(&val)?;
            Ok(Expr::new(None, ExprKind::Set(id.clone(), rval)))
        }
        ExprKind::Cons(first, rest) => {
            let rfirst = rc(&first)?;
            let rrest = rc(&rest)?;
            Ok(Expr::new(None, ExprKind::Cons(rfirst, rrest)))
        }
        ExprKind::Car(val) => {
            let rval = rc(&val)?;
            Ok(Expr::new(None, ExprKind::Car(rval)))
        }
        ExprKind::Cdr(val) => {
            let rval = rc(&val)?;
            Ok(Expr::new(None, ExprKind::Cdr(rval)))
        }
        ExprKind::IsNull(val) => {
            let rval = rc(&val)?;
            Ok(Expr::new(None, ExprKind::IsNull(rval)))
        }
        ExprKind::Null(typ) => Ok(Expr::new(None, ExprKind::Null(rc_type(&typ)?))),
        ExprKind::Tuple(exps) => {
            let rexps = exps
                .iter()
                .map(|subexp| rc(&subexp))
                .collect::<Result<Vector<Expr>, RecordConvertError>>()?;
            Ok(Expr::new(None, ExprKind::Tuple(rexps)))
        }
        ExprKind::TupleGet(tuple, key) => {
            let rtuple = rc(&tuple)?;
            Ok(Expr::new(None, ExprKind::TupleGet(rtuple, *key)))
        }
        ExprKind::Record(bindings) => {
            let mut bindings_vec: Vec<(String, Expr)> = bindings.iter().cloned().collect();
            bindings_vec.sort_unstable_by(|a, b| a.0.cmp(&b.0));
            let exp_vec: Vector<Expr> = bindings_vec
                .iter()
                .map(|(_field, exp)| exp.clone())
                .collect();
            Ok(Expr::new(None, ExprKind::Tuple(exp_vec)))
        }
        ExprKind::RecordGet(record, key) => {
            let tuple = rc(&record)?;
            let tuple_index = get_field_index(record, key)?;
            Ok(Expr::new(None, ExprKind::TupleGet(tuple, tuple_index)))
        }
        ExprKind::Pack(val, sub, exist) => {
            let rval = rc(&val)?;
            let rsub = rc_type(&sub)?;
            let rexist = rc_type(&exist)?;
            Ok(Expr::new(None, ExprKind::Pack(rval, rsub, rexist)))
        }
        ExprKind::Unpack(var, package, typ_sub, body) => {
            let rpackage = rc(&package)?;
            let rbody = rc(&body)?;
            Ok(Expr::new(
                None,
                ExprKind::Unpack(var.clone(), rpackage, *typ_sub, rbody),
            ))
        }
        ExprKind::FnApp(func, args) => {
            let rfunc = rc(&func)?;
            let rargs = args
                .iter()
                .map(|arg| rc(&arg))
                .collect::<Result<Vector<Expr>, RecordConvertError>>()?;
            Ok(Expr::new(None, ExprKind::FnApp(rfunc, rargs)))
        }
    }
}

fn get_field_index(record: &Expr, field: &str) -> Result<u64, RecordConvertError> {
    let record_type: Type = match type_check(record) {
        Ok(record) => record.checked_type.ok_or_else(|| {
            RecordConvertError::from("Record expression does not have type annotation.")
        })?,
        Err(e) => {
            return Err(RecordConvertError(format!(
                "Type checking error during record conversion: {}",
                e
            )))
        }
    };
    let mut fields_vec: Vec<(String, Type)> = match record_type {
        Type::Record(fields) => Ok(fields.iter().cloned().collect()),
        _ => Err(RecordConvertError::from(
            "Type annotation for record has incorrect type.",
        )),
    }?;
    fields_vec.sort_unstable_by(|a, b| a.0.cmp(&b.0));
    fields_vec
        .iter()
        .position(|pair| pair.0 == field)
        .map(|val| val as u64)
        .ok_or_else(|| RecordConvertError::from("Field not found in record."))
}
