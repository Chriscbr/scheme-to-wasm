use crate::util::format_vector;
use im_rc::Vector;

#[derive(Clone, Debug)]
pub enum Type {
    Int,
    Bool,
    Str,
    List(Box<Type>),                // homogenous list
    Func(Vector<Type>, Box<Type>),  // array of input types, and a return type
    Tuple(Vector<Type>),            // array of types
    Record(Vector<(String, Type)>), // array of bindings
    Exists(u64, Box<Type>),         // abstract type T, and base type in terms of T
    TypeVar(u64),                   // abstract type T
    Unknown,                        // placeholder
}

// PartialEq is implemented manually to handle the specific case where two
// types are both existential types, and they should be equal with respect to
// substitution of one type variable for the other
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::List(base_a), Type::List(base_b)) => base_a == base_b,
            (Type::Func(in_a, ret_a), Type::Func(in_b, ret_b)) => in_a == in_b && ret_a == ret_b,
            (Type::Tuple(vec_a), Type::Tuple(vec_b)) => vec_a == vec_b,
            (Type::Record(vec_a), Type::Record(vec_b)) => vec_a == vec_b,
            (Type::Exists(typ_var_a, base_typ_a), Type::Exists(typ_var_b, base_typ_b)) => {
                let other_sub =
                    type_var_substitute(base_typ_b, *typ_var_b, &Type::TypeVar(*typ_var_a));
                **base_typ_a == other_sub
            }
            (Type::TypeVar(a), Type::TypeVar(b)) => a == b,
            (Type::Int, Type::Int) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Str, Type::Str) => true,
            (Type::Unknown, Type::Unknown) => true,
            (_, _) => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypeSubstituteError(String);

// Allows other errors to wrap this one
impl std::error::Error for TypeSubstituteError {}

impl From<&str> for TypeSubstituteError {
    fn from(message: &str) -> Self {
        TypeSubstituteError(String::from(message))
    }
}

impl std::fmt::Display for TypeSubstituteError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "TypeSubstituteError: {}", self.0)
    }
}

pub fn type_var_substitute(typ: &Type, type_var: u64, replace_with: &Type) -> Type {
    match typ {
        Type::Int => Type::Int,
        Type::Bool => Type::Bool,
        Type::Str => Type::Str,
        Type::List(base_typ) => {
            let sbase_typ = type_var_substitute(base_typ, type_var, replace_with);
            Type::List(Box::new(sbase_typ))
        }
        Type::Func(in_typs, ret_typ) => {
            let sin_typs: Vector<Type> = in_typs
                .iter()
                .map(|inner_typ| type_var_substitute(inner_typ, type_var, replace_with))
                .collect();
            let sret_typ = type_var_substitute(ret_typ, type_var, replace_with);
            Type::Func(sin_typs, Box::new(sret_typ))
        }
        Type::Tuple(typs) => {
            let styps: Vector<Type> = typs
                .iter()
                .map(|inner_typ| type_var_substitute(inner_typ, type_var, replace_with))
                .collect();
            Type::Tuple(styps)
        }
        Type::Record(bindings) => {
            let sbindings: Vector<(String, Type)> = bindings
                .iter()
                .map(|pair| {
                    let styp = type_var_substitute(&pair.1, type_var, replace_with);
                    (pair.0.clone(), styp)
                })
                .collect();
            Type::Record(sbindings)
        }
        Type::Exists(base_typ_var, base_typ) => {
            if *base_typ_var == type_var {
                let new_base_typ_var = type_var + 1;
                let base_typ_clean =
                    type_var_substitute(base_typ, *base_typ_var, &Type::TypeVar(new_base_typ_var));
                let sbase_typ = type_var_substitute(&base_typ_clean, type_var, replace_with);
                Type::Exists(new_base_typ_var, Box::new(sbase_typ))
            } else {
                let sbase_typ = type_var_substitute(base_typ, type_var, replace_with);
                Type::Exists(*base_typ_var, Box::new(sbase_typ))
            }
        }
        Type::TypeVar(x) => {
            if *x == type_var {
                replace_with.clone()
            } else {
                Type::TypeVar(*x)
            }
        }
        Type::Unknown => Type::Unknown,
    }
}

pub fn type_contains_var(typ: &Type, var: u64) -> bool {
    match typ {
        Type::Int => false,
        Type::Bool => false,
        Type::Str => false,
        Type::List(x) => type_contains_var(x, var),
        Type::Func(typs, ret_typ) => {
            typs.iter().any(|typ| type_contains_var(typ, var)) || type_contains_var(ret_typ, var)
        }
        Type::Tuple(typs) => typs.iter().any(|typ| type_contains_var(typ, var)),
        Type::Record(fields) => fields.iter().any(|field| type_contains_var(&field.1, var)),
        Type::Exists(bound_var, inner_typ) => {
            *bound_var != var && type_contains_var(inner_typ, var)
        }
        Type::TypeVar(x) => *x == var,
        Type::Unknown => false,
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Str => write!(f, "string"),
            Type::List(typ) => write!(f, "(list {})", typ),
            Type::Func(in_typs, ret_typ) => {
                if in_typs.is_empty() {
                    write!(f, "(-> {})", ret_typ)
                } else {
                    write!(f, "(-> {} {})", format_vector(in_typs.clone()), ret_typ)
                }
            }
            Type::Tuple(typs) => write!(f, "(tuple {})", format_vector(typs.clone())),
            Type::Record(bindings) => {
                if bindings.is_empty() {
                    write!(f, "(record)")
                } else {
                    let bindings_str_vec = bindings
                        .iter()
                        .map(|pair| format!("({} : {})", pair.0, pair.1))
                        .collect();
                    write!(f, "(record {})", format_vector(bindings_str_vec))
                }
            }
            Type::Exists(typ_var, base) => write!(f, "(exists T{} {})", typ_var, base),
            Type::TypeVar(id) => write!(f, "T{}", id),
            Type::Unknown => write!(f, "unknown"),
        }
    }
}
