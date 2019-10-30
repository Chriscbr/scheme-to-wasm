use im_rc::vector;
use scheme_to_rust::parse::parse_type;
use scheme_to_rust::types::{type_var_substitute, Type};

#[test]
fn test_type_var_substitute_idempotent() {
    // no substitution (simple)
    let typ = Type::Int;
    let type_var = 0;
    let replace_with = Type::Bool;
    assert_eq!(
        type_var_substitute(&typ, type_var, &replace_with),
        Type::Int
    );

    // no substitution (list)
    let typ = Type::List(Box::new(Type::Int));
    let type_var = 0;
    let replace_with = Type::Bool;
    assert_eq!(
        type_var_substitute(&typ, type_var, &replace_with),
        Type::List(Box::new(Type::Int))
    );

    // no substitution (function)
    let typ = Type::Func(vector![Type::Str, Type::Bool], Box::new(Type::Str));
    let type_var = 0;
    let replace_with = Type::Bool;
    assert_eq!(
        type_var_substitute(&typ, type_var, &replace_with),
        Type::Func(vector![Type::Str, Type::Bool], Box::new(Type::Str))
    );

    // no substitution (tuple)
    let typ = Type::Tuple(vector![Type::Str, Type::Bool, Type::Int]);
    let type_var = 0;
    let replace_with = Type::Bool;
    assert_eq!(
        type_var_substitute(&typ, type_var, &replace_with),
        Type::Tuple(vector![Type::Str, Type::Bool, Type::Int])
    );

    // no substitution (existential, different bound type)
    let typ = Type::Exists(1, Box::new(Type::TypeVar(1)));
    let type_var = 0;
    let replace_with = Type::Bool;
    assert_eq!(
        type_var_substitute(&typ, type_var, &replace_with),
        Type::Exists(1, Box::new(Type::TypeVar(1))),
    );

    // no substitution (existential, same bound type - inner typevar should get renamed)
    let typ = Type::Exists(0, Box::new(Type::TypeVar(0)));
    let type_var = 0;
    let replace_with = Type::Bool;
    assert_eq!(
        type_var_substitute(&typ, type_var, &replace_with),
        Type::Exists(1, Box::new(Type::TypeVar(1))),
    );

    // no substitution (different type var)
    let typ = Type::TypeVar(1);
    let type_var = 0;
    let replace_with = Type::Bool;
    assert_eq!(
        type_var_substitute(&typ, type_var, &replace_with),
        Type::TypeVar(1),
    );
}

#[test]
fn test_type_var_substitute_happy() {
    // substitution (simple)
    let typ = Type::TypeVar(3);
    let type_var = 3;
    let replace_with = Type::Bool;
    assert_eq!(
        type_var_substitute(&typ, type_var, &replace_with),
        Type::Bool
    );

    // substitution (list)
    let typ = Type::List(Box::new(Type::TypeVar(3)));
    let type_var = 3;
    let replace_with = Type::Bool;
    assert_eq!(
        type_var_substitute(&typ, type_var, &replace_with),
        Type::List(Box::new(Type::Bool))
    );

    // substitution (function)
    let typ = Type::Func(
        vector![Type::TypeVar(3), Type::Bool],
        Box::new(Type::TypeVar(3)),
    );
    let type_var = 3;
    let replace_with = Type::Int;
    assert_eq!(
        type_var_substitute(&typ, type_var, &replace_with),
        Type::Func(vector![Type::Int, Type::Bool], Box::new(Type::Int))
    );

    // substitution (tuple)
    let typ = Type::Tuple(vector![Type::TypeVar(3), Type::Bool]);
    let type_var = 3;
    let replace_with = Type::Int;
    assert_eq!(
        type_var_substitute(&typ, type_var, &replace_with),
        Type::Tuple(vector![Type::Int, Type::Bool])
    );

    // substitution (existential)
    let typ = Type::Exists(
        1,
        Box::new(Type::Tuple(vector![Type::TypeVar(0), Type::TypeVar(1)])),
    );
    let type_var = 0;
    let replace_with = Type::Bool;
    assert_eq!(
        type_var_substitute(&typ, type_var, &replace_with),
        Type::Exists(
            1,
            Box::new(Type::Tuple(vector![Type::Bool, Type::TypeVar(1)]))
        ),
    );
}

#[test]
fn test_existential_type_equality() {
    let typ1 = parse_type(&lexpr::from_str("(exists T0 T0)").unwrap()).unwrap();
    let typ2 = parse_type(&lexpr::from_str("(exists T1 T1)").unwrap()).unwrap();
    assert_eq!(typ1, typ2);

    let typ1 = parse_type(&lexpr::from_str("(exists T0 T2)").unwrap()).unwrap();
    let typ2 = parse_type(&lexpr::from_str("(exists T1 T2)").unwrap()).unwrap();
    assert_eq!(typ1, typ2);

    let typ1 = parse_type(&lexpr::from_str("(exists T0 T0)").unwrap()).unwrap();
    let typ2 = parse_type(&lexpr::from_str("(exists T1 T2)").unwrap()).unwrap();
    assert_ne!(typ1, typ2);
}
