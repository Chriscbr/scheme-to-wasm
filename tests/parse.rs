use im_rc::vector;
use scheme_to_wasm::parse::parse;
use scheme_to_wasm::parse::parse_type;
use scheme_to_wasm::types::Type;

#[test]
fn test_parse_int_bounds() {
    let exp = lexpr::from_str("2147483648").unwrap();
    assert_eq!(parse(&exp).is_err(), true);
}

#[test]
fn test_parse_type_primitives() {
    let exp = lexpr::from_str("int").unwrap();
    assert_eq!(parse_type(&exp).unwrap(), Type::Int);

    let exp = lexpr::from_str("bool").unwrap();
    assert_eq!(parse_type(&exp).unwrap(), Type::Bool);

    let exp = lexpr::from_str("string").unwrap();
    assert_eq!(parse_type(&exp).unwrap(), Type::Str);
}

#[test]
fn test_parse_type_type_vars() {
    let exp = lexpr::from_str("T0").unwrap();
    assert_eq!(parse_type(&exp).unwrap(), Type::TypeVar(0));

    let exp = lexpr::from_str("T42").unwrap();
    assert_eq!(parse_type(&exp).unwrap(), Type::TypeVar(42));
}

#[test]
fn test_parse_type_existentials() {
    let exp = lexpr::from_str("(exists T0 (-> T0 bool))").unwrap();
    assert_eq!(
        parse_type(&exp).unwrap(),
        Type::Exists(
            0,
            Box::new(Type::Func(vector![Type::TypeVar(0)], Box::new(Type::Bool)))
        )
    );
}

#[test]
fn test_parse_type_lists() {
    let exp = lexpr::from_str("(list int)").unwrap();
    assert_eq!(parse_type(&exp).unwrap(), Type::List(Box::new(Type::Int)));

    let exp = lexpr::from_str("(list (list int))").unwrap();
    assert_eq!(
        parse_type(&exp).unwrap(),
        Type::List(Box::new(Type::List(Box::new(Type::Int))))
    );
}

#[test]
fn test_parse_type_tuples() {
    let exp = lexpr::from_str("(tuple)").unwrap();
    assert_eq!(parse_type(&exp).unwrap(), Type::Tuple(vector![]));

    let exp = lexpr::from_str("(tuple int)").unwrap();
    assert_eq!(parse_type(&exp).unwrap(), Type::Tuple(vector![Type::Int]));

    let exp = lexpr::from_str("(tuple int string)").unwrap();
    assert_eq!(
        parse_type(&exp).unwrap(),
        Type::Tuple(vector![Type::Int, Type::Str])
    );
}

#[test]
fn test_parse_type_records() {
    let exp = lexpr::from_str("(record)").unwrap();
    assert_eq!(parse_type(&exp).unwrap(), Type::Record(vector![]));

    let exp = lexpr::from_str("(record (x : int))").unwrap();
    assert_eq!(
        parse_type(&exp).unwrap(),
        Type::Record(vector![(String::from("x"), Type::Int)])
    );

    let exp = lexpr::from_str("(record (init : string) (update : (-> string string)))").unwrap();
    assert_eq!(
        parse_type(&exp).unwrap(),
        Type::Record(vector![
            (String::from("init"), Type::Str),
            (
                String::from("update"),
                Type::Func(vector![Type::Str], Box::new(Type::Str))
            )
        ])
    );
}

#[test]
fn test_parse_type_functions() {
    let exp = lexpr::from_str("(-> int)").unwrap();
    assert_eq!(
        parse_type(&exp).unwrap(),
        Type::Func(vector![], Box::new(Type::Int))
    );

    let exp = lexpr::from_str("(-> int int)").unwrap();
    assert_eq!(
        parse_type(&exp).unwrap(),
        Type::Func(vector![Type::Int], Box::new(Type::Int))
    );

    let exp = lexpr::from_str("(-> string int bool)").unwrap();
    assert_eq!(
        parse_type(&exp).unwrap(),
        Type::Func(vector![Type::Str, Type::Int], Box::new(Type::Bool))
    );

    let exp = lexpr::from_str("(-> (-> int int bool) int int bool)").unwrap();
    assert_eq!(
        parse_type(&exp).unwrap(),
        Type::Func(
            vector![
                Type::Func(vector![Type::Int, Type::Int], Box::new(Type::Bool)),
                Type::Int,
                Type::Int
            ],
            Box::new(Type::Bool)
        )
    );
}
