use im_rc::vector;
use scheme_to_wasm::common::TypeEnv;
use scheme_to_wasm::parse::{parse, parse_type};
use scheme_to_wasm::type_check::{tc_with_env, type_check};
use scheme_to_wasm::types::Type;

#[test]
fn test_typecheck_prims() {
    let exp = lexpr::from_str("3").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);

    let exp = lexpr::from_str("-497").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);

    let exp = lexpr::from_str("#t").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Bool);

    let exp = lexpr::from_str("#f").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Bool);

    let exp = lexpr::from_str("true").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Bool);

    let exp = lexpr::from_str("false").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Bool);

    let exp = lexpr::from_str("\"true\"").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Str);

    let exp = lexpr::from_str("\"foo\"").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Str);

    let exp = lexpr::from_str("\"\"").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Str);
}

#[test]
fn test_typecheck_binops_happy() {
    let exp = lexpr::from_str("(+ 3 5)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);

    let exp = lexpr::from_str("(* 3 5)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);

    let exp = lexpr::from_str("(- 3 5)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);

    let exp = lexpr::from_str("(/ 3 5)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);

    let exp = lexpr::from_str("(+ (* 4 5) (- 5 2))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);

    let exp = lexpr::from_str(r#"(concat "hello " "world")"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Str);

    let exp = lexpr::from_str("(and true false)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Bool);

    let exp = lexpr::from_str("(or true false)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Bool);
}

#[test]
fn test_typecheck_binops_sad() {
    let exp = lexpr::from_str("(+ 3 true)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    let exp = lexpr::from_str("(* 3 true)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    let exp = lexpr::from_str(r#"(- false "hello")"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    let exp = lexpr::from_str(r#"(/ "foo" 3)"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    let exp = lexpr::from_str(r#"(concat 3 "world")"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    let exp = lexpr::from_str(r#"(and 3 6)"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    let exp = lexpr::from_str(r#"(or "hello" "world")"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);
}

#[test]
fn test_typecheck_lists_happy() {
    let exp = lexpr::from_str("(null int)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::List(Box::new(Type::Int)));

    let exp = lexpr::from_str("(null bool)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::List(Box::new(Type::Bool)));

    let exp = lexpr::from_str("(cons 3 (null int))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::List(Box::new(Type::Int)));

    let exp = lexpr::from_str("(cons 3 (cons 4 (null int)))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::List(Box::new(Type::Int)));

    let exp = lexpr::from_str(r#"(cons "foo" (cons "bar" (null string)))"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::List(Box::new(Type::Str)));

    // NOTE: these two cases probably looks weird, but it is the simplest solution
    // we can just assume car/cdr of an empty list is the type of the list's items
    // or the type of the list respectively (or that the program just panics?)
    let exp = lexpr::from_str("(car (null int))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);

    let exp = lexpr::from_str("(cdr (null int))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::List(Box::new(Type::Int)));

    let exp = lexpr::from_str("(car (cons 3 (null int)))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);

    let exp = lexpr::from_str("(cdr (cons 3 (null int)))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::List(Box::new(Type::Int)));

    let exp = lexpr::from_str("(null? (null int))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Bool);
}

#[test]
fn test_typecheck_lists_sad() {
    // type of car does not match type of cdr
    let exp = lexpr::from_str(r#"(cons "hey" (null int))"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    // invalid argument to car
    let exp = lexpr::from_str("(car 3)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    // invalid argument to cdr
    let exp = lexpr::from_str("(cdr 3)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);
}

#[test]
fn test_typecheck_tuples_happy() {
    let exp = lexpr::from_str(r#"(make-tuple)"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Tuple(vector![]));

    let exp = lexpr::from_str(r#"(make-tuple 3 "hello")"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(
        type_check(&exp).unwrap(),
        Type::Tuple(vector![Type::Int, Type::Str])
    );

    let exp = lexpr::from_str(r#"(tuple-ref (make-tuple 3 "hello") 0)"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);

    let exp = lexpr::from_str(r#"(tuple-ref (make-tuple 3 "hello") 1)"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Str);
}

#[test]
fn test_typecheck_tuples_sad() {
    // types do not match
    let exp = lexpr::from_str(r#"(make-tuple (3 "hello") : (bool string))"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    // too few types
    let exp = lexpr::from_str(r#"(make-tuple (3 "hello") : (int))"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    // too too many types
    let exp = lexpr::from_str(r#"(make-tuple (3 "hello") : (int string bool))"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    // key too large
    let exp = lexpr::from_str(r#"(get-nth (make-tuple (3 "hello") : (int string)) 2)"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    // key is not a number
    let exp = lexpr::from_str(r#"(get-nth (make-tuple (3 "hello") : (int string)) true)"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    // first expression is not a tuple
    let exp = lexpr::from_str(r#"(get-nth (cons 3 (null int)) 0)"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);
}

#[test]
fn test_typecheck_records_happy() {
    let exp = lexpr::from_str(r#"(make-record)"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Record(vector![]));

    let exp = lexpr::from_str(r#"(make-record (num 3) (name "hello"))"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(
        type_check(&exp).unwrap(),
        Type::Record(vector![
            (String::from("num"), Type::Int),
            (String::from("name"), Type::Str)
        ])
    );

    let exp = lexpr::from_str(r#"(let ((bar 3)) (make-record (foo bar)))"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(
        type_check(&exp).unwrap(),
        Type::Record(vector![(String::from("foo"), Type::Int)])
    );

    let exp = lexpr::from_str(r#"(record-ref (make-record (num 3) (name "hello")) num)"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);

    let exp = lexpr::from_str(r#"(record-ref (make-record (num 3) (name "hello")) name)"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Str);

    let exp = lexpr::from_str(r#"(let ((a (make-record (b 3)))) (record-ref a b))"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);
}

#[test]
fn test_typecheck_records_sad() {
    // we don't know what type bar is
    let exp = lexpr::from_str(r#"(make-record (foo bar))"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    // TODO: In practice we shouldn't allow multiple of the same field for a record,
    // but this isn't high priority language feature.

    // // record contains duplicate values
    // let exp = lexpr::from_str(r#"(make-record (num 3) (num 4))"#).unwrap();
    // let exp = parse(&exp).unwrap();
    // assert_eq!(type_check(&exp).is_err(), true);

    // // record contains duplicate values (of different types)
    // let exp = lexpr::from_str(r#"(make-record (num 3) (num "hello"))"#).unwrap();
    // let exp = parse(&exp).unwrap();
    // assert_eq!(type_check(&exp).is_err(), true);

    // invalid key on record-ref
    let exp = lexpr::from_str(r#"(record-ref (make-record (num 3) (name "hello")) foo)"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    // non-record expression passed to record-ref
    let exp = lexpr::from_str(r#"(record-ref "hello" foo)"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);
}

#[test]
fn test_typecheck_let_happy() {
    let exp = lexpr::from_str("(let ((x 23)) (+ x 24))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);

    let exp = lexpr::from_str("(let ((x 3) (y 5)) (+ x y))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);
}

#[test]
fn test_typecheck_let_sad() {
    // one variable missing
    let exp = lexpr::from_str("(let ((x 23)) (+ x y))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);
}

#[test]
fn test_typecheck_sideeffects_happy() {
    let exp = lexpr::from_str("(begin (+ 3 5))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);

    let exp = lexpr::from_str("(begin (+ 3 5) (- 4 1))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);

    let exp = lexpr::from_str("(let ((x 3)) (set! x 7))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);
}

#[test]
fn test_typecheck_sideeffects_sad() {
    // intermediary expression in begin is not valid
    let exp = lexpr::from_str(r#"(begin (+ 3 "hello") (- 4 1))"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    // last expression in begin is not valid
    let exp = lexpr::from_str(r#"(begin (+ 3 4) (- 4 "hello"))"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    // using set! before variable is defined
    let exp = lexpr::from_str("(set! x 7)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);
}

#[test]
fn test_typecheck_local_scoping() {
    // local variable overrides outer variable
    let exp = lexpr::from_str(
        r#"(let ((x "hello"))
                (let ((x 23))
                    (+ x 24)))"#,
    )
    .unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);

    // binding from let lasts past its scope
    let exp = lexpr::from_str("(+ (let ((x 5)) (+ x 3)) x)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    // binding from lambda lasts past its scope
    let exp = lexpr::from_str("(+ ((lambda ((x : int)) : int x) 3) x)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    // let bindings with same names of conflicting types
    let exp = lexpr::from_str("(and (let ((x 5)) (< x 3)) (let ((x false)) (or x true)))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Bool);

    // lambda bindings with same names of conflicting types
    let exp =
        lexpr::from_str("(and ((lambda ((x : int)) : bool (< x 3)) 5) ((lambda ((x : bool)) : bool (and x true)) false))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Bool);

    // using set! after variable goes out of scope
    let exp = lexpr::from_str("(begin (let ((x 3)) (+ x 5)) (set! x 7))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);
}

#[test]
fn test_test_typecheck_if_happy() {
    let exp = lexpr::from_str(r#"(if (< 3 4) 1 -1)"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);
}

#[test]
fn test_typecheck_if_sad() {
    // invalid predicate
    let exp = lexpr::from_str(r#"(if 3 4 5)"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    // consequent and alternate do not match
    let exp = lexpr::from_str(r#"(if (< 3 4) "hello" 5)"#).unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);
}

#[test]
fn test_typecheck_lambda_happy() {
    let exp = lexpr::from_str("(lambda () : int 3)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(
        type_check(&exp).unwrap(),
        Type::Func(vector![], Box::new(Type::Int))
    );

    let exp = lexpr::from_str("(lambda ((x : int)) : bool (< x 5))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(
        type_check(&exp).unwrap(),
        Type::Func(vector![Type::Int], Box::new(Type::Bool))
    );

    let exp = lexpr::from_str("(lambda ((x : int) (y : int)) : int (* x y))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(
        type_check(&exp).unwrap(),
        Type::Func(vector![Type::Int, Type::Int], Box::new(Type::Int))
    );

    let exp =
        lexpr::from_str("(lambda ((fn : (-> int int bool)) (x : int) (y : int)) : bool (fn x y))")
            .unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(
        type_check(&exp).unwrap(),
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

#[test]
fn test_typecheck_nested_lambdas() {
    let exp = parse(
        &lexpr::from_str(
            "(let ((a 3))
  (lambda ((f : (-> int int))) : (-> int)
    (lambda () : int (f a))))",
        )
        .unwrap(),
    )
    .unwrap();
    let expected = parse_type(&lexpr::from_str("(-> (-> int int) (-> int))").unwrap()).unwrap();
    assert_eq!(type_check(&exp).unwrap(), expected);

    let exp = parse(
        &lexpr::from_str(
            r#"(let ((a 3))
  (lambda ((f : (-> int int int int))) : (-> int (-> int int))
     (lambda ((z : int)) : (-> int int)
       (lambda ((x : int)) : int
         (f x z a)))))"#,
        )
        .unwrap(),
    )
    .unwrap();
    let expected =
        parse_type(&lexpr::from_str("(-> (-> int int int int) (-> int (-> int int)))").unwrap())
            .unwrap();
    assert_eq!(type_check(&exp).unwrap(), expected);

    let exp = parse(
        &lexpr::from_str(
            "(let ((f (lambda ((x : int)) : (-> int int)
           (lambda ((y : int)) : int (+ x y)))))
  ((f 4) 3))",
        )
        .unwrap(),
    )
    .unwrap();
    let expected = parse_type(&lexpr::from_str("int").unwrap()).unwrap();
    assert_eq!(type_check(&exp).unwrap(), expected);
}

#[test]
fn test_typecheck_lambdas_recursive_happy() {
    // Not a very clean way to implement recursive functions.
    // Currently creates a dummy function (so that the function will have a name)
    // and then sets the name to the actual function definition, with the
    // name available for use.
    // Consider adding a named-lambda or fixed point operator.
    let exp = lexpr::from_str(
        r#"(let ((foo (lambda ((x : int)) : int 0)))
    (set! foo (lambda ((x : int)) : int (if (< x 1) 0 (+ 1 (foo (- x 1)))))))"#,
    )
    .unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(
        type_check(&exp).unwrap(),
        Type::Func(vector![Type::Int], Box::new(Type::Int))
    );
}

#[test]
fn test_typecheck_lambda_sad() {
    // mismatched return type
    let exp = lexpr::from_str("(lambda () : bool 3)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    // input types do not work in body
    let exp = lexpr::from_str("(lambda ((x : bool) (y : bool)) : int (+ x y))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);
}

#[test]
fn test_typecheck_apply_happy() {
    let exp = lexpr::from_str("((lambda () : int 3))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);

    let exp = lexpr::from_str("((lambda ((x : int)) : bool (< x 5)) 3)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Bool);

    let exp = lexpr::from_str("((lambda ((x : int) (y : int)) : int (* x y)) 5 6)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);

    let exp = lexpr::from_str("((lambda ((x : int) (y : int)) : int (* x y)) 5 6)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Int);
}

#[test]
fn test_typecheck_apply_hof_happy() {
    // Note: this is basically (apply (lambda equivalent to <) 3 5)
    let exp = lexpr::from_str("((lambda ((fn : (-> int int bool)) (x : int) (y : int)) : bool (fn x y)) (lambda ((a : int) (b : int)) : bool (< a b)) 3 5)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).unwrap(), Type::Bool);

    // "map" is recursive, so without implementing type-checking for define,
    // (which would create the binding from "map" to its type signature)
    // we must add the name to the environment for the unit test
    let exp = lexpr::from_str(
        "(lambda ((f : (-> int int)) (lst : (list int))) : (list int)
            (if (null? lst)
                (null int)
                (cons (f (car lst)) (map f (cdr lst)))))",
    )
    .unwrap();
    let exp = parse(&exp).unwrap();
    let map_type = Type::Func(
        vector![
            Type::Func(vector![Type::Int], Box::new(Type::Int)),
            Type::List(Box::new(Type::Int)),
        ],
        Box::new(Type::List(Box::new(Type::Int))),
    ); // (-> (-> int int) (list int) (list int))
    let mut env = TypeEnv::new();
    env = env.add_binding((String::from("map"), map_type.clone()));
    assert_eq!(tc_with_env(&exp, &env).unwrap(), map_type);
}

#[test]
fn test_typecheck_apply_sad() {
    // missing parameters
    let exp = lexpr::from_str("((lambda ((x : int)) : bool (< x 5)))").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    // too many parameters
    let exp = lexpr::from_str("((lambda ((x : int)) : bool (< x 5)) 3 5)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    // arg type does not match param type
    let exp = lexpr::from_str("((lambda ((x : int)) : bool (< x 5)) true)").unwrap();
    let exp = parse(&exp).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);
}

#[test]
fn test_typecheck_pack_happy() {
    // show that multiple types can inhabit an existential type
    let exp = parse(&lexpr::from_str("(pack 3 int (exists T0 T0))").unwrap()).unwrap();
    let typ = parse_type(&lexpr::from_str("(exists T0 T0)").unwrap()).unwrap();
    assert_eq!(type_check(&exp).unwrap(), typ);

    let exp = parse(&lexpr::from_str("(pack true bool (exists T0 T0))").unwrap()).unwrap();
    let typ = parse_type(&lexpr::from_str("(exists T0 T0)").unwrap()).unwrap();
    assert_eq!(type_check(&exp).unwrap(), typ);

    // function
    let exp = parse(
        &lexpr::from_str("(pack (lambda ((x : int)) : int (+ x 1)) int (exists T0 (-> T0 T0)))")
            .unwrap(),
    )
    .unwrap();
    let typ = parse_type(&lexpr::from_str("(exists T0 (-> T0 T0))").unwrap()).unwrap();
    assert_eq!(type_check(&exp).unwrap(), typ);

    // slightly more specific type ascription
    let exp = parse(
        &lexpr::from_str("(pack (lambda ((x : int)) : int (+ x 1)) int (exists T0 (-> T0 int)))")
            .unwrap(),
    )
    .unwrap();
    let typ = parse_type(&lexpr::from_str("(exists T0 (-> T0 int))").unwrap()).unwrap();
    assert_eq!(type_check(&exp).unwrap(), typ);

    // packing a record expression
    let exp = parse(
        &lexpr::from_str(
            r#"(pack (make-record (a 0)
                            (f (lambda ((x : int)) : int (+ 1 x))))
               int
               (exists T0 (record (a : T0) (f : (-> T0 int)))))"#,
        )
        .unwrap(),
    )
    .unwrap();
    let typ =
        parse_type(&lexpr::from_str("(exists T0 (record (a : T0) (f : (-> T0 int))))").unwrap())
            .unwrap();
    assert_eq!(type_check(&exp).unwrap(), typ);
}

#[test]
fn test_typecheck_pack_sad() {
    let exp = parse(&lexpr::from_str("(pack true int (exists T0 T0))").unwrap()).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    let exp = parse(&lexpr::from_str("(pack 3 int (exists T0 bool))").unwrap()).unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    let exp = parse(
        &lexpr::from_str("(pack (lambda ((x : int)) : bool 3) int (exists T0 (-> T0 T0)))")
            .unwrap(),
    )
    .unwrap();
    assert_eq!(type_check(&exp).is_err(), true);
}

#[test]
fn test_typecheck_unpack_happy() {
    let exp = parse(
        &lexpr::from_str(
            r#"
            (let ((p (pack (make-record (a 0)
                                (f (lambda ((x : int)) : int
                                     (+ 1 x))))
                   int
                   (exists T0 (record (a : T0)
                                      (f : (-> T0 int)))))))
      (unpack (q p T0)
              ((record-ref q f) (record-ref q a))))
                    "#,
        )
        .unwrap(),
    )
    .unwrap();
    let typ = parse_type(&lexpr::from_str("int").unwrap()).unwrap();
    assert_eq!(type_check(&exp).unwrap(), typ);

    let exp = parse(
        &lexpr::from_str(
            r#"(let ((p (pack (make-record (a 0)
                            (f (lambda ((x : int)) : int
                                 (+ 1 x))))
               int
               (exists T0 (record (a : T0)
                                  (f : (-> T0 int)))))))
  (unpack (q p T2)
          ((lambda ((y : T2)) : int ((record-ref q f) y))
           (record-ref q a))))"#,
        )
        .unwrap(),
    )
    .unwrap();
    let typ = parse_type(&lexpr::from_str("int").unwrap()).unwrap();
    assert_eq!(type_check(&exp).unwrap(), typ);
}

#[test]
fn test_typecheck_unpack_sad() {
    // q.a is not guaranteed to be a number in the existential type,
    // so we cannot use it (and add to it) concretely as such
    let exp = parse(
        &lexpr::from_str(
            r#"
            (let ((p (pack (make-record (a 0)
                                (f (lambda ((x : int)) : int
                                     (+ 1 x))))
                   int
                   (exists T0 (record (a : T0)
                                      (f : (-> T0 int)))))))
      (unpack (q p T0)
              (+ (record-ref q a) 1)))
                    "#,
        )
        .unwrap(),
    )
    .unwrap();
    assert_eq!(type_check(&exp).is_err(), true);

    // the result type cannot have the type variable free
    let exp = parse(
        &lexpr::from_str(
            r#"
            (let ((p (pack (make-record (a 0)
                                (f (lambda ((x : int)) : int
                                     (+ 1 x))))
                   int
                   (exists T0 (record (a : T0)
                                      (f : (-> T0 int)))))))
      (unpack (q p T0)
              (record-ref q a)))
                    "#,
        )
        .unwrap(),
    )
    .unwrap();
    assert_eq!(type_check(&exp).is_err(), true);
}

#[test]
fn test_typecheck_adt() {
    let exp = parse(
        &lexpr::from_str(
            "(pack (make-record (new 1)
                   (get (lambda ((i : int)) : int i))
                   (inc (lambda ((i : int)) : int (+ i 1))))
      int
      (exists T0 (record (new : T0)
                         (get : (-> T0 int))
                         (inc : (-> T0 T0)))))",
        )
        .unwrap(),
    )
    .unwrap();
    let typ = parse_type(
        &lexpr::from_str(
            "(exists T0 (record (new : T0)
                         (get : (-> T0 int))
                         (inc : (-> T0 T0))))",
        )
        .unwrap(),
    )
    .unwrap();
    assert_eq!(type_check(&exp).unwrap(), typ);
}
