#[macro_use]
extern crate lazy_static;

use scheme_to_rust::closure_convert::{closure_convert, dangerously_reset_gensym_count};
use scheme_to_rust::parser::parse;
use scheme_to_rust::type_checker::type_check;
use std::sync::Mutex;

lazy_static! {
    static ref THE_RESOURCE: Mutex<()> = Mutex::new(());
}

#[test]
fn test_closure_convert_lambda_no_free_vars() {
    let _shared = THE_RESOURCE.lock().unwrap();
    dangerously_reset_gensym_count();

    let exp = parse(&lexpr::from_str("(lambda ((x : int)) : int (+ x 3))").unwrap()).unwrap();
    let exp_typ = type_check(&exp);
    assert_eq!(exp_typ.is_err(), false);

    let expected_exp = parse(
        &lexpr::from_str(
            r#"(pack (make-tuple
       (lambda ((env0 : (record))
                (x : int)) : int
         (+ x 3))
       (make-record))
      (record)
      (exists T1 (tuple (-> T1 int int) T1)))"#,
        )
        .unwrap(),
    )
    .unwrap();
    let cc_exp = closure_convert(&exp).unwrap();
    type_check(&cc_exp).unwrap();

    println!("Source: {}", exp);
    println!("Closure converted: {}", cc_exp);
    assert_eq!(cc_exp, expected_exp);
}

#[test]
fn test_closure_convert_apply_lambda_no_free_vars() {
    let _shared = THE_RESOURCE.lock().unwrap();
    dangerously_reset_gensym_count();

    let exp = parse(&lexpr::from_str("((lambda ((x : int)) : int (+ x 3)) 5)").unwrap()).unwrap();
    let exp_typ = type_check(&exp);
    assert_eq!(exp_typ.is_err(), false);

    let expected_exp = parse(
        &lexpr::from_str(
            r#"(unpack (temp0
         (pack (make-tuple
                (lambda ((env1 : (record))
                         (x : int)) : int
                  (+ x 3))
                (make-record))
               (record)
               (exists T2 (tuple (-> T2 int int) T2))) T3)
        ((tuple-ref temp0 0) (tuple-ref temp0 1) 5))"#,
        )
        .unwrap(),
    )
    .unwrap();
    let cc_exp = closure_convert(&exp).unwrap();
    type_check(&cc_exp).unwrap();

    println!("Source: {}", exp);
    println!("Closure converted: {}", cc_exp);
    assert_eq!(cc_exp, expected_exp);
}

#[test]
fn test_closure_convert_lambda_yes_free_vars() {
    let _shared = THE_RESOURCE.lock().unwrap();
    dangerously_reset_gensym_count();

    let exp = parse(&lexpr::from_str("(let ((y 3)) (lambda ((x : int)) : int (+ x y)))").unwrap())
        .unwrap();
    let exp_typ = type_check(&exp);
    assert_eq!(exp_typ.is_err(), false);

    let expected_exp = parse(
        &lexpr::from_str(
            r#"(let ((y 3))
  (pack (make-tuple
         (lambda ((env0 : (record (y : int)))
                  (x : int)) : int
           (+ x (record-ref env0 y)))
         (make-record (y y)))
        (record (y : int))
        (exists T1 (tuple (-> T1 int int) T1))))"#,
        )
        .unwrap(),
    )
    .unwrap();
    let cc_exp = closure_convert(&exp).unwrap();
    type_check(&cc_exp).unwrap();

    println!("Source: {}", exp);
    println!("Closure converted: {}", cc_exp);
    assert_eq!(cc_exp, expected_exp);
}

#[test]
fn test_closure_convert_apply_lambda_yes_free_vars() {
    let _shared = THE_RESOURCE.lock().unwrap();
    dangerously_reset_gensym_count();

    let exp =
        parse(&lexpr::from_str("(let ((y 4)) ((lambda ((x : int)) : int (+ x y)) 3))").unwrap())
            .unwrap();
    let exp_typ = type_check(&exp);
    assert_eq!(exp_typ.is_err(), false);

    let expected_exp = parse(
        &lexpr::from_str(
            r#"(let ((y 4))
  (unpack
   (temp0
    (pack
     (make-tuple
      (lambda ((env1 : (record (y : int)))
               (x : int)) : int
        (+ x (record-ref env1 y)))
      (make-record (y y)))
     (record (y : int))
     (exists T2 (tuple (-> T2 int int) T2)))
    T3)
   ((tuple-ref temp0 0) (tuple-ref temp0 1) 3)))"#,
        )
        .unwrap(),
    )
    .unwrap();
    let cc_exp = closure_convert(&exp).unwrap();
    type_check(&cc_exp).unwrap();

    println!("Source: {}", exp);
    println!("Closure converted: {}", cc_exp);
    assert_eq!(cc_exp, expected_exp);
}

#[test]
fn test_closure_convert_lambda_by_name() {
    let _shared = THE_RESOURCE.lock().unwrap();
    dangerously_reset_gensym_count();

    let exp =
        parse(&lexpr::from_str("(let ((f (lambda ((x : int)) : int (+ x 3)))) (f 4))").unwrap())
            .unwrap();
    let exp_typ = type_check(&exp);
    assert_eq!(exp_typ.is_err(), false);

    let expected_exp = parse(
        &lexpr::from_str(
            r#"(let ((f (pack (make-tuple
                (lambda ((env0 : (record))
                         (x : int)) : int
                  (+ x 3))
                (make-record))
               (record)
               (exists T1 (tuple (-> T1 int int) T1)))))
  (unpack (temp2 f T3) ((tuple-ref temp2 0) (tuple-ref temp2 1) 4)))"#,
        )
        .unwrap(),
    )
    .unwrap();
    let cc_exp = closure_convert(&exp).unwrap();
    type_check(&cc_exp).unwrap();

    println!("Source: {}", exp);
    println!("Closure converted: {}", cc_exp);
    assert_eq!(cc_exp, expected_exp);
}

#[test]
fn test_closure_convert_lambda_with_func_param() {
    let _shared = THE_RESOURCE.lock().unwrap();
    dangerously_reset_gensym_count();

    let exp = parse(
        &lexpr::from_str(
            r#"(let ((a 3))
  (lambda ((f : (-> int int))) : (-> int)
    (lambda () : int (f a))))"#,
        )
        .unwrap(),
    )
    .unwrap();
    let exp_typ = type_check(&exp);
    assert_eq!(exp_typ.is_err(), false);

    let expected_exp = parse(
        &lexpr::from_str(
            r#"(let ((a 3))
  (pack
   (make-tuple
    (lambda ((env5 : (record (a : int)))
             (f : (exists T6 (tuple (-> T6 int int) T6))))
      : (exists T7 (tuple (-> T7 int) T7))
      (pack
       (make-tuple
        (lambda ((env2 : (record (f : (exists T3 (tuple (-> T3 int int) T3)))
                                 (a : int))))
          : int
          (unpack (temp0 (record-ref env2 f) T1)
                  ((tuple-ref temp0 0) (tuple-ref temp0 1) (record-ref env2 a))))
        (make-record (f f) (a (record-ref env5 a))))
       (record (f : (exists T3 (tuple (-> T3 int int) T3))) (a : int))
       (exists T4 (tuple (-> T4 int) T4))))
    (make-record (a a)))
   (record (a : int))
   (exists T10 (tuple (-> T10 (exists T8 (tuple (-> T8 int int) T8)) (exists T9 (tuple (-> T9 int) T9))) T10))))"#,
        )
        .unwrap(),
    )
    .unwrap();
    let cc_exp = closure_convert(&exp).unwrap();
    // type_check(&cc_exp).unwrap();

    println!("Source: {}", exp);
    println!("Closure converted: {}", cc_exp);
    assert_eq!(cc_exp, expected_exp);
}
