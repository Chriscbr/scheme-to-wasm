#[macro_use]
extern crate lazy_static;

use scheme_to_rust::closure_convert::{closure_convert, dangerously_reset_gensym_count};
use scheme_to_rust::parser::parse;
use std::sync::Mutex;

lazy_static! {
    static ref THE_RESOURCE: Mutex<()> = Mutex::new(());
}

#[test]
fn test_closure_convert_lambda_no_free_vars() {
    let _shared = THE_RESOURCE.lock().unwrap();
    dangerously_reset_gensym_count();
    let exp = parse(&lexpr::from_str("(lambda ((x : int)) : int (+ x 3))").unwrap()).unwrap();
    let expected_exp = parse(
        &lexpr::from_str(
            r#"(make-tuple (lambda ((env0 : (record)) (x : int)) : int
                            (+ x 3))
                           (make-record))"#,
        )
        .unwrap(),
    )
    .unwrap();
    let cc_exp = closure_convert(&exp).unwrap();
    println!("Source: {}", exp);
    println!("Closure converted: {}", cc_exp);
    assert_eq!(cc_exp, expected_exp);
}

#[test]
fn test_closure_convert_apply_lambda_no_free_vars() {
    let _shared = THE_RESOURCE.lock().unwrap();
    dangerously_reset_gensym_count();
    let exp = parse(&lexpr::from_str("((lambda ((x : int)) : int (+ x 3)) 5)").unwrap()).unwrap();
    let expected_exp = parse(
        &lexpr::from_str(
            r#"(let ((temp1
       (make-tuple
        (lambda ((env0 : (record)) (x : int)) : int
           (+ x 3))
         (make-record))))
  ((tuple-ref temp1 0) (tuple-ref temp1 1) 5))"#,
        )
        .unwrap(),
    )
    .unwrap();
    let cc_exp = closure_convert(&exp).unwrap();
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
    let expected_exp = parse(
        &lexpr::from_str(
            r#"
        (let ((y 3))
            (make-tuple (lambda ((env0 : (record (y : int))) (x : int)) : int
                            (+ x (record-ref env0 y)))
                        (make-record (y y))))"#,
        )
        .unwrap(),
    )
    .unwrap();
    let cc_exp = closure_convert(&exp).unwrap();
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
    let expected_exp = parse(
        &lexpr::from_str(
            r#"(let ((y 4))
            (let ((temp1 (make-tuple
                    (lambda ((env0 : (record (y : int))) (x : int)) : int
                        (+ x (record-ref env0 y)))
                    (make-record (y y)))))
                ((tuple-ref temp1 0) (tuple-ref temp1 1) 3)))"#,
        )
        .unwrap(),
    )
    .unwrap();
    let cc_exp = closure_convert(&exp).unwrap();
    println!("Source: {}", exp);
    println!("Closure converted: {}", cc_exp);
    assert_eq!(cc_exp, expected_exp);
}
