#[macro_use]
extern crate lazy_static;

use im_rc::vector;
use scheme_to_rust::common::{dangerously_reset_gensym_count, Prog};
use scheme_to_rust::lambda_lift::lambda_lift;
use scheme_to_rust::parser::parse;
use std::sync::Mutex;

lazy_static! {
    static ref THE_RESOURCE: Mutex<()> = Mutex::new(());
}

#[test]
fn test_lambda_lift_happy() {
    let _shared = THE_RESOURCE.lock().unwrap();
    dangerously_reset_gensym_count();

    let exp = parse(
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
    let expected_fn = parse(
        &lexpr::from_str(
            r#"(lambda ((env1 : (record))
                         (x : int)) : int
                  (+ x 3))"#,
        )
        .unwrap(),
    )
    .unwrap();
    let expected_exp = parse(
        &lexpr::from_str(
            r#"(unpack (temp0
         (pack (make-tuple
                func0
                (make-record))
               (record)
               (exists T2 (tuple (-> T2 int int) T2))) T3)
        ((tuple-ref temp0 0) (tuple-ref temp0 1) 5))"#,
        )
        .unwrap(),
    )
    .unwrap();
    let expected_prog = Prog {
        fns: vector![(String::from("func0"), expected_fn)],
        exp: expected_exp,
    };
    let prog = lambda_lift(&exp).unwrap();
    assert_eq!(prog.fns, expected_prog.fns);
    assert_eq!(prog.exp, expected_prog.exp);
}
