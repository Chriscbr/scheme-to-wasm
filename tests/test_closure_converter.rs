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
            r#"(make-tuple ((lambda ((env0 : unknown) (x : int)) : int
                            (+ x 3))
                           (make-env))
                           :
                           ((-> unknown int int) unknown))"#,
        )
        .unwrap(),
    )
    .unwrap();
    assert_eq!(closure_convert(&exp).unwrap(), expected_exp);
}

// #[test]
// fn test_closure_convert_apply_lambda_no_free_vars() {
//     let _shared = THE_RESOURCE.lock().unwrap();
//     dangerously_reset_gensym_count();
//     let exp = parse(&lexpr::from_str("((lambda ((x : int)) : int (+ x 3)) 5)").unwrap()).unwrap();
//     let expected_exp = parse(
//         &lexpr::from_str(
//             r#"(let ((temp0
//        (make-tuple
//         ((lambda ((env0 : unknown) (x : int)) : int
//            (+ x 3))
//          (make-env))
//         :
//         ((-> unknown int int)
//          unknown))))
//   ((get-nth temp0 0) (get-nth temp0 1) 5))"#,
//         )
//         .unwrap(),
//     )
//     .unwrap();
//     assert_eq!(closure_convert(&exp).unwrap(), expected_exp);
// }

// #[test]
// fn test_closure_convert_lambda_yes_free_vars() {
//     let _shared = THE_RESOURCE.lock().unwrap();
//     dangerously_reset_gensym_count();
//     let exp = parse(&lexpr::from_str("(let ((y 3)) (lambda ((x : int)) : int (+ x y)))").unwrap())
//         .unwrap();
//     let expected_exp = CExpr::Let(
//         vector![(String::from("y"), CExpr::Num(3))],
//         Box::from(CExpr::Closure(
//             Box::from(CExpr::Lambda(
//                 vector![
//                     (String::from("env0"), Type::Unknown),
//                     (String::from("x"), Type::Int)
//                 ],
//                 Type::Int,
//                 Box::from(CExpr::Binop(
//                     BinOp::Add,
//                     Box::from(CExpr::Id(String::from("x"))),
//                     Box::from(CExpr::EnvGet(String::from("env0"), String::from("y"))),
//                 )),
//             )),
//             Box::from(CExpr::Env(vector![(
//                 String::from("y"),
//                 CExpr::Id(String::from("y"))
//             )])),
//         )),
//     );
//     assert_eq!(closure_convert(&exp).unwrap(), expected_exp);
// }
