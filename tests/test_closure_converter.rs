#[macro_use]
extern crate lazy_static; // 1.0.2

use im_rc::vector;
use scheme_to_rust::closure_convert::{
    closure_convert, dangerously_reset_gensym_count, CExpr, CType,
};
use scheme_to_rust::common::BinOp;
use scheme_to_rust::parser::parse;
use std::sync::Mutex;

lazy_static! {
    static ref THE_RESOURCE: Mutex<()> = Mutex::new(());
}

#[test]
fn test_closure_convert_lambda_no_free_vars() {
    let _shared = THE_RESOURCE.lock().unwrap();
    dangerously_reset_gensym_count();
    let exp = lexpr::from_str("(lambda ((x : int)) : int (+ x 3))").unwrap();
    let parsed_exp = parse(&exp).unwrap();
    let expected_exp = CExpr::Closure(
        Box::from(CExpr::Lambda(
            vector![
                (String::from("env0"), CType::Env(vector![])),
                (String::from("x"), CType::Int)
            ],
            CType::Int,
            Box::from(CExpr::Binop(
                BinOp::Add,
                Box::from(CExpr::Id(String::from("x"))),
                Box::from(CExpr::Num(3)),
            )),
        )),
        Box::from(CExpr::Env(vector![])),
    );
    println!("{}", expected_exp);
    assert_eq!(closure_convert(&parsed_exp).unwrap(), expected_exp);
}

#[test]
fn test_closure_convert_lambda_yes_free_vars() {
    let _shared = THE_RESOURCE.lock().unwrap();
    dangerously_reset_gensym_count();
    let exp = lexpr::from_str("(let ((y 3)) (lambda ((x : int)) : int (+ x y)))").unwrap();
    let parsed_exp = parse(&exp).unwrap();
    let expected_exp = CExpr::Let(
        vector![(String::from("y"), CExpr::Num(3))],
        Box::from(CExpr::Closure(
            Box::from(CExpr::Lambda(
                vector![
                    (String::from("env0"), CType::Env(vector![CType::Int])),
                    (String::from("x"), CType::Int)
                ],
                CType::Int,
                Box::from(CExpr::Binop(
                    BinOp::Add,
                    Box::from(CExpr::Id(String::from("x"))),
                    Box::from(CExpr::EnvGet(String::from("env0"), String::from("y"))),
                )),
            )),
            Box::from(CExpr::Env(vector![(
                String::from("y"),
                CExpr::Id(String::from("y"))
            )])),
        )),
    );
    assert_eq!(closure_convert(&parsed_exp).unwrap(), expected_exp);
}
