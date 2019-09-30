use im_rc::vector;
use scheme_to_rust::closure_convert::{closure_convert_main, CExpr, CType};
use scheme_to_rust::parser::{parse, BinOp};

#[test]
fn test_closure_convert_lambda_no_free_vars() {
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
                Box::from(CExpr::Sym(String::from("x"))),
                Box::from(CExpr::Num(3)),
            )),
        )),
        Box::from(CExpr::Env(vector![])),
    );
    // assert_eq!(closure_convert_main(&parsed_exp).unwrap(), expected_exp);
}

#[test]
fn test_closure_convert_lambda_yes_free_vars() {
    let exp = lexpr::from_str("(let ((y 3)) (lambda ((x : int)) : int (+ x y)))").unwrap();
    let parsed_exp = parse(&exp).unwrap();
    let expected_exp = CExpr::Let(
        vector![(String::from("y"), CExpr::Num(3))],
        Box::from(CExpr::Closure(
            Box::from(CExpr::Lambda(
                vector![
                    (String::from("env0"), CType::Env(vector![CType::Unknown])), // TODO
                    (String::from("x"), CType::Int)
                ],
                CType::Int,
                Box::from(CExpr::Binop(
                    BinOp::Add,
                    Box::from(CExpr::Sym(String::from("x"))),
                    Box::from(CExpr::EnvGet(String::from("env0"), String::from("y"))),
                )),
            )),
            Box::from(CExpr::Env(vector![(
                String::from("y"),
                CExpr::Sym(String::from("y"))
            )])),
        )),
    );
    // assert_eq!(closure_convert_main(&parsed_exp).unwrap(), expected_exp);
}
