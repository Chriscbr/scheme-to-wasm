use im_rc::vector;
use scheme_to_rust::closure_convert::{closure_convert, CExpr, CType};
use scheme_to_rust::parser::{parse, BinOp};

#[test]
fn test_closure_convert() {
    let exp = lexpr::from_str("(lambda ((x : int)) : int (+ x 3))").unwrap();
    let parsed_exp = parse(&exp).unwrap();
    let expected_exp = CExpr::Closure(
        Box::from(CExpr::Lambda(
            vector![
                (String::from("env"), CType::Env(vector![CType::Int])),
                (String::from("x"), CType::Int)
            ],
            CType::Int,
            Box::from(CExpr::Binop(
                BinOp::Add,
                Box::from(CExpr::Sym(String::from("x"))),
                Box::from(CExpr::Num(3)),
            )),
        )),
        Box::from(CExpr::Env(vector![(
            String::from("x"),
            CExpr::Sym(String::from("x"))
        )])),
    );
    assert_eq!(closure_convert(&parsed_exp).is_err(), true);
    // TODO: uncomment this
    // assert_eq!(closure_convert(&parsed_exp), expected_exp);
}
