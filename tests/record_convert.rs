use scheme_to_wasm::parse::parse;
use scheme_to_wasm::record_convert::record_convert;
use scheme_to_wasm::type_check::type_check;

#[test]
fn test_record_convert_simple() {
    let exp = parse(&lexpr::from_str("(make-record (bar 3) (foo \"hello\"))").unwrap()).unwrap();
    let exp_typ = type_check(&exp);
    assert_eq!(exp_typ.is_err(), false);

    let expected_exp = parse(&lexpr::from_str("(make-tuple 3 \"hello\")").unwrap()).unwrap();
    let rc_exp = record_convert(&exp).unwrap();
    type_check(&rc_exp).unwrap();

    println!("Source: {}", exp);
    println!("Record converted: {}", rc_exp);
    assert_eq!(rc_exp, expected_exp);
}

#[test]
fn test_record_convert_order_invariant() {
    let exp = parse(&lexpr::from_str("(make-record (foo \"hello\") (bar 3))").unwrap()).unwrap();
    let exp_typ = type_check(&exp);
    assert_eq!(exp_typ.is_err(), false);

    let expected_exp = parse(&lexpr::from_str("(make-tuple 3 \"hello\")").unwrap()).unwrap();
    let rc_exp = record_convert(&exp).unwrap();
    type_check(&rc_exp).unwrap();

    println!("Source: {}", exp);
    println!("Record converted: {}", rc_exp);
    assert_eq!(rc_exp, expected_exp);
}

#[test]
fn test_record_convert_record_get() {
    let exp =
        parse(&lexpr::from_str("(record-ref (make-record (foo \"hello\") (bar 3)) foo)").unwrap())
            .unwrap();
    let exp_typ = type_check(&exp);
    assert_eq!(exp_typ.is_err(), false);

    let expected_exp =
        parse(&lexpr::from_str("(tuple-ref (make-tuple 3 \"hello\") 1)").unwrap()).unwrap();
    let rc_exp = record_convert(&exp).unwrap();
    type_check(&rc_exp).unwrap();

    println!("Source: {}", exp);
    println!("Record converted: {}", rc_exp);
    assert_eq!(rc_exp, expected_exp);
}
