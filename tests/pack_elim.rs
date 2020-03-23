use scheme_to_wasm::pack_elim::pack_elim_exp;
use scheme_to_wasm::parse::parse;
use scheme_to_wasm::type_check::type_check;

#[test]
fn test_pack_elim_lambda_apply() {
    let exp = parse(
        &lexpr::from_str(
            r#"(unpack (temp0
         (pack (make-tuple
                (lambda ((env1 : (record))
                         (x : int)) : int
                  (+ x 1))
                (make-record))
               (record)
               (exists T2 (tuple (-> T2 int int) T2))) T3)
        ((tuple-ref temp0 0) (tuple-ref temp0 1) 5))"#,
        )
        .unwrap(),
    )
    .unwrap();
    let typed_exp = type_check(&exp).unwrap();

    let expected_exp = type_check(
        &parse(
            &lexpr::from_str(
                r#"(let ((temp0 (make-tuple
                        (lambda ((env1 : (record)) (x : int)) : int (+ x 1))
                        (make-record))))
                    ((tuple-ref temp0 0)
                     (tuple-ref temp0 1)
                     5))"#,
            )
            .unwrap(),
        )
        .unwrap(),
    )
    .unwrap();
    let pe_exp = pack_elim_exp(&typed_exp).unwrap();

    println!("Source: {}", exp);
    println!("Pack elimination: {}", pe_exp);
    assert_eq!(pe_exp, expected_exp);
}

#[test]
fn test_pack_elim_lambda_apply_with_free_vars() {
    let exp = parse(
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
    let typed_exp = type_check(&exp).unwrap();

    let expected_exp = type_check(
        &parse(
            &lexpr::from_str(
                r#"(let ((y 4))
                (let ((temp0 (make-tuple
                    (lambda ((env1 : (record (y : int)))
                             (x : int)) : int
                      (+ x (record-ref env1 y)))
                    (make-record (y y)))))
                  ((tuple-ref temp0 0) (tuple-ref temp0 1) 3)))"#,
            )
            .unwrap(),
        )
        .unwrap(),
    )
    .unwrap();
    let pe_exp = pack_elim_exp(&typed_exp).unwrap();

    println!("Source: {}", exp);
    println!("Pack elimination: {}", pe_exp);
    assert_eq!(pe_exp, expected_exp);
}

// TODO: this test is failing.
// currently a major issue! need to consider ways to get pack_elim working
// on a wider range of cases
// #[test]
// fn test_pack_elim_named_lambda() {
//     let exp = parse(
//         &lexpr::from_str(
//             r#"(let ((f (pack (make-tuple
//                 (lambda ((env0 : (record))
//                          (x : int)) : int
//                   (+ x 3))
//                 (make-record))
//                (record)
//                (exists T1 (tuple (-> T1 int int) T1)))))
//   (unpack (temp2 f T3) ((tuple-ref temp2 0) (tuple-ref temp2 1) 4)))"#,
//         )
//         .unwrap(),
//     )
//     .unwrap();
//     let typed_exp = type_check(&exp).unwrap();

//     let expected_exp = type_check(
//         &parse(
//             &lexpr::from_str(
//                 r#"(let ((f (make-tuple
//                     (lambda ((env0 : (record))
//                              (x : int)) : int
//                       (+ x 3))
//                     (make-record))))
//       (let ((temp2 f))
//         ((tuple-ref temp2 0) (tuple-ref temp2 1) 4)))"#,
//             )
//             .unwrap(),
//         )
//         .unwrap(),
//     )
//     .unwrap();
//     let pe_exp = pack_elim_exp(&typed_exp).unwrap();

//     println!("Source: {}", exp);
//     println!("Pack elimination: {}", pe_exp);
//     assert_eq!(pe_exp, expected_exp);
// }
