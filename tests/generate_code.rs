use scheme_to_wasm::common::Expr;
use scheme_to_wasm::generate_code::{construct_module, gen_instr, CodeGenerateState};
use scheme_to_wasm::parse::parse;
use scheme_to_wasm::type_check::type_check;

use parity_wasm::builder;
use parity_wasm::elements::{Instruction, Instructions, ValueType};
use wasmer_runtime::{imports, instantiate, Value};

/// Compiles the expression into wasm and outputs the resulting value
///
/// Does not assume that the Expr has been type checked.
fn test_runner(exp: Expr, test_name: &str) -> Value {
    let typed_exp = type_check(&exp).unwrap();
    let exp_type = typed_exp.checked_type.clone().unwrap();
    let mut state = CodeGenerateState::default();
    let instructions = dbg!(gen_instr(&typed_exp, &mut state).unwrap());
    let module = construct_module(
        "main",
        state,
        vec![],
        exp_type,
        Instructions::new(instructions),
    );
    let binary = parity_wasm::serialize(module.clone()).unwrap();

    // output to file for debugging
    parity_wasm::serialize_to_file(std::env::current_dir().unwrap().join(test_name), module)
        .unwrap();

    let import_object = imports! {};
    let instance = instantiate(&binary, &import_object).unwrap();
    let values = instance.dyn_func("main").unwrap().call(&[]).unwrap();

    values[0].clone()
}

#[test]
fn test_basic_math() {
    let exp = parse(&lexpr::from_str("(* (+ 3 5) (- 4 2))").unwrap()).unwrap();
    let output = test_runner(exp, "basic_math.wasm");
    assert_eq!(output, Value::I64(16));
}

#[test]
fn test_basic_control() {
    let exp = parse(&lexpr::from_str("(if (< 5 3) 10 20)").unwrap()).unwrap();
    let output = test_runner(exp, "basic_control1.wasm");
    assert_eq!(output, Value::I64(20));

    let exp = parse(&lexpr::from_str("(if (or (< -2 7) (null? 3)) 10 20)").unwrap()).unwrap();
    let output = test_runner(exp, "basic_control2.wasm");
    assert_eq!(output, Value::I64(10));
}

#[test]
fn test_basic_let() {
    let exp = parse(&lexpr::from_str("(let ((a (+ 3 4))) (+ 3 a))").unwrap()).unwrap();
    let output = test_runner(exp, "basic_let.wasm");
    assert_eq!(output, Value::I64(10));
}

#[test]
fn test_basic_tuple() {
    let exp = parse(&lexpr::from_str("(tuple-ref (make-tuple 3 4) 1)").unwrap()).unwrap();
    let output = test_runner(exp, "basic_tuple1.wasm");
    assert_eq!(output, Value::I64(4));

    let exp =
        parse(&lexpr::from_str("(let ((a (make-tuple 23 -9 304))) (tuple-ref a 2))").unwrap())
            .unwrap();
    let output = test_runner(exp, "basic_tuple2.wasm");
    assert_eq!(output, Value::I64(304));

    let exp = parse(
        &lexpr::from_str(
            "(let ((a (make-tuple -3 5))
                   (b (make-tuple 4 -1)))
            (+ (tuple-ref a 1) (tuple-ref b 0)))",
        )
        .unwrap(),
    )
    .unwrap();
    let output = test_runner(exp, "basic_tuple3.wasm");
    assert_eq!(output, Value::I64(9));
}

#[test]
fn test_nested_tuple() {
    let exp = parse(
        &lexpr::from_str(
            "(tuple-ref (tuple-ref (make-tuple (make-tuple 5 6) 7 (make-tuple 8 9)) 0) 1)",
        )
        .unwrap(),
    )
    .unwrap();
    let output = test_runner(exp, "nested_tuple1.wasm");
    assert_eq!(output, Value::I64(6));
}

#[test]
fn test_basic_cons() {
    let exp = parse(&lexpr::from_str("(cons 3 (null int))").unwrap()).unwrap();
    let output = test_runner(exp, "basic_cons1.wasm");
    assert_eq!(output, Value::I32(0));

    let exp = parse(&lexpr::from_str("(cons 3 (cons 4 (null int)))").unwrap()).unwrap();
    let output = test_runner(exp, "basic_cons2.wasm");
    assert_eq!(output, Value::I32(12));
}

#[test]
fn test_handwritten_tuple() {
    let module = builder::module()
        .memory()
        .with_min(32)
        .with_max(None)
        .build()
        .function()
        .signature()
        .with_return_type(Some(ValueType::I64))
        .build()
        .body()
        .with_instructions(Instructions::new(vec![
            Instruction::I32Const(0),    // optional offset value
            Instruction::I64Const(10),   // value that gets stored
            Instruction::I64Store(0, 0), // offset value and (optional) alignment value
            Instruction::I32Const(0),
            Instruction::I64Load(0, 0),
            Instruction::End,
        ]))
        .build()
        .build()
        .export()
        .field("main")
        .internal()
        .func(0)
        .build()
        .build();

    let output = parity_wasm::serialize(module.clone()).unwrap();

    // output to file for debugging
    parity_wasm::serialize_to_file(
        std::env::current_dir()
            .unwrap()
            .join("handwritten_tuple.wasm"),
        module,
    )
    .unwrap();

    let import_object = imports! {};
    let instance = instantiate(&output, &import_object).unwrap();
    let values = instance.dyn_func("main").unwrap().call(&[]).unwrap();

    assert_eq!(values[0], Value::I64(10));
}
