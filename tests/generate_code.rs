use std::collections::BTreeMap;

use scheme_to_wasm::generate_code::{construct_module, gen_instr};
use scheme_to_wasm::parse::parse;
use scheme_to_wasm::type_check::type_check;
use scheme_to_wasm::types::Type;

use parity_wasm::builder;
use parity_wasm::elements::{Instruction, Instructions, ValueType};
use wasmer_runtime::{imports, instantiate, Value};

#[test]
fn test_basic_math() {
    let exp = parse(&lexpr::from_str("(* (+ 3 5) (- 4 2))").unwrap()).unwrap();
    let typed_exp = type_check(&exp).unwrap();
    let mut locals = BTreeMap::new();
    let instructions = gen_instr(&typed_exp, &mut locals).unwrap();
    let module = construct_module(
        "main",
        locals,
        vec![],
        Type::Int,
        Instructions::new(instructions),
    );
    let binary = parity_wasm::serialize(module.clone()).unwrap();
    // output to file for debugging
    parity_wasm::serialize_to_file(
        std::env::current_dir().unwrap().join("basic_math.wasm"),
        module,
    )
    .unwrap();

    let import_object = imports! {};
    let instance = instantiate(&binary, &import_object).unwrap();
    let values = instance.dyn_func("main").unwrap().call(&[]).unwrap();

    assert_eq!(values[0], Value::I64(16));
}

#[test]
fn test_basic_control() {
    let exp = parse(&lexpr::from_str("(if (< 5 3) 10 20)").unwrap()).unwrap();
    let typed_exp = type_check(&exp).unwrap();
    let mut locals = BTreeMap::new();
    let instructions = gen_instr(&typed_exp, &mut locals).unwrap();
    let module = construct_module(
        "main",
        locals,
        vec![],
        Type::Int,
        Instructions::new(instructions),
    );
    let binary = parity_wasm::serialize(module.clone()).unwrap();
    // output to file for debugging
    parity_wasm::serialize_to_file(
        std::env::current_dir().unwrap().join("basic_control.wasm"),
        module,
    )
    .unwrap();

    let import_object = imports! {};
    let instance = instantiate(&binary, &import_object).unwrap();
    let values = instance.dyn_func("main").unwrap().call(&[]).unwrap();

    assert_eq!(values[0], Value::I64(20));
}

#[test]
fn test_basic_let() {
    let exp = parse(&lexpr::from_str("(let ((a (+ 3 4))) (+ 3 a))").unwrap()).unwrap();
    let typed_exp = type_check(&exp).unwrap();
    let mut locals = BTreeMap::new();
    let instructions = gen_instr(&typed_exp, &mut locals).unwrap();
    let module = construct_module(
        "main",
        locals,
        vec![],
        Type::Int,
        Instructions::new(instructions),
    );
    let binary = parity_wasm::serialize(module.clone()).unwrap();
    // output to file for debugging
    parity_wasm::serialize_to_file(
        std::env::current_dir().unwrap().join("basic_let.wasm"),
        module,
    )
    .unwrap();

    let import_object = imports! {};
    let instance = instantiate(&binary, &import_object).unwrap();
    let values = instance.dyn_func("main").unwrap().call(&[]).unwrap();

    assert_eq!(values[0], Value::I64(10));
}

#[test]
fn test_handwritten_record() {
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

    // Can output to a file for debugging/decompiling if necessary.
    parity_wasm::serialize_to_file(
        std::env::current_dir()
            .unwrap()
            .join("handwritten_basic_control.wasm"),
        module,
    )
    .unwrap();

    let import_object = imports! {};
    let instance = instantiate(&output, &import_object).unwrap();
    let values = instance.dyn_func("main").unwrap().call(&[]).unwrap();

    assert_eq!(values[0], Value::I64(10));
}
