use std::collections::BTreeMap;

use scheme_to_wasm::generate_code::{construct_module, gen_instr};
use scheme_to_wasm::parse::parse;
use scheme_to_wasm::type_check::type_check;
use scheme_to_wasm::types::Type;

use parity_wasm::builder;
use parity_wasm::elements::{Instruction, Instructions, Local, ValueType};
use wasmer_runtime::{imports, instantiate, Value};

#[test]
fn test_basic_math() {
    let exp = parse(&lexpr::from_str("(* (+ 3 5) (- 4 2))").unwrap()).unwrap();
    let mut locals = BTreeMap::new();
    let instructions = gen_instr(&exp, &mut locals).unwrap();
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
    let mut locals = BTreeMap::new();
    let instructions = gen_instr(&exp, &mut locals).unwrap();
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
fn test_handwritten_add_one() {
    let module = builder::module()
        .function()
        .signature()
        .with_param(ValueType::I64)
        .with_return_type(Some(ValueType::I64))
        .build()
        .body()
        .with_instructions(Instructions::new(vec![
            Instruction::GetLocal(0),
            Instruction::I64Const(1),
            Instruction::I64Add,
            Instruction::End,
        ]))
        .build()
        .build()
        .export()
        .field("add_one")
        .internal()
        .func(0)
        .build()
        .build();

    let output = parity_wasm::serialize(module.clone()).unwrap();

    // Can output to a file for debugging/decompiling if necessary.
    parity_wasm::serialize_to_file(
        std::env::current_dir()
            .unwrap()
            .join("handwritten_add_one.wasm"),
        module,
    )
    .unwrap();

    let import_object = imports! {};
    let instance = instantiate(&output, &import_object).unwrap();
    let values = instance
        .dyn_func("add_one")
        .unwrap()
        .call(&[Value::I64(42)])
        .unwrap();

    assert_eq!(values[0], Value::I64(43));
}

#[test]
fn test_handwritten_basic_control() {
    let module = builder::module()
        .memory()
        .with_min(32)
        .with_max(Some(32))
        .build()
        .function()
        .signature()
        .with_return_type(Some(ValueType::I64))
        .build()
        .body()
        .with_locals(vec![Local::new(1, ValueType::I64)])
        .with_instructions(Instructions::new(vec![
            Instruction::I64Const(5),
            Instruction::SetLocal(0),
            Instruction::GetLocal(0),
            Instruction::I64Const(1),
            Instruction::I64Add,
            Instruction::GetLocal(0),
            Instruction::I64Mul,
            Instruction::I64Const(2),
            Instruction::I64DivS,
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

    assert_eq!(values[0], Value::I64(15));
}
