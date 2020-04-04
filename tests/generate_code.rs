use scheme_to_wasm::common::{Expr, ExprKind, Prog, TypedExpr};
use scheme_to_wasm::compile::compile_exp;
use scheme_to_wasm::generate_code::{
    construct_module, construct_module_from_prog, gen_instr, CodeGenerateState,
};
use scheme_to_wasm::parse::parse;
use scheme_to_wasm::type_check::type_check;
use scheme_to_wasm::types::Type;

use im_rc::vector;
use parity_wasm::builder;
use parity_wasm::elements::{Instruction, Instructions, Module, ValueType};
use wasmer_runtime::{imports, instantiate, Value};

fn output_wasm_to_file(module: Module, test_name: &str) {
    let output_dir = std::env::current_dir().unwrap().join("wasm-output");
    std::fs::create_dir_all(output_dir.clone()).unwrap();
    parity_wasm::serialize_to_file(output_dir.join(test_name), module).unwrap();
}

/// Compiles the (untyped) expression into wasm and outputs the resulting value
fn test_runner_exp(exp: Expr, test_name: &str) -> Value {
    let typed_exp = type_check(&exp).unwrap();
    let mut state = CodeGenerateState::default();
    let instructions = gen_instr(&typed_exp, &mut state).unwrap();
    let module = construct_module("$$MAIN$$", state, vec![], Instructions::new(instructions));
    let module = module.build();
    let binary = parity_wasm::serialize(module.clone()).unwrap();
    output_wasm_to_file(module, test_name);

    let import_object = imports! {};
    let instance = instantiate(&binary, &import_object).unwrap();
    let values = instance.dyn_func("$$MAIN$$").unwrap().call(&[]).unwrap();

    values[0].clone()
}

/// Compiles the (typed) program into wasm and outputs the resulting value
fn test_runner_prog(prog: Prog<TypedExpr>, test_name: &str) -> Value {
    let module = construct_module_from_prog(&prog).unwrap();
    let binary = parity_wasm::serialize(module.clone()).unwrap();
    output_wasm_to_file(module, test_name);

    let import_object = imports! {};
    let instance = instantiate(&binary, &import_object).unwrap();
    let values = instance.dyn_func("$$MAIN$$").unwrap().call(&[]).unwrap();

    values[0].clone()
}

#[test]
fn test_compile_math() {
    let exp = parse(&lexpr::from_str("(* (+ 3 5) (- 4 2))").unwrap()).unwrap();
    let output = test_runner_exp(exp, "math.wasm");
    assert_eq!(output, Value::I32(16));
}

#[test]
fn test_compile_control() {
    let exp = parse(&lexpr::from_str("(if (< 5 3) 10 20)").unwrap()).unwrap();
    let output = test_runner_exp(exp, "control1.wasm");
    assert_eq!(output, Value::I32(20));

    let exp = parse(&lexpr::from_str("(if (or (< -2 7) (null? 3)) 10 20)").unwrap()).unwrap();
    let output = test_runner_exp(exp, "control2.wasm");
    assert_eq!(output, Value::I32(10));
}

#[test]
fn test_compile_let() {
    let exp = parse(&lexpr::from_str("(let ((a (+ 3 4))) (+ 3 a))").unwrap()).unwrap();
    let output = test_runner_exp(exp, "let.wasm");
    assert_eq!(output, Value::I32(10));
}

#[test]
fn test_compile_tuple() {
    let exp = parse(&lexpr::from_str("(tuple-ref (make-tuple 3 4) 1)").unwrap()).unwrap();
    let output = test_runner_exp(exp, "tuple1.wasm");
    assert_eq!(output, Value::I32(4));

    let exp =
        parse(&lexpr::from_str("(let ((a (make-tuple 23 -9 304))) (tuple-ref a 2))").unwrap())
            .unwrap();
    let output = test_runner_exp(exp, "tuple2.wasm");
    assert_eq!(output, Value::I32(304));

    let exp = parse(
        &lexpr::from_str(
            "(let ((a (make-tuple -3 5))
                   (b (make-tuple 4 -1)))
            (+ (tuple-ref a 1) (tuple-ref b 0)))",
        )
        .unwrap(),
    )
    .unwrap();
    let output = test_runner_exp(exp, "tuple3.wasm");
    assert_eq!(output, Value::I32(9));
}

#[test]
fn test_compile_nested_tuple() {
    let exp = parse(
        &lexpr::from_str(
            "(tuple-ref (tuple-ref (make-tuple (make-tuple 5 6) 7 (make-tuple 11 13)) 2) 1)",
        )
        .unwrap(),
    )
    .unwrap();
    let output = test_runner_exp(exp, "nested_tuple.wasm");
    assert_eq!(output, Value::I32(13));
}

#[test]
fn test_compile_empty_tuple() {
    let exp = parse(
        &lexpr::from_str(
            "(let ((a (make-tuple)))
                (begin (make-tuple) (+ 3 5)))",
        )
        .unwrap(),
    )
    .unwrap();
    let output = test_runner_exp(exp, "empty_tuple.wasm");
    assert_eq!(output, Value::I32(8));
}

#[test]
fn test_compile_cons() {
    let exp = parse(&lexpr::from_str("(car (cons 3 (null int)))").unwrap()).unwrap();
    let output = test_runner_exp(exp, "cons1.wasm");
    assert_eq!(output, Value::I32(3));

    let exp = parse(&lexpr::from_str("(car (cons 3 (cons 4 (null int))))").unwrap()).unwrap();
    let output = test_runner_exp(exp, "cons2.wasm");
    assert_eq!(output, Value::I32(3));

    let exp = parse(&lexpr::from_str("(car (cdr (cons 3 (cons 4 (null int)))))").unwrap()).unwrap();
    let output = test_runner_exp(exp, "cons3.wasm");
    assert_eq!(output, Value::I32(4));
}

// Tests lists (made with cons) parameterized over a type of more than 4 bytes
#[test]
fn test_compile_cons_tuple() {
    let exp = parse(
        &lexpr::from_str("(tuple-ref (car (cons (make-tuple 3 4) (null (tuple int int)))) 1)")
            .unwrap(),
    )
    .unwrap();
    let output = test_runner_exp(exp, "cons_tuple1.wasm");
    assert_eq!(output, Value::I32(4));

    let exp = parse(
        &lexpr::from_str("(tuple-ref (car (cdr (cons (make-tuple 3 4) (cons (make-tuple 5 6) (null (tuple int int)))))) 1)")
            .unwrap(),
    )
    .unwrap();
    let output = test_runner_exp(exp, "cons_tuple2.wasm");
    assert_eq!(output, Value::I32(6));

    let exp = parse(
        &lexpr::from_str("(null? (cdr (cons (make-tuple 3 4) (null (tuple int int)))))").unwrap(),
    )
    .unwrap();
    let output = test_runner_exp(exp, "cons_tuple3.wasm");
    assert_eq!(output, Value::I32(1)); // true

    let exp = parse(
        &lexpr::from_str("(null? (car (cons (make-tuple) (cons (make-tuple) (null (tuple))))))")
            .unwrap(),
    )
    .unwrap();
    let output = test_runner_exp(exp, "cons_tuple4.wasm");
    assert_eq!(output, Value::I32(0)); // false

    let exp = parse(
        &lexpr::from_str(
            "(begin (make-tuple 3 4)
            (null? (cdr (cdr (cons (make-tuple) (cons (make-tuple) (null (tuple)))))))
        )",
        )
        .unwrap(),
    )
    .unwrap();
    let output = test_runner_exp(exp, "cons_tuple5.wasm");
    assert_eq!(output, Value::I32(1)); // true
}

// Tests tuples containing complex list types
#[test]
fn test_compile_tuple_cons_tuple() {
    let exp = parse(
        &lexpr::from_str(
            "(tuple-ref
                            (car
                                (tuple-ref
                                    (make-tuple (cons (make-tuple 15 19)
                                                      (null (tuple int int)))
                                                7)
                                    0))
                           1)",
        )
        .unwrap(),
    )
    .unwrap();
    let output = test_runner_exp(exp, "tuple_cons_tuple1.wasm");
    assert_eq!(output, Value::I32(19));
}

#[test]
fn test_compile_is_null() {
    let exp = parse(&lexpr::from_str("(null? (null int))").unwrap()).unwrap();
    let output = test_runner_exp(exp, "is_null1.wasm");
    assert_eq!(output, Value::I32(1)); // true

    let exp = parse(&lexpr::from_str("(null? (cons 3 (null int)))").unwrap()).unwrap();
    let output = test_runner_exp(exp, "is_null2.wasm");
    assert_eq!(output, Value::I32(0)); // false

    let exp = parse(&lexpr::from_str("(null? 7)").unwrap()).unwrap();
    let output = test_runner_exp(exp, "is_null3.wasm");
    assert_eq!(output, Value::I32(0)); // false

    let exp = parse(&lexpr::from_str("(null? (make-tuple))").unwrap()).unwrap();
    let output = test_runner_exp(exp, "is_null4.wasm");
    assert_eq!(output, Value::I32(0)); // false
}

#[test]
fn test_compile_begin() {
    let exp =
        parse(&lexpr::from_str("(begin (+ 3 5) (+ 4 7) (make-tuple 1 2) 42)").unwrap()).unwrap();
    let output = test_runner_exp(exp, "begin1.wasm");
    assert_eq!(output, Value::I32(42));
}

#[test]
fn test_compile_set() {
    let exp = parse(&lexpr::from_str("(let ((a 3)) (set! a 5))").unwrap()).unwrap();
    let output = test_runner_exp(exp, "set1.wasm");
    assert_eq!(output, Value::I32(5));

    let exp = parse(&lexpr::from_str("(let ((a 3)) (begin (set! a 5) (+ a 2)))").unwrap()).unwrap();
    let output = test_runner_exp(exp, "set2.wasm");
    assert_eq!(output, Value::I32(7));
}

#[test]
fn test_compile_func_without_closure_conversion() {
    let func = parse(&lexpr::from_str("(lambda ((x : int)) : int (+ x 1))").unwrap()).unwrap();
    let typed_func = type_check(&func).unwrap();
    // let exp = parse(&lexpr::from_str("(func0 5)").unwrap()).unwrap();
    let typed_exp = TypedExpr::new(
        Type::Int,
        ExprKind::FnApp(
            TypedExpr::new(
                Type::Func(vector![Type::Int], Box::from(Type::Int)),
                ExprKind::Id(String::from("func0")),
            ),
            vector![TypedExpr::new(Type::Int, ExprKind::Num(5))],
        ),
    );
    // let typed_exp = type_check(&exp).unwrap();
    let prog = Prog {
        fns: vector![(String::from("func0"), typed_func)],
        exp: typed_exp,
    };
    let output = test_runner_prog(prog, "func_handwritten1.wasm");
    assert_eq!(output, Value::I32(6));
}

#[test]
fn test_compile_func_simple() {
    let exp = parse(&lexpr::from_str("((lambda ((x : int)) : int (+ x 1)) 5)").unwrap()).unwrap();
    let prog = compile_exp(&exp).unwrap();
    let output = test_runner_prog(prog, "func_simple.wasm");
    assert_eq!(output, Value::I32(6));
}

#[test]
fn test_compile_func_free_var() {
    let exp =
        parse(&lexpr::from_str("(let ((a 3)) ((lambda ((x : int)) : int (+ x a)) 5))").unwrap())
            .unwrap();
    let prog = compile_exp(&exp).unwrap();
    let output = test_runner_prog(prog, "func_free_var.wasm");
    assert_eq!(output, Value::I32(8));
}

#[test]
fn test_compile_named_func() {
    let exp =
        parse(&lexpr::from_str("(let ((a (lambda ((x : int)) : int (+ x 1)))) (a 3))").unwrap())
            .unwrap();
    let prog = compile_exp(&exp).unwrap();
    let output = test_runner_prog(prog, "named_func.wasm");
    assert_eq!(output, Value::I32(4));
}

#[test]
fn test_compile_func_two_arg() {
    let exp =
        parse(&lexpr::from_str("((lambda ((x : int) (y : int)) : int (* x y)) 5 6)").unwrap())
            .unwrap();
    let prog = compile_exp(&exp).unwrap();
    let output = test_runner_prog(prog, "func_two_arg.wasm");
    assert_eq!(output, Value::I32(30));
}

#[test]
fn test_compile_curried_func() {
    let exp = parse(
        &lexpr::from_str(
            "(let ((f (lambda ((x : int)) : (-> int int)
            (lambda ((y : int)) : int (+ x y)))))
   ((f 4) 3))",
        )
        .unwrap(),
    )
    .unwrap();
    let prog = compile_exp(&exp).unwrap();
    let output = test_runner_prog(prog, "curried_func.wasm");
    assert_eq!(output, Value::I32(7));
}

#[test]
fn test_compile_extra_curried_func() {
    let exp = parse(
        &lexpr::from_str(
            "(let ((f (lambda ((x : int)) : (-> int (-> int int))
            (lambda ((y : int)) : (-> int int)
            (lambda ((z : int)) : int (+ x (+ y z)))))))
   (((f 4) 3) 2))",
        )
        .unwrap(),
    )
    .unwrap();
    let prog = compile_exp(&exp).unwrap();
    let output = test_runner_prog(prog, "extra_curried_func.wasm");
    assert_eq!(output, Value::I32(9));
}

#[test]
fn test_compile_make_adder() {
    let exp = parse(
        &lexpr::from_str(
            "(let ((make-adder (lambda ((x : int)) : (-> int int)
            (lambda ((y : int)) : int (+ x y)))))
   (let ((add3 (make-adder 3))) (* (add3 2) (add3 4))))",
        )
        .unwrap(),
    )
    .unwrap();
    let prog = compile_exp(&exp).unwrap();
    let output = test_runner_prog(prog, "make_adder.wasm");
    assert_eq!(output, Value::I32(35));
}

#[test]
fn test_compile_make_adder_scoped() {
    let exp = parse(
        &lexpr::from_str(
            "(let ((make-adder (lambda ((x : int)) : (-> int int)
                (lambda ((y : int)) : int (+ x y)))))
                    (let ((f (let ((xx 100))
                        (make-adder xx))))
                    (f 200)))",
        )
        .unwrap(),
    )
    .unwrap();
    let prog = compile_exp(&exp).unwrap();
    let output = test_runner_prog(prog, "make_adder_scoped.wasm");
    assert_eq!(output, Value::I32(300));
}

#[test]
fn test_compile_prepend() {
    let exp = parse(
        &lexpr::from_str(
            r#"
(let ((prepend1 (lambda ((lst : (list int))) : (list int)
            (cons 1 lst))))
(let ((a (prepend1 (cons 3 (cons 4 (null int))))))
  (+ (car a) (car (cdr a)))))
                "#,
        )
        .unwrap(),
    )
    .unwrap();
    let prog = compile_exp(&exp).unwrap();
    let output = test_runner_prog(prog, "prepend1.wasm");
    assert_eq!(output, Value::I32(4));
}

#[test]
fn test_handwritten_lambda() {
    let module = builder::module()
        .table()
        .with_min(32)
        .with_max(None)
        .with_element(0, vec![0])
        .build()
        .memory()
        .with_min(32)
        .with_max(None)
        .build()
        // add1 function
        .function()
        .signature()
        .with_param(ValueType::I32)
        .with_return_type(Some(ValueType::I32))
        .build()
        .body()
        .with_instructions(Instructions::new(vec![
            Instruction::GetLocal(0),
            Instruction::I32Const(1),
            Instruction::I32Add,
            Instruction::End,
        ]))
        .build()
        .build()
        .export()
        .field("add1")
        .internal()
        .func(0)
        .build()
        // main function
        .function()
        .signature()
        .with_return_type(Some(ValueType::I32))
        .build()
        .body()
        .with_instructions(Instructions::new(vec![
            Instruction::I32Const(5),
            Instruction::I32Const(0),
            Instruction::CallIndirect(0, 0),
            Instruction::End,
        ]))
        .build()
        .build()
        .export()
        .field("main")
        .internal()
        .func(1)
        .build()
        .build();
    let binary = parity_wasm::serialize(module.clone()).unwrap();
    output_wasm_to_file(module, "handwritten_lambda.wasm");

    let import_object = imports! {};
    let instance = instantiate(&binary, &import_object).unwrap();
    let values = instance.dyn_func("main").unwrap().call(&[]).unwrap();

    assert_eq!(values[0], Value::I32(6));
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
        .with_return_type(Some(ValueType::I32))
        .build()
        .body()
        .with_instructions(Instructions::new(vec![
            Instruction::I32Const(0),    // offset argument
            Instruction::I32Const(10),   // value that gets stored
            Instruction::I32Store(0, 0), // (optional) alignment value, and offset value
            Instruction::I32Const(0),    // offset argument
            Instruction::I32Load(0, 0),  // (optimal) alignment value, and offset value
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
    let binary = parity_wasm::serialize(module.clone()).unwrap();
    output_wasm_to_file(module, "handwritten_tuple.wasm");

    let import_object = imports! {};
    let instance = instantiate(&binary, &import_object).unwrap();
    let values = instance.dyn_func("main").unwrap().call(&[]).unwrap();

    assert_eq!(values[0], Value::I32(10));
}
