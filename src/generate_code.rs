// Just experimenting with WASM

// Some useful examples at
// https://github.com/paritytech/parity-wasm/tree/master/examples

use crate::common::{BinOp, Expr, ExprKind};
use crate::types::Type;

use parity_wasm::builder;
use parity_wasm::elements::{BlockType, Instruction, Instructions, Module, ValueType};

#[derive(Clone, Debug)]
pub struct CodeGenerateError(String);

// Allows other errors to wrap this one
impl std::error::Error for CodeGenerateError {}

impl From<&str> for CodeGenerateError {
    fn from(message: &str) -> Self {
        CodeGenerateError(String::from(message))
    }
}

impl std::fmt::Display for CodeGenerateError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "CodeGenerateError: {}", self.0)
    }
}

fn gen_instr_binop(
    op: BinOp,
    arg1: &Expr,
    arg2: &Expr,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    match op {
        BinOp::Add => {
            let arg1_instr = gen_instr(arg1)?;
            let arg2_instr = gen_instr(arg2)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64Add]].concat())
        }
        BinOp::Subtract => {
            let arg1_instr = gen_instr(arg1)?;
            let arg2_instr = gen_instr(arg2)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64Sub]].concat())
        }
        BinOp::Multiply => {
            let arg1_instr = gen_instr(arg1)?;
            let arg2_instr = gen_instr(arg2)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64Mul]].concat())
        }
        BinOp::Divide => {
            let arg1_instr = gen_instr(arg1)?;
            let arg2_instr = gen_instr(arg2)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64DivS]].concat())
        }
        BinOp::LessThan => {
            let arg1_instr = gen_instr(arg1)?;
            let arg2_instr = gen_instr(arg2)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64LtS]].concat())
        }
        BinOp::GreaterThan => {
            let arg1_instr = gen_instr(arg1)?;
            let arg2_instr = gen_instr(arg2)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64GtS]].concat())
        }
        BinOp::LessOrEqual => {
            let arg1_instr = gen_instr(arg1)?;
            let arg2_instr = gen_instr(arg2)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64LeS]].concat())
        }
        BinOp::GreaterOrEqual => {
            let arg1_instr = gen_instr(arg1)?;
            let arg2_instr = gen_instr(arg2)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64GeS]].concat())
        }
        BinOp::EqualTo => {
            let arg1_instr = gen_instr(arg1)?;
            let arg2_instr = gen_instr(arg2)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64Eq]].concat())
        }
        BinOp::And => {
            let arg1_instr = gen_instr(arg1)?;
            let arg2_instr = gen_instr(arg2)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64And]].concat())
        }
        BinOp::Or => {
            let arg1_instr = gen_instr(arg1)?;
            let arg2_instr = gen_instr(arg2)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64Or]].concat())
        }
        BinOp::Concat => Err(CodeGenerateError::from("Unhandled binop: concat.")),
    }
}

fn gen_instr_if(
    pred: &Expr,
    cons: &Expr,
    alt: &Expr,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let pred_instr = gen_instr(pred)?;
    let cons_instr = gen_instr(cons)?;
    let alt_instr = gen_instr(alt)?;

    // TODO: remove hardcoded block type
    let block_type = BlockType::Value(ValueType::I64);
    Ok([
        pred_instr,
        vec![Instruction::If(block_type)],
        cons_instr,
        vec![Instruction::Else],
        alt_instr,
        vec![Instruction::End],
    ]
    .concat())
}

fn gen_instr(exp: &Expr) -> Result<Vec<Instruction>, CodeGenerateError> {
    let instructions: Result<Vec<Instruction>, CodeGenerateError> = match &*exp.kind {
        ExprKind::Num(x) => Ok(vec![Instruction::I64Const(*x)]),
        ExprKind::Bool(x) => Ok(vec![Instruction::I64Const(*x as i64)]),
        // ExprKind::Str(_) => Ok(Type::Str),
        // ExprKind::Id(sym) => match env.find(sym.as_str()) {
        //     Some(val) => Ok(val.clone()),
        //     None => Err(TypeCheckError(format!(
        //         "Not a recognized function name: {}.",
        //         sym
        //     ))),
        // },
        ExprKind::Binop(op, arg1, arg2) => Ok(gen_instr_binop(*op, &arg1, &arg2)?),
        ExprKind::If(pred, cons, alt) => Ok(gen_instr_if(&pred, &cons, &alt)?),
        // ExprKind::Let(bindings, body) => tc_let_with_env(&bindings, &body, env),
        // ExprKind::Lambda(params, ret_typ, body) => {
        //     tc_lambda_with_env(&params, &ret_typ, &body, env)
        // }
        // ExprKind::Record(bindings) => tc_record_with_env(&bindings, env),
        // ExprKind::RecordGet(record, key) => tc_record_get_with_env(&record, &key, env),
        // ExprKind::Begin(exps) => tc_begin_with_env(&exps, env),
        // ExprKind::Set(sym, exp) => tc_set_bang_with_env(&sym, &exp, env),
        // ExprKind::Cons(first, rest) => tc_cons_with_env(&first, &rest, env),
        // ExprKind::Car(exp) => tc_car_with_env(&exp, env),
        // ExprKind::Cdr(exp) => tc_cdr_with_env(&exp, env),
        // ExprKind::IsNull(exp) => tc_is_null_with_env(&exp, env),
        // ExprKind::Null(typ) => Ok(Type::List(Box::new(typ.clone()))),
        // ExprKind::Tuple(exps) => tc_tuple_with_env(&exps, env),
        // ExprKind::TupleGet(tup, key) => tc_tuple_get_with_env(&tup, *key, env),
        // ExprKind::Pack(val, sub, exist) => tc_pack_with_env(&val, &sub, &exist, env),
        // ExprKind::Unpack(var, package, typ_sub, body) => {
        //     tc_unpack_with_env(&var, &package, *typ_sub, &body, env)
        // }
        // ExprKind::FnApp(func, args) => tc_apply_with_env(&func, &args, env),
        _ => Err(CodeGenerateError::from("Unhandled expression kind.")),
    };
    Ok(instructions?)
}

fn construct_module(
    name: &str,
    param_types: Vec<Type>,
    ret_type: Type,
    mut instructions: Instructions,
) -> Module {
    let wasm_ret_type = match ret_type {
        Type::Int => ValueType::I64,
        _ => panic!("Unhandled return type."),
    };

    let wasm_param_types = param_types
        .iter()
        .map(|typ| match typ {
            Type::Int => ValueType::I64,
            _ => panic!("Unhandled parameter type."),
        })
        .collect::<Vec<ValueType>>();

    instructions.elements_mut().push(Instruction::End);

    builder::module()
        .function()
        .signature()
        .with_params(wasm_param_types)
        .with_return_type(Some(wasm_ret_type))
        .build()
        .body()
        .with_instructions(instructions)
        .build()
        .build()
        .export()
        .field(name)
        .internal()
        .func(0)
        .build()
        .build()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::parse;

    use wasmer_runtime::{imports, instantiate, Value};

    #[test]
    fn test_basic_math() {
        let exp = parse(&lexpr::from_str("(* (+ 3 5) (- 4 2))").unwrap()).unwrap();
        let instructions = gen_instr(&exp).unwrap();
        let module = construct_module("main", vec![], Type::Int, Instructions::new(instructions));
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
        let instructions = gen_instr(&exp).unwrap();
        let module = construct_module("main", vec![], Type::Int, Instructions::new(instructions));
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
            .function()
            .signature()
            .with_return_type(Some(ValueType::I64))
            .build()
            .body()
            .with_instructions(Instructions::new(vec![
                Instruction::I64Const(5),
                Instruction::I64Const(5),
                Instruction::I64LtS,
                Instruction::If(BlockType::Value(ValueType::I64)),
                Instruction::I64Const(10),
                Instruction::Else,
                Instruction::I64Const(20),
                Instruction::End,
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

        assert_eq!(values[0], Value::I64(20));
    }
}
