// Just experimenting with WASM

// Some useful examples at
// https://github.com/paritytech/parity-wasm/tree/master/examples

use crate::common::{BinOp, Expr, ExprKind};
use crate::types::Type;

use std::collections::BTreeMap;

use im_rc::Vector;
use parity_wasm::builder;
use parity_wasm::elements::{BlockType, Instruction, Instructions, Local, Module, ValueType};

type LocalsMap<'a> = BTreeMap<String, (u32, ValueType)>;

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

fn convert_to_wasm_type(typ: Type) -> ValueType {
    match typ {
        Type::Int => ValueType::I64,
        Type::Bool => ValueType::I32,
        Type::Str => panic!("Unhandled type: str"),
        Type::List(_x) => panic!("Unhandled type: list"),
        Type::Func(_typs, _ret_typ) => panic!("Unhandled type: func"),
        Type::Tuple(_typs) => panic!("Unhandled type: tuple"),
        Type::Record(_fields) => panic!("Unhandled type: record"),
        Type::Exists(_bound_var, _inner_typ) => panic!("Unhandled type: existential type"),
        Type::TypeVar(_x) => panic!("Unhandled type: type var"),
        Type::Unknown => panic!("Unhandled type: unknown"),
    }
}

fn gen_instr_binop(
    op: BinOp,
    arg1: &Expr,
    arg2: &Expr,
    locals: &mut LocalsMap,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    match op {
        BinOp::Add => {
            let arg1_instr = gen_instr(arg1, locals)?;
            let arg2_instr = gen_instr(arg2, locals)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64Add]].concat())
        }
        BinOp::Subtract => {
            let arg1_instr = gen_instr(arg1, locals)?;
            let arg2_instr = gen_instr(arg2, locals)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64Sub]].concat())
        }
        BinOp::Multiply => {
            let arg1_instr = gen_instr(arg1, locals)?;
            let arg2_instr = gen_instr(arg2, locals)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64Mul]].concat())
        }
        BinOp::Divide => {
            let arg1_instr = gen_instr(arg1, locals)?;
            let arg2_instr = gen_instr(arg2, locals)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64DivS]].concat())
        }
        BinOp::LessThan => {
            let arg1_instr = gen_instr(arg1, locals)?;
            let arg2_instr = gen_instr(arg2, locals)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64LtS]].concat())
        }
        BinOp::GreaterThan => {
            let arg1_instr = gen_instr(arg1, locals)?;
            let arg2_instr = gen_instr(arg2, locals)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64GtS]].concat())
        }
        BinOp::LessOrEqual => {
            let arg1_instr = gen_instr(arg1, locals)?;
            let arg2_instr = gen_instr(arg2, locals)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64LeS]].concat())
        }
        BinOp::GreaterOrEqual => {
            let arg1_instr = gen_instr(arg1, locals)?;
            let arg2_instr = gen_instr(arg2, locals)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64GeS]].concat())
        }
        BinOp::EqualTo => {
            let arg1_instr = gen_instr(arg1, locals)?;
            let arg2_instr = gen_instr(arg2, locals)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64Eq]].concat())
        }
        BinOp::And => {
            let arg1_instr = gen_instr(arg1, locals)?;
            let arg2_instr = gen_instr(arg2, locals)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64And]].concat())
        }
        BinOp::Or => {
            let arg1_instr = gen_instr(arg1, locals)?;
            let arg2_instr = gen_instr(arg2, locals)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64Or]].concat())
        }
        BinOp::Concat => Err(CodeGenerateError::from("Unhandled binop: concat.")),
    }
}

fn gen_instr_if(
    pred: &Expr,
    cons: &Expr,
    alt: &Expr,
    locals: &mut LocalsMap,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let pred_instr = gen_instr(pred, locals)?;
    let cons_instr = gen_instr(cons, locals)?;
    let alt_instr = gen_instr(alt, locals)?;

    let cons_type = cons
        .checked_type
        .clone()
        .ok_or_else(|| CodeGenerateError::from("Expression does not have type annotation."))?;

    let block_type = BlockType::Value(convert_to_wasm_type(cons_type));
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

fn gen_instr_let(
    bindings: &Vector<(String, Expr)>,
    body: &Expr,
    locals: &mut LocalsMap,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let mut let_instr: Vec<Instruction> = vec![];
    for pair in bindings {
        let local_index = locals.len() as u32;
        let exp_type =
            pair.1.checked_type.clone().ok_or_else(|| {
                CodeGenerateError::from("Expression does not have type annotation.")
            })?;
        let mut exp_instr = gen_instr(&pair.1, locals)?;
        let wasm_type = convert_to_wasm_type(exp_type);
        locals.insert(pair.0.clone(), (local_index, wasm_type));
        let_instr.append(&mut exp_instr);
        let_instr.push(Instruction::SetLocal(local_index));
    }
    // let body_type = body
    //     .checked_type
    //     .clone()
    //     .ok_or_else(|| CodeGenerateError::from("Body does not have type annotation."))?;
    // let body_type_wasm = convert_to_wasm_type(body_type);
    let body_instr = gen_instr(body, locals)?;

    // let block_type = BlockType::Value(body_type_wasm);
    Ok([let_instr, body_instr].concat())
}

pub fn gen_instr(
    exp: &Expr,
    locals: &mut LocalsMap,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let instructions: Result<Vec<Instruction>, CodeGenerateError> = match &*exp.kind {
        ExprKind::Num(x) => Ok(vec![Instruction::I64Const(*x)]),
        ExprKind::Bool(x) => Ok(vec![Instruction::I64Const(*x as i64)]),
        // ExprKind::Str(_) => Ok(Type::Str),
        ExprKind::Id(sym) => {
            let local_idx = locals
                .get(sym)
                .ok_or_else(|| CodeGenerateError::from("Symbol not found in LocalsMap."))?;
            Ok(vec![Instruction::GetLocal(local_idx.0)])
        }
        ExprKind::Binop(op, arg1, arg2) => Ok(gen_instr_binop(*op, &arg1, &arg2, locals)?),
        ExprKind::If(pred, cons, alt) => Ok(gen_instr_if(&pred, &cons, &alt, locals)?),
        ExprKind::Let(bindings, body) => Ok(gen_instr_let(&bindings, &body, locals)?),
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

pub fn construct_module(
    name: &str,
    locals: LocalsMap,
    param_types: Vec<Type>,
    ret_type: Type,
    mut instructions: Instructions,
) -> Module {
    let wasm_ret_type = convert_to_wasm_type(ret_type);

    let wasm_param_types = param_types
        .iter()
        .map(|typ| convert_to_wasm_type(typ.clone()))
        .collect::<Vec<ValueType>>();

    let mut wasm_locals = locals
        .iter()
        .map(|(_sym, idx_typ_pair)| *idx_typ_pair)
        .collect::<Vec<(u32, ValueType)>>();
    wasm_locals.sort_unstable_by(|a, b| a.0.cmp(&b.0));
    let wasm_locals = wasm_locals
        .iter()
        .map(|(_index, typ)| Local::new(1, *typ))
        .collect::<Vec<Local>>();
    instructions.elements_mut().push(Instruction::End);

    builder::module()
        .function()
        .signature()
        .with_params(wasm_param_types)
        .with_return_type(Some(wasm_ret_type))
        .build()
        .body()
        .with_locals(wasm_locals)
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
