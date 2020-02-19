// Just experimenting with WASM

// Some useful examples at
// https://github.com/paritytech/parity-wasm/tree/master/examples

use crate::common::{BinOp, Expr, ExprKind};
use crate::types::Type;

use std::cmp::Ordering;
use std::collections::BTreeMap;

use im_rc::Vector;
use parity_wasm::builder;
use parity_wasm::elements::{BlockType, Instruction, Instructions, Local, Module, ValueType};

/// A key-value map for finding the WebAssembly type and index associated
/// with a local variable in the WebAssembly.
///
/// Ex. when compiling `(let ((a 3)) (+ a 3))`, the value of `a` will be
/// stored in a local variable within the WebAssembly function, and the index
/// needs to be tracked so that it can be provided to the WebAssembly
/// local.get function when `a` is referenced within the body.
type LocalsMap = BTreeMap<String, (u32, ValueType)>;

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

#[derive(Default)]
pub struct CodeGenerateState {
    locals: LocalsMap,
    mem_index: u32,
}

impl CodeGenerateState {
    pub fn new() -> Self {
        CodeGenerateState {
            locals: LocalsMap::new(),
            mem_index: 0,
        }
    }
}

/// Provide the appropriate representation of each type in WebAssembly.
///
/// This primarily pertains to how what type it needs to be if it is stored in
/// a local variable, or is provided in a parameter, etc. Some types are stored
/// directly (e.g. integers, booleans), while others are just stored as 32-bit
/// pointers (e.g. strings, records) to a place in the WebAssembly memory.
impl From<Type> for ValueType {
    fn from(typ: Type) -> Self {
        match typ {
            Type::Int => ValueType::I64,
            Type::Bool => ValueType::I32,
            Type::Str => panic!("Unhandled type: str"),
            Type::List(_x) => panic!("Unhandled type: list"),
            Type::Func(_typs, _ret_typ) => panic!("Unhandled type: func"),
            Type::Tuple(_typs) => ValueType::I32,
            Type::Record(_fields) => panic!("Unhandled type: record"),
            Type::Exists(_bound_var, _inner_typ) => panic!("Unhandled type: existential type"),
            Type::TypeVar(_x) => panic!("Unhandled type: type var"),
            Type::Unknown => panic!("Unhandled type: unknown"),
        }
    }
}

/// Calculate the number of bytes needed to store a value of the particular
/// type in the WebAssembly's linear memory.
fn wasm_size_of(typ: &Type) -> u32 {
    match typ {
        Type::Int => 8,
        Type::Bool => 4,
        Type::Str => panic!("Unhandled type: str"),
        Type::List(_x) => panic!("Unhandled type: list"),
        Type::Func(_typs, _ret_typ) => panic!("Unhandled type: func"),
        Type::Tuple(types) => types.iter().map(|typ| wasm_size_of(typ)).sum(),
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
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    match op {
        BinOp::Add => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64Add]].concat())
        }
        BinOp::Subtract => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64Sub]].concat())
        }
        BinOp::Multiply => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64Mul]].concat())
        }
        BinOp::Divide => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64DivS]].concat())
        }
        BinOp::LessThan => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64LtS]].concat())
        }
        BinOp::GreaterThan => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64GtS]].concat())
        }
        BinOp::LessOrEqual => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64LeS]].concat())
        }
        BinOp::GreaterOrEqual => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64GeS]].concat())
        }
        BinOp::EqualTo => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64Eq]].concat())
        }
        BinOp::And => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64And]].concat())
        }
        BinOp::Or => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I64Or]].concat())
        }
        BinOp::Concat => Err(CodeGenerateError::from("Unhandled binop: concat.")),
    }
}

fn gen_instr_if(
    pred: &Expr,
    cons: &Expr,
    alt: &Expr,
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let pred_instr = gen_instr(pred, state)?;
    let cons_instr = gen_instr(cons, state)?;
    let alt_instr = gen_instr(alt, state)?;

    let cons_type = cons
        .checked_type
        .clone()
        .ok_or_else(|| CodeGenerateError::from("If consequent does not have type annotation."))?;

    let block_type = BlockType::Value(cons_type.into());
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
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let mut let_instr: Vec<Instruction> = vec![];
    for pair in bindings {
        let local_index = state.locals.len() as u32;
        let exp_type =
            pair.1.checked_type.clone().ok_or_else(|| {
                CodeGenerateError::from("Let binding does not have type annotation.")
            })?;
        let mut exp_instr = gen_instr(&pair.1, state)?;
        state
            .locals
            .insert(pair.0.clone(), (local_index, exp_type.into()));
        let_instr.append(&mut exp_instr);
        let_instr.push(Instruction::SetLocal(local_index));
    }
    let body_instr = gen_instr(body, state)?;

    Ok([let_instr, body_instr].concat())
}

/// Generate instructions for a tuple expression.
///
/// The general idea is to insert each part of the tuple into linear memory,
/// so that they are all in order (and hence can be easily retrieved). The
/// index for the first piece is left on the top of the stack.
fn gen_instr_tuple(
    exprs: &Vector<Expr>,
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let mut tuple_instr: Vec<Instruction> = vec![];
    let tuple_head_idx = state.mem_index as i32;
    for exp in exprs {
        let mut exp_instr = gen_instr(exp, state)?;
        let exp_type = exp.checked_type.clone().ok_or_else(|| {
            CodeGenerateError::from("Tuple component does not have type annotation.")
        })?;
        let part_size = wasm_size_of(&exp_type);
        let idx = state.mem_index as i32;
        state.mem_index += part_size;
        tuple_instr.push(Instruction::I32Const(idx));
        tuple_instr.append(&mut exp_instr);
        match exp_type.into() {
            ValueType::I32 => tuple_instr.push(Instruction::I32Store(0, 0)),
            ValueType::I64 => tuple_instr.push(Instruction::I64Store(0, 0)),
            _ => return Err(CodeGenerateError::from("Unhandled wasm type.")),
        }
    }
    tuple_instr.push(Instruction::I32Const(tuple_head_idx));
    Ok(tuple_instr)
}

fn gen_instr_tuple_get(
    tuple: &Expr,
    key: u64,
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let tuple_instr = gen_instr(tuple, state)?;
    let tuple_type = tuple
        .checked_type
        .clone()
        .ok_or_else(|| CodeGenerateError::from("Tuple does not have type annotation."))?;
    let mut tuple_get_instr: Vec<Instruction> = vec![];
    let mut curr_key: u64 = 0;
    let mut curr_mem_offset: u32 = 0;
    match tuple_type {
        Type::Tuple(inner_types) => {
            for typ in inner_types {
                // TODO: change key to be of type u32?
                match curr_key.cmp(&key) {
                    Ordering::Equal => {
                        match typ.into() {
                            ValueType::I32 => {
                                tuple_get_instr.push(Instruction::I32Load(0, curr_mem_offset))
                            }
                            ValueType::I64 => {
                                tuple_get_instr.push(Instruction::I64Load(0, curr_mem_offset))
                            }
                            _ => return Err(CodeGenerateError::from("Unhandled wasm type.")),
                        };
                        break;
                    }
                    Ordering::Greater => {
                        return Err(CodeGenerateError::from("Memory alignment error."))
                    }
                    Ordering::Less => {
                        let type_size = wasm_size_of(&typ);
                        curr_mem_offset += type_size;
                        curr_key += 1;
                    }
                }
            }
        }
        _ => {
            return Err(CodeGenerateError::from(
                "get_instr_tuple_get called with non-tuple expression.",
            ))
        }
    }
    Ok([tuple_instr, tuple_get_instr].concat())
}

pub fn gen_instr(
    exp: &Expr,
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let instructions: Result<Vec<Instruction>, CodeGenerateError> = match &*exp.kind {
        ExprKind::Num(x) => Ok(vec![Instruction::I64Const(*x)]),
        ExprKind::Bool(x) => Ok(vec![Instruction::I64Const(*x as i64)]),
        ExprKind::Str(_) => panic!("Unhandled gen_instr case: Str"),
        ExprKind::Id(sym) => {
            let local_idx = state
                .locals
                .get(sym)
                .ok_or_else(|| CodeGenerateError::from("Symbol not found in LocalsMap."))?;
            Ok(vec![Instruction::GetLocal(local_idx.0)])
        }
        ExprKind::Binop(op, arg1, arg2) => Ok(gen_instr_binop(*op, &arg1, &arg2, state)?),
        ExprKind::If(pred, cons, alt) => Ok(gen_instr_if(&pred, &cons, &alt, state)?),
        ExprKind::Let(bindings, body) => Ok(gen_instr_let(&bindings, &body, state)?),
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
        ExprKind::Tuple(exps) => Ok(gen_instr_tuple(&exps, state)?),
        ExprKind::TupleGet(tup, key) => Ok(gen_instr_tuple_get(&tup, *key, state)?),
        // ExprKind::Pack(val, sub, exist) => tc_pack_with_env(&val, &sub, &exist, env),
        // ExprKind::Unpack(var, package, typ_sub, body) => {
        //     tc_unpack_with_env(&var, &package, *typ_sub, &body, env)
        // }
        // ExprKind::FnApp(func, args) => tc_apply_with_env(&func, &args, env),
        _ => Err(CodeGenerateError::from("Unhandled expression kind.")),
    };
    Ok(instructions?)
}

/// Construct a WebAssembly module.
///
/// We assume that the `Instructions` argument passed in does not contain
/// a closing `Instruction::End` instruction. (can be changed if needed)
pub fn construct_module(
    name: &str,
    state: CodeGenerateState,
    param_types: Vec<Type>,
    ret_type: Type,
    mut instructions: Instructions,
) -> Module {
    // Construct the list of WebAssembly parameter types
    let wasm_param_types = param_types
        .iter()
        .map(|typ| typ.clone().into())
        .collect::<Vec<ValueType>>();

    // Construct the list of local variables in order from state.locals,
    // which is internally a BTreeMap. First, the map is iterated over to
    // build a vector.
    let mut wasm_locals = state
        .locals
        .iter()
        .map(|(_sym, idx_typ_pair)| *idx_typ_pair) // iter over (key, value) in BTreeMap
        .collect::<Vec<(u32, ValueType)>>();
    // Sort the entires by index (since the iterator's order is not guaranteed)
    wasm_locals.sort_unstable_by(|a, b| a.0.cmp(&b.0));
    // Finally, drop the indices, and construct locals using the type information
    let wasm_locals = wasm_locals
        .iter()
        .map(|(_index, typ)| Local::new(1, *typ))
        .collect::<Vec<Local>>();

    // Add the required end instruction
    instructions.elements_mut().push(Instruction::End);

    builder::module()
        .memory()
        .with_min(32)
        .with_max(None)
        .build()
        .function()
        .signature()
        .with_params(wasm_param_types)
        .with_return_type(Some(ret_type.into()))
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
