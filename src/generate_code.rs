use crate::common::{BinOp, ExprKind, Prog, TypedExpr};
use crate::types::Type;

use std::cmp;
use std::cmp::Ordering;
use std::collections::BTreeMap;

use im_rc::Vector;
use parity_wasm::builder;
use parity_wasm::elements::{BlockType, Instruction, Instructions, Local, Module, ValueType};

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

/// A key-value map for finding the local variable index associated with
/// a local variable.
///
/// E.g. when compiling `(let ((a 3)) (+ a 3))`, the value of `a` will be
/// stored in a local variable within the WebAssembly function, and the index
/// needs to be tracked so that it can be provided to the WebAssembly
/// local.get function when `a` is referenced within the body.
type LocalsMap = BTreeMap<String, u32>;

/// A key-value map for finding the index of a WebAssembly function instance
/// which is associated with a particular identifier (string).
type FuncsMap = BTreeMap<String, u32>;

/// Maintains metadata used by code-generating functions.
///
/// The code-generating functions (gen_instr_*) recursively call each other,
/// so this struct needs to be mutable (even if it's just via cloning, etc.)
///
/// This struct store information such as:
/// a) the local variables that are within scope of the expression being
///    compiled
/// b) the first free index within WebAssembly's linear memory safe to allocate
///    new data (tuples, records, etc.) to
#[derive(Default)]
pub struct CodeGenerateState {
    locals: LocalsMap,
    funcs: FuncsMap,
    mem_index: u32,
}

impl CodeGenerateState {
    pub fn new() -> Self {
        CodeGenerateState {
            locals: LocalsMap::new(),
            funcs: FuncsMap::new(),
            mem_index: 0,
        }
    }
}

/// Calculate the number of bytes needed to store a value of the particular
/// type in the WebAssembly's linear memory.
///
/// The full size of the data structure might be large, but the value returned
/// by this function in particular is concerned with how large are the elements
/// if you were to pack them into a tuple. All of the values of a tuple are
/// stored adjacently in linear memory, so that size must necessarily at least
/// be the size of the combined inner types. But for all other complex types
/// (such as lists), we can just put a pointer to them in the tuple we are
/// constructing - not the entire data structure.
fn wasm_size_of(typ: &Type) -> u32 {
    match typ {
        Type::Int => 4,
        Type::Bool => 4,
        Type::Str => panic!("Unhandled type: str"),
        Type::List(_inner_typ) => 4,
        Type::Func(_typs, _ret_typ) => 4,
        // Ensure that the size of a tuple is at least 4,
        // to handle the case of empty tuples.
        Type::Tuple(types) => cmp::max(4, types.iter().map(|typ| wasm_size_of(typ)).sum()),
        Type::Record(_fields) => panic!("Unhandled type: record"),
        Type::Exists(_bound_var, inner_typ) => wasm_size_of(inner_typ),
        Type::TypeVar(_x) => 4, // ??? not sure what to put here
        Type::Unknown => panic!("Unhandled type: unknown"),
    }
}

fn gen_instr_binop(
    op: BinOp,
    arg1: &TypedExpr,
    arg2: &TypedExpr,
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    match op {
        BinOp::Add => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I32Add]].concat())
        }
        BinOp::Subtract => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I32Sub]].concat())
        }
        BinOp::Multiply => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I32Mul]].concat())
        }
        BinOp::Divide => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I32DivS]].concat())
        }
        BinOp::LessThan => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I32LtS]].concat())
        }
        BinOp::GreaterThan => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I32GtS]].concat())
        }
        BinOp::LessOrEqual => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I32LeS]].concat())
        }
        BinOp::GreaterOrEqual => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I32GeS]].concat())
        }
        BinOp::EqualTo => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I32Eq]].concat())
        }
        BinOp::And => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I32And]].concat())
        }
        BinOp::Or => {
            let arg1_instr = gen_instr(arg1, state)?;
            let arg2_instr = gen_instr(arg2, state)?;
            Ok([arg1_instr, arg2_instr, vec![Instruction::I32Or]].concat())
        }
        BinOp::Concat => Err(CodeGenerateError::from("Unhandled binop: concat.")),
    }
}

fn gen_instr_if(
    pred: &TypedExpr,
    cons: &TypedExpr,
    alt: &TypedExpr,
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let pred_instr = gen_instr(pred, state)?;
    let cons_instr = gen_instr(cons, state)?;
    let alt_instr = gen_instr(alt, state)?;

    let block_type = BlockType::Value(ValueType::I32);
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

/// Generate instructions for a let expression.
fn gen_instr_let(
    bindings: &Vector<(String, TypedExpr)>,
    body: &TypedExpr,
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let mut let_instr: Vec<Instruction> = vec![];
    for pair in bindings {
        let local_index = state.locals.len() as u32;
        let mut exp_instr = gen_instr(&pair.1, state)?;
        state.locals.insert(pair.0.clone(), local_index);
        let_instr.append(&mut exp_instr);
        let_instr.push(Instruction::SetLocal(local_index));
    }
    let body_instr = gen_instr(body, state)?;

    Ok([let_instr, body_instr].concat())
}

/// Generate instructions for a begin expression.
fn gen_instr_begin(
    exps: &Vector<TypedExpr>,
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    if exps.is_empty() {
        return Err(CodeGenerateError::from(
            "Begin expression contains no subexpressions!",
        ));
    }
    let first_exps = exps.iter().take(exps.len() - 1);
    let last_exp = exps.last().unwrap();
    let mut begin_instr: Vec<Instruction> = vec![];
    for exp in first_exps {
        let mut exp_instr = gen_instr(exp, state)?;
        begin_instr.append(&mut exp_instr);
        begin_instr.push(Instruction::Drop);
    }
    let mut last_exp_instr = gen_instr(&last_exp, state)?;
    begin_instr.append(&mut last_exp_instr);
    Ok(begin_instr)
}

/// Generate instructions for a set! expression.
fn gen_instr_set(
    sym: &str,
    exp: &TypedExpr,
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let local_idx = *(state
        .locals
        .get(sym)
        .ok_or_else(|| "Symbol not found within local scope.")?);
    let mut set_instr = gen_instr(exp, state)?;
    set_instr.push(Instruction::TeeLocal(local_idx));
    Ok(set_instr)
}

/// Generate instructions for a make-tuple expression.
///
/// The general idea is to insert each part of the tuple into linear memory,
/// so that they are all in order (and hence can be easily retrieved). The
/// the pointer for the head of the tuple is left on the top of the stack.
///
/// In the example below, a tuple with three parts (A, B, C) is created, where
/// A and B are two arbitrary values that require memory allocation, and C is
/// just a number. The instructions for calculating A, B, and C are generated,
/// which include several memory allocations. Then, the values left on the
/// stack (a pointer to A, a pointer to B, and the value of C) are stored in
/// linear memory (in order), with the index of the beginning (12 in the
/// diagram) left on top of the stack.
///
/// Memory:
/// +---+---+---+---+---+---+
/// | A | B     | 0 | 4 | C |
/// +---+---+---+---+---+---+
/// 0   4   8   12  16  20  24
/// Stack:
/// [ 12 ]
///
/// Note: In our current implementation, we make empty tuples of size "4", so
/// all tuples take up at least 4 bytes in the heap. This ensures we don't
/// need complex edge-case handling for handling tuple-ref's and other
/// expressions. Hence, when an empty tuple is constructed, it will allocate
/// a value in the heap, but it's value will be meaningless.
/// But we are assuming through type-safety that nothing improper will happen
/// that would try to access and use this value stored in memory (since all
/// possible tuple-ref's will not type-check on an empty tuple).
fn gen_instr_tuple(
    exprs: &Vector<TypedExpr>,
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let mut tuple_instr: Vec<Instruction> = vec![];

    // First, calculate and construct all of the components of the tuple (making
    // any calculations and memory allocations as necessary). This will leave n
    // values on top of the stack, where n is the tuple size. Each value will
    // be either a primitive (if it was a boolean or number), or a pointer
    // (represented as an I32.const).
    //
    // While doing so, we keep track of the types of each value so that we
    // can allocate the parts of the tuple in reverse order (since the last
    // component of the tuple will be on the top of the stack, etc.).
    let mut tuple_part_wasm_types: Vec<ValueType> = vec![];
    let mut tuple_wasm_size: u32 = 0;

    for exp in exprs {
        // This extra instruction is needed so that after calculating the
        // first component of the tuple, the stack will have I32.const 0 and
        // the component value on the stack. Both arguments are needed
        // for the value to be stored into linear memory (with I32Store or
        // I64Store).
        tuple_instr.push(Instruction::I32Const(0));

        let mut exp_instr = gen_instr(exp, state)?;
        tuple_wasm_size += wasm_size_of(&exp.typ);
        tuple_instr.append(&mut exp_instr);
        tuple_part_wasm_types.push(ValueType::I32);
    }

    // Allocate the stack values into linear memory by processing through them
    // in reverse order.
    //
    // We also allocate the values backwards in memory, so we start bringing
    // state.mem_index all the way forward, then iterate through backwards,
    // and them move it back all the way forward so none of the values
    // get overwritten.
    let tuple_head_idx = state.mem_index;
    state.mem_index += tuple_wasm_size;
    for _ in 0..tuple_part_wasm_types.len() {
        state.mem_index -= 4;
        tuple_instr.push(Instruction::I32Store(0, state.mem_index));
    }
    state.mem_index += tuple_wasm_size;

    // Finally, leave the index for the head of the tuple on top of the stack.
    tuple_instr.push(Instruction::I32Const(tuple_head_idx as i32));
    Ok(tuple_instr)
}

/// Generate instructions for a tuple-ref expression.
///
/// The general strategy (based on the way tuples are constructed) is to
/// calculate the wasm sizes of the tuple's components, and then iterate
/// through the tuple's components until we are at the correct position (based
/// on the key provided as a second argument to tuple-ref), and then load
/// the value from memory.
///
/// TODO: I think this process can be rewritten in a functional/non-iterative
/// manner. Try calculating curr_mem_offset by summing the first (key - 1)
/// sizes of the tuple's types converted to wasm types.
fn gen_instr_tuple_get(
    tuple: &TypedExpr,
    key: u32,
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let tuple_instr = gen_instr(tuple, state)?;
    let mut tuple_get_instr: Vec<Instruction> = vec![];
    let mut curr_key: u32 = 0;
    let mut curr_mem_offset: u32 = 0;
    match &tuple.typ {
        Type::Tuple(inner_types) => {
            for typ in inner_types {
                match curr_key.cmp(&key) {
                    Ordering::Equal => {
                        tuple_get_instr.push(Instruction::I32Load(0, curr_mem_offset));
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

/// Generate instructions for a cons expression.
///
/// A List expression is stored as simply a pair of values: a car (sometimes
/// a pointer), and a cdr (always a pointer).
///
/// The overall strategy is to first generate the instructions for the car and
/// cdr of of the expression (leaving two values, most likely pointers, on the
/// stack), and then store the two stack values in memory, leaving an index to
/// the cons pair on the stack.
fn gen_instr_cons(
    first: &TypedExpr,
    rest: &TypedExpr,
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let mut cons_instr: Vec<Instruction> = vec![];

    // This extra instruction is needed so that after calculating the
    // car of the cons, the stack will have I32.const 0 and the car value
    // on the stack. Both arguments are needed for the value to be
    // stored into linear memory (with I32Store or I64Store).
    cons_instr.push(Instruction::I32Const(0));
    let mut first_instr = gen_instr(first, state)?;
    cons_instr.append(&mut first_instr);
    let first_wasm_type = ValueType::I32;

    // See comment above for why this instruction is needed.
    cons_instr.push(Instruction::I32Const(0));
    let mut rest_instr = gen_instr(rest, state)?;
    cons_instr.append(&mut rest_instr);

    let cons_idx = state.mem_index;
    match first_wasm_type {
        ValueType::I32 => {
            // car is I32, cdr is I32
            state.mem_index += 4;
            cons_instr.push(Instruction::I32Store(0, state.mem_index));
            state.mem_index -= 4;
            cons_instr.push(Instruction::I32Store(0, state.mem_index));
            state.mem_index += 8;
        }
        ValueType::I64 => {
            // car is I64, cdr is I32
            state.mem_index += 8;
            cons_instr.push(Instruction::I32Store(0, state.mem_index));
            state.mem_index -= 8;
            cons_instr.push(Instruction::I64Store(0, state.mem_index));
            state.mem_index += 12;
        }
        _ => return Err(CodeGenerateError::from("Unhandled wasm type.")),
    }
    cons_instr.push(Instruction::I32Const(cons_idx as i32));

    Ok(cons_instr)
}

/// Generate instructions for a car expression.
///
/// Evaluating the cons expression will leave a 32-bit pointer (the address of
/// the cons structure in WebAssembly linear memory). To obtain the first
/// component, we access the pointer contents with no offset.
fn gen_instr_car(
    cons: &TypedExpr,
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let mut car_instr = gen_instr(cons, state)?;
    car_instr.push(Instruction::I32Load(0, 0));
    Ok(car_instr)
}

/// Generate instructions for a cdr expression.
///
/// Evaluating the cons expression will leave a 32-bit pointer (the address of
/// the cons structure in WebAssembly linear memory). To obtain the second
/// component, we access the pointer contents with a 4-byte offset.
fn gen_instr_cdr(
    cons: &TypedExpr,
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let mut cdr_instr = gen_instr(cons, state)?;
    cdr_instr.push(Instruction::I32Load(0, 4));
    Ok(cdr_instr)
}

/// Generate instructions for a null expression.
///
/// A null expression doesn't really store any meaningful information (its
/// type is meaningful for type checking purposes), so we can just represent it
/// with a dummy value.
fn gen_instr_null(
    _typ: &Type,
    _state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    Ok(vec![Instruction::I32Const(-1)])
}

/// Generates instructions for a null? expression.
///
/// Our current implementation will allow non-List type expressions to appear
/// as arguments, but the result value will always be false (i.e. any
/// non-list types are automatically not null). We could allow for other
/// constructs such as the notion of falsey values (perhaps Num(0)) is null?,
/// but this distinction isn't particularly important (except perhaps for
/// the desire to respect type-theoretic guarantees about the language).
///
/// In addition, even if the inner expression is not a list type, we will
/// still calculate it in case some kind of useful / effect-ful computation
/// is being performed.
fn gen_instr_is_null(
    exp: &TypedExpr,
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let mut exp_instr = gen_instr(exp, state)?;
    match &exp.typ {
        Type::List(_inner_type) => {
            exp_instr.push(Instruction::I32Const(-1)); // all (null 'typ) expressions are represented as I32Const(-1)
            exp_instr.push(Instruction::I32Eq);
            Ok(exp_instr)
        }
        _ => {
            exp_instr.push(Instruction::Drop); // ignore the actual value of the inner expression
            exp_instr.push(Instruction::I32Const(0)); // return "false"
            Ok(exp_instr)
        }
    }
}

fn gen_instr_pack(
    val: &TypedExpr,
    _sub: &Type,
    _exist: &Type,
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    gen_instr(val, state)
}

fn gen_instr_unpack(
    var: &str,
    package: &TypedExpr,
    _type_sub: u64,
    body: &TypedExpr,
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let mut let_instr: Vec<Instruction> = vec![];
    let local_index = state.locals.len() as u32;
    let mut exp_instr = gen_instr(package, state)?;
    state.locals.insert(var.to_string(), local_index);
    let_instr.append(&mut exp_instr);
    let_instr.push(Instruction::SetLocal(local_index));
    let body_instr = gen_instr(body, state)?;

    Ok([let_instr, body_instr].concat())
}

fn gen_instr_fn_app(
    func: &TypedExpr,
    args: &Vector<TypedExpr>,
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let mut fn_app_instr: Vec<Instruction> = vec![];
    for exp in args {
        let mut exp_instr = gen_instr(exp, state)?;
        fn_app_instr.append(&mut exp_instr);
    }
    let mut func_idx_instr: Vec<Instruction> = gen_instr(func, state)?;
    fn_app_instr.append(&mut func_idx_instr);
    fn_app_instr.push(Instruction::CallIndirect(0, 0));
    Ok(fn_app_instr)
    // match &*func.kind {
    //     ExprKind::Id(name) => {
    //         let func_idx: u32 = match state.funcs.get(name) {
    //             Some(idx) => *idx,
    //             None => {
    //                 return Err(CodeGenerateError::from(
    //                     "Function name not found in functions maps!",
    //                 ))
    //             }
    //         };
    //         let mut fn_app_instr: Vec<Instruction> = vec![];
    //         for exp in args {
    //             let mut exp_instr = gen_instr(exp, state)?;
    //             fn_app_instr.append(&mut exp_instr);
    //         }
    //         fn_app_instr.push(Instruction::Call(func_idx));
    //         Ok(fn_app_instr)
    //     }
    //     _ => Err(CodeGenerateError::from(
    //         "Function application does not appear to be correctly lambda lifted!",
    //     )),
    // }
}

pub fn gen_instr(
    exp: &TypedExpr,
    state: &mut CodeGenerateState,
) -> Result<Vec<Instruction>, CodeGenerateError> {
    let instructions: Result<Vec<Instruction>, CodeGenerateError> = match &*exp.kind {
        ExprKind::Num(x) => Ok(vec![Instruction::I32Const(*x)]),
        ExprKind::Bool(x) => Ok(vec![Instruction::I32Const(*x as i32)]),
        ExprKind::Str(_) => panic!("Unhandled gen_instr case: Str"),
        ExprKind::Id(sym) => {
            println!("{:?}", state.locals);
            match state.locals.get(sym) {
                Some(local_idx) => Ok(vec![Instruction::GetLocal(*local_idx)]),
                None => match state.funcs.get(sym) {
                    Some(func_idx) => Ok(vec![Instruction::I32Const(*func_idx as i32)]),
                    None => {
                        return Err(CodeGenerateError::from(
                            "Symbol not found in locals or function table.",
                        ))
                    }
                },
            }
        }
        ExprKind::Binop(op, arg1, arg2) => Ok(gen_instr_binop(*op, &arg1, &arg2, state)?),
        ExprKind::If(pred, cons, alt) => Ok(gen_instr_if(&pred, &cons, &alt, state)?),
        ExprKind::Let(bindings, body) => Ok(gen_instr_let(&bindings, &body, state)?),
        ExprKind::Lambda(_params, _ret_type, _body) => Err(CodeGenerateError::from(
            "Lambda expressions should have been hoisted to the top level via lambda lifting pass.",
        )),
        ExprKind::Record(_bindings) => Err(CodeGenerateError::from(
            "Record expressions should be removed via record conversion pass.",
        )),
        ExprKind::RecordGet(_record, _key) => Err(CodeGenerateError::from(
            "Record expressions should be removed via record conversion pass.",
        )),
        ExprKind::Begin(exps) => Ok(gen_instr_begin(&exps, state)?),
        ExprKind::Set(sym, exp) => Ok(gen_instr_set(&sym, &exp, state)?),
        ExprKind::Cons(first, rest) => Ok(gen_instr_cons(&first, &rest, state)?),
        ExprKind::Car(cons) => Ok(gen_instr_car(&cons, state)?),
        ExprKind::Cdr(cons) => Ok(gen_instr_cdr(&cons, state)?),
        ExprKind::IsNull(exp) => Ok(gen_instr_is_null(&exp, state)?),
        ExprKind::Null(typ) => Ok(gen_instr_null(&typ, state)?),
        ExprKind::Tuple(exps) => Ok(gen_instr_tuple(&exps, state)?),
        ExprKind::TupleGet(tup, key) => Ok(gen_instr_tuple_get(&tup, *key, state)?),
        ExprKind::Pack(val, sub, exist) => Ok(gen_instr_pack(&val, &sub, &exist, state)?),
        ExprKind::Unpack(var, package, type_sub, body) => {
            Ok(gen_instr_unpack(&var, &package, *type_sub, &body, state)?)
        }
        ExprKind::FnApp(func, args) => Ok(gen_instr_fn_app(&func, &args, state)?),
    };
    Ok(instructions?)
}

/// Construct a WebAssembly module.
///
/// We assume that the `Instructions` argument passed in does not contain
/// a closing `Instruction::End` instruction.
pub fn construct_module(
    name: &str,
    state: CodeGenerateState,
    param_types: Vec<Type>,
    mut instructions: Instructions,
) -> builder::ModuleBuilder {
    // Construct the list of WebAssembly parameter types
    let wasm_param_types = std::iter::repeat(ValueType::I32)
        .take(param_types.len())
        .collect::<Vec<ValueType>>();

    // Construct the list of WebAssembly local types
    let wasm_locals = construct_locals(&state.locals);

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
        .with_return_type(Some(ValueType::I32))
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
}

pub fn construct_module_from_prog(prog: &Prog<TypedExpr>) -> Result<Module, CodeGenerateError> {
    let mut module_builder = builder::module()
        .memory()
        .with_min(32)
        .with_max(None)
        .build();
    let mut state = CodeGenerateState::new();

    // First, the lambda-lifted functions within `prog` will get compiled.
    // This is necessary for populating state.funcs, which maps the names of
    // functions to indices within the WebAssembly store. For reference, see:
    // https://webassembly.github.io/spec/core/exec/instructions.html#function-calls
    // https://webassembly.github.io/spec/core/exec/runtime.html#syntax-store
    prog.fns
        .iter()
        .for_each(|(name, lambda)| match &*lambda.kind {
            ExprKind::Lambda(params, _ret_type, body) => {
                let param_types = params
                    .iter()
                    .map(|(_name, typ)| typ.clone())
                    .collect::<Vec<Type>>();
                // Add the lambda's n parameters as the first n local variables
                params.iter().for_each(|(name, _typ)| {
                    let local_index = state.locals.len() as u32;
                    state.locals.insert(name.clone(), local_index);
                });

                let func_instructions = gen_instr(&body, &mut state).unwrap();
                let wasm_function = construct_function(
                    param_types,
                    Instructions::new(func_instructions),
                    &mut state,
                );

                // Update the `FuncsMap` table within `CodeGenerateState` so
                // that any time this function gets referred to by name later,
                // we know which index within WebAssembly's store we need to
                // use to call the function.

                // ex. the program has several functions. One of them is named
                // "foo", and is the third to get compiled so then
                // (key: "foo", value: 2) gets inserted to the table. Then
                // when compiling the body, when it (foo 5) is seen, the
                // code generate can look at state.funcs to see that foo
                // maps to 2, so we just need to put 5 on the stack and add
                // Instruction::Call(2) to perform the function application.
                let func_index = state.funcs.len() as u32;
                state.funcs.insert(name.to_string(), func_index);

                // Add the function to the module
                module_builder.push_function(wasm_function);

                // Reset state.locals so that the locals don't carry on
                // when compiling the next function...
                // Having to remember this kind of thing is a bit of a flaw
                // in the mutating-state-passing pattern we are using.
                state.locals.clear();
            }
            _ => panic!("Function inside prog.fns is not a lambda."),
        });

    // Construct a dummy table to make Instruction::CallIndirect work.
    let mut module_builder = module_builder.table().with_min(32).with_max(None);
    for i in 0..prog.fns.len() {
        module_builder = module_builder.with_element(i as u32, vec![i as u32]);
    }
    let module_builder = module_builder.build();

    // Finally, the body of the program is compiled. We will just give it a
    // fancy name like $$MAIN$$ and hope that nobody else uses it. :-)
    let mut main_instructions = gen_instr(&prog.exp, &mut state).unwrap();
    main_instructions.push(Instruction::End);
    let wasm_locals = construct_locals(&state.locals);
    let func_index = state.funcs.len() as u32;
    Ok(module_builder
        .function()
        .signature()
        .with_params(vec![])
        .with_return_type(Some(ValueType::I32))
        .build()
        .body()
        .with_locals(wasm_locals)
        .with_instructions(Instructions::new(main_instructions))
        .build()
        .build()
        .export()
        .field("$$MAIN$$")
        .internal()
        .func(func_index)
        .build()
        .build())
}

/// Construct a WebAssembly `FunctionDefinition`, a format for a function which
/// can be inserted easily into a WebAssembly `Module`.
///
/// We assume that the `Instructions` argument passed in does not contain
/// a closing `Instruction::End` instruction.
fn construct_function(
    param_types: Vec<Type>,
    mut instructions: Instructions,
    state: &mut CodeGenerateState,
) -> builder::FunctionDefinition {
    // Construct the list of WebAssembly parameter types
    let wasm_param_types = std::iter::repeat(ValueType::I32)
        .take(param_types.len())
        .collect::<Vec<ValueType>>();

    let wasm_locals = construct_locals(&state.locals);

    // Add the required end instruction
    instructions.elements_mut().push(Instruction::End);

    // Construction the `FunctionDefinition`
    builder::function()
        .signature()
        .with_params(wasm_param_types)
        .with_return_type(Some(ValueType::I32))
        .build()
        .body()
        .with_locals(wasm_locals)
        .with_instructions(instructions)
        .build()
        .build()
}

/// The `state` argument passed around by the code generation functions will
/// track any local variables that were needed during compilation (e.g. let
/// expressions will generate local variables). These need to be converted
/// into a format accepted by the `parity_wasm` library's `FunctionBuilder`
/// API.
fn construct_locals(locals: &LocalsMap) -> Vec<Local> {
    std::iter::repeat(Local::new(1, ValueType::I32))
        .take(locals.len())
        .collect::<Vec<Local>>()
}
