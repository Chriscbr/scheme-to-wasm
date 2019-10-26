use crate::closure_convert::closure_convert;
use crate::generate_code::generate_code;
use crate::lambda_lift::lambda_lift;
use crate::parse::parse;
use crate::type_check::{type_check, type_check_prog};

static boilerplate: &str = r#"

#[derive(Clone)]
pub enum Val {
    Int(i64),
}

impl std::fmt::Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Val::Int(x) => write!(f, "{}", *x),
        }
    }
}

impl Val {
    pub fn stringify(&self, heap: &Heap) -> String {
        match self {
            Val::Int(x) => format!("{}", *x),
        }
    }

    pub fn unwrap_int(&self) -> i64 {
        match self {
            Val::Int(x) => *x,
            _ => panic!("called `Val::unwrap_int` on something besides Val::Int")
        }
    }
}

#[derive(Default)]
pub struct Heap {
    mem: Vec<Val>
}

impl From<Vec<Val>> for Heap {
    fn from(mem: Vec<Val>) -> Self {
        Heap { mem: mem }
    }
}

impl Heap {
    pub fn new() -> Self {
        Heap { mem: vec![] }
    }

    pub fn alloc(&mut self, v: Val) -> usize {
        self.mem.push(v);
        self.mem.len() - 1
    }

    pub fn get(&self, i: usize) -> &Val {
        &self.mem[i]
    }

    pub fn set(&mut self, i: usize, v: Val) {
        self.mem[i] = v;
    }

    pub fn copy(&mut self, into: usize, from: usize) {
        self.mem[into] = self.mem[from].clone();
    }

    pub fn next_alloc_addr(&self) -> usize {
        self.mem.len()
    }
}

fn run(h: &mut Heap) -> usize {
    <EXP>
}

fn main() {
    let mut h = Heap::new();
    let result = run(&mut h);
    println!("{}", h.get(result).stringify(&h));
}
"#;

// TODO: rework to produce some kind of fallback if any step fails,
// e.g. as done in
// https://docs.rs/lexpr-macros/0.2.0/src/lexpr_macros/lib.rs.html
pub fn compile(input: lexpr::Value) -> String {
    let exp = parse(&input).unwrap();
    type_check(&exp).unwrap();
    let cc_exp = closure_convert(&exp).unwrap();
    type_check(&cc_exp).unwrap();
    let prog = lambda_lift(&cc_exp).unwrap();
    type_check_prog(&prog).unwrap();
    let code = generate_code(&prog).unwrap();
    boilerplate.replace("<EXP>", format!("{}", code).as_str())
}
