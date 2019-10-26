use proc_macro2::TokenStream;
use scheme_to_rust::compile::compile;
use std::{env, fs, process::Command};

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

// TODO: fix so this test cleans itself up if it fails
// (probably requires finding some crate to do this?)
fn run_code(input: TokenStream) -> String {
    let data = boilerplate.replace("<EXP>", format!("{}", input).as_str());
    let curr_dir = env::current_dir().expect("Could not get current directory.");
    let tmp_dir = curr_dir.join("tests").join("tmp");
    let tmp_file = tmp_dir.join("foo.rs");
    // let data = String::from(r#"fn main() { print!("3"); }"#);
    fs::create_dir(tmp_dir.clone()).expect("Unable to create tmp directory.");
    fs::write(tmp_file.clone(), data).expect("Unable to write file.");
    // let cleanup = Command::new("rustfmt")
    let prog = Command::new("rustc")
        .arg(tmp_file.clone())
        .arg("--out-dir")
        .arg(tmp_dir.clone())
        .output()
        .expect("Unable to compile rust code.");
    println!(
        "compile stderr: {}",
        String::from_utf8(prog.stderr).expect("no stderr")
    );
    println!(
        "compile stderr: {}",
        String::from_utf8(prog.stdout).expect("no stdout")
    );
    let output = Command::new("./tests/tmp/foo")
        .output()
        .expect("Failed to compile rust code.");
    fs::remove_dir_all(tmp_dir.clone()).expect("Unable to clean up tmp directory.");
    String::from_utf8(output.stdout).expect("Unable to read output.")
}

#[test]
fn test_compile_happy() {
    let exp = lexpr::from_str("3").unwrap();
    let code = compile(exp);
    println!("{}", code);
    let mut result = run_code(code);
    result.pop(); // remove newline
    assert_eq!(result, String::from("3"));
}
