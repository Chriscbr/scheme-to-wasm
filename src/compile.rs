use crate::closure_convert::closure_convert;
use crate::generate_code::generate_code_prog;
use crate::lambda_lift::lambda_lift;
use crate::parse::parse;
use crate::type_check::{type_check, type_check_prog};
use std::error::Error;
use std::io;

static BOILERPLATE: &str = r#"
fn run(h: &mut Heap) -> impl HeapVal {
    <EXP>
}

fn main() {
    let mut h = Heap::new();
    let result = run(&mut h);
    println!("{}", result);
}
"#;

fn get_library_code() -> io::Result<String> {
    let curr_dir = std::env::current_dir()?;
    let lib_path = curr_dir.join("boilerplate").join("src").join("lib.rs");
    std::fs::read_to_string(lib_path)
}

pub fn compile(input: lexpr::Value) -> Result<String, Box<dyn Error>> {
    let exp = parse(&input)?;
    type_check(&exp)?;
    let cc_exp = closure_convert(&exp)?;
    println!("{}", cc_exp.clone());
    type_check(&cc_exp)?;
    let prog = lambda_lift(&cc_exp)?;
    println!("{}", prog.clone());
    type_check_prog(&prog)?;
    let code = generate_code_prog(&prog)?;
    let lib_code = get_library_code()?;
    let main_code = BOILERPLATE.replace("<EXP>", format!("{}", code).as_str());
    Ok(format!("{}\n{}", lib_code, main_code))
}
