use crate::closure_convert::closure_convert;
use crate::generate_code::generate_code_prog;
use crate::lambda_lift::lambda_lift;
use crate::parse::parse;
use crate::type_check::{type_check, type_check_prog};

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

fn get_library_code() -> String {
    let curr_dir = std::env::current_dir().expect("Could not get current directory.");
    let lib_path = curr_dir.join("boilerplate").join("src").join("lib.rs");
    std::fs::read_to_string(lib_path).expect("Unable to find library code.")
}

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
    let code = generate_code_prog(&prog).unwrap();
    let lib_code = get_library_code();
    let main_code = BOILERPLATE.replace("<EXP>", format!("{}", code).as_str());
    format!("{}\n{}", lib_code, main_code)
}
