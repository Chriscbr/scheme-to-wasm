use proc_macro2::TokenStream;
use scheme_to_rust::compile::compile;
use std::{env, fs, process::Command};

// TODO: implement
fn run_code(input: TokenStream) -> String {
    let data = format!("{}", input);
    let curr_dir = env::current_dir().expect("Could not get current directory.");
    let tmp_dir = curr_dir.join("tests").join("tmp");
    let tmp_file = tmp_dir.join("foo.rs");
    let data = String::from(r#"fn main() { print!("3"); }"#);
    fs::create_dir(tmp_dir.clone()).expect("Unable to create tmp directory.");
    fs::write(tmp_file.clone(), data).expect("Unable to write file.");
    let compile_prog = Command::new("rustc")
        .arg(tmp_file.clone())
        .arg("--out-dir")
        .arg(tmp_dir.clone())
        .output()
        .expect("Unable to compile rust code.");
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
    let result = run_code(code);
    assert_eq!(result, String::from("3"));
}
