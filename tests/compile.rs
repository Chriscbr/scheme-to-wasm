use scheme_to_rust::compile::compile;
use std::{env, fs, process::Command};

fn run_code(input: String) -> String {
    let curr_dir = env::current_dir().expect("Could not get current directory.");
    let tmp_dir = curr_dir.join("tests").join("tmp");
    let tmp_file = tmp_dir.join("foo.rs");
    fs::create_dir(tmp_dir.clone())
        .or_else(|_err| {
            fs::remove_dir_all(tmp_dir.clone()).expect("Unable to remove temp directory");
            fs::create_dir(tmp_dir.clone())
        })
        .expect("Unable to set up temp directory.");
    fs::write(tmp_file.clone(), input).expect("Unable to write file.");
    let _cleanup = Command::new("rustfmt")
        .arg(tmp_file.clone())
        .output()
        .expect("Unable to format rust code.");
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
    result.pop(); // remove newline from program output
    assert_eq!(result, String::from("3"));
}
