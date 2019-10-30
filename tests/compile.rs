use scheme_to_rust::compile::compile;
use serial_test_derive::serial;
use std::{env, error::Error, fs, process::Command};

#[derive(Clone, Debug)]
pub struct RuntimeError(String);

// Allows other errors to wrap this one
impl Error for RuntimeError {}

impl From<&str> for RuntimeError {
    fn from(message: &str) -> Self {
        RuntimeError(String::from(message))
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "RuntimeError: {}", self.0)
    }
}

/// Helper function for testing compiled code.
fn run_code(code: String) -> Result<String, Box<dyn Error>> {
    // Set up a temporary directory for putting the code and binary inside
    let curr_dir = env::current_dir()?;
    let tmp_dir = curr_dir.join("tests").join("tmp");
    let tmp_file = tmp_dir.join("foo.rs");
    fs::create_dir(tmp_dir.clone()).or_else(|_err| {
        fs::remove_dir_all(tmp_dir.clone())?;
        fs::create_dir(tmp_dir.clone())
    })?;
    fs::write(tmp_file.clone(), code)?;

    // Try to make the code more readable (for debugging)
    Command::new("rustfmt").arg(tmp_file.clone()).output()?;

    // Compile the code. In general, the standard error can have a lot of
    // warnings even if the code doesn't have problems, so we just print it
    // in case it is useful for debugging.
    let prog = Command::new("rustc")
        .arg(tmp_file.clone())
        .arg("--out-dir")
        .arg(tmp_dir.clone())
        .output()?;
    println!("compile stderr: {}", String::from_utf8(prog.stderr)?);
    println!("compile stderr: {}", String::from_utf8(prog.stdout)?);

    // Run the compiled code, save the output to a variable
    let output = Command::new("./tests/tmp/foo").output()?;

    // Clean up all of the files
    fs::remove_dir_all(tmp_dir.clone())?;

    // Extract the runtime output
    let mut result = String::from_utf8(output.stdout)?;
    result.pop(); // remove newline from end of output

    // We assume that if something was printed to the standard error pipe,
    // then the Rust code failed. Feel free to change this if this isn't
    // the case.
    if !output.stderr.is_empty() {
        Err(Box::from(RuntimeError::from(
            String::from_utf8(output.stderr)?.as_str(),
        )))
    } else {
        Ok(result)
    }
}

#[test]
#[serial]
fn test_compile_primitives() {
    let exp = lexpr::from_str("3").unwrap();
    let code = compile(exp).unwrap();
    let result = run_code(code).unwrap();
    assert_eq!(result, String::from("3"));

    let exp = lexpr::from_str("true").unwrap();
    let code = compile(exp).unwrap();
    let result = run_code(code).unwrap();
    assert_eq!(result, String::from("true"));

    let exp = lexpr::from_str("\"foo\"").unwrap();
    let code = compile(exp).unwrap();
    let result = run_code(code).unwrap();
    assert_eq!(result, String::from("\"foo\""));
}

#[test]
#[serial]
fn test_compile_binops() {
    let exp = lexpr::from_str("(+ (/ 8 2) (* (- 4 2) 3))").unwrap();
    let code = compile(exp).unwrap();
    let result = run_code(code).unwrap();
    assert_eq!(result, String::from("10"));

    let exp = lexpr::from_str(r#"(concat "foo" "bar")"#).unwrap();
    let code = compile(exp).unwrap();
    let result = run_code(code).unwrap();
    assert_eq!(result, String::from("\"foobar\""));

    let exp = lexpr::from_str("(and (or false true) true)").unwrap();
    let code = compile(exp).unwrap();
    let result = run_code(code).unwrap();
    assert_eq!(result, String::from("true"));
}

#[test]
#[serial]
fn test_compile_if() {
    // test consequent / short circuit behavior
    let exp = lexpr::from_str("(if (<= 3 5) 10 (/ 3 0))").unwrap();
    let code = compile(exp).unwrap();
    let result = run_code(code).unwrap();
    assert_eq!(result, String::from("10"));

    // test alternate behavior
    let exp = lexpr::from_str("(if (> 3 5) (/ 3 0) 11)").unwrap();
    let code = compile(exp).unwrap();
    let result = run_code(code).unwrap();
    assert_eq!(result, String::from("11"));
}

#[test]
#[serial]
fn test_compile_let() {
    let exp = lexpr::from_str("(let ((x 3)) (+ x 5))").unwrap();
    let code = compile(exp).unwrap();
    let result = run_code(code).unwrap();
    assert_eq!(result, String::from("8"));

    let exp = lexpr::from_str("(let ((x 3) (y 4)) (let ((z 5)) (+ x (+ y z))))").unwrap();
    let code = compile(exp).unwrap();
    let result = run_code(code).unwrap();
    assert_eq!(result, String::from("12"));
}

#[test]
#[serial]
fn test_compile_cons() {
    let exp = lexpr::from_str("(cons 3 (cons 4 (null int)))").unwrap();
    let code = compile(exp).unwrap();
    let result = run_code(code).unwrap();
    assert_eq!(result, String::from("(cons 3 (cons 4 (null T)))"));
}

#[test]
#[serial]
fn test_compile_div_by_zero() {
    let exp = lexpr::from_str("(/ 3 0)").unwrap();
    let code = compile(exp).unwrap();
    let result = run_code(code);
    assert_eq!(result.is_err(), true);
}
