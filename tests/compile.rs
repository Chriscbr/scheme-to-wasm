use proc_macro2::TokenStream;
use scheme_to_rust::compile::compile;

// TODO: implement
fn run_code(input: TokenStream) -> String {
    String::from("3")
}

#[test]
fn test_compile_simple_happy() {
    let exp = lexpr::from_str("3").unwrap();
    let code = compile(exp);
    println!("{}", code);
    let result = run_code(code);
    assert_eq!(result, String::from("3"));
}
