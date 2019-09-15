use std::collections::LinkedList;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Str,
    List(Box<Type>),
    Func(Vec<Type>, Box<Type>), // array of input types, and a return type
}

#[derive(Default)]
pub struct Env {
    frames: LinkedList<Vec<(String, Type)>>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            frames: LinkedList::new(),
        }
    }

    pub fn push_frame(&mut self, frame: Vec<(String, Type)>) {
        self.frames.push_front(frame)
    }

    pub fn pop_frame(&mut self) -> Option<Vec<(String, Type)>> {
        self.frames.pop_front()
    }

    pub fn find(&mut self, key: &str) -> Option<&Type> {
        for vec in self.frames.iter() {
            for pair in vec.iter() {
                if pair.0 == key {
                    return Some(&pair.1);
                }
            }
        }
        None
    }
}

#[derive(Clone, Debug)]
pub struct TypeCheckError(String);

impl From<&str> for TypeCheckError {
    fn from(message: &str) -> Self {
        TypeCheckError(String::from(message))
    }
}

impl std::fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "TypeCheckError: {}", self.0)
    }
}

// allows other errors to wrap this one
// see https://doc.rust-lang.org/rust-by-example/error/multiple_error_types/define_error_type.html
impl std::error::Error for TypeCheckError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        // Generic error, underlying cause isn't tracked.
        None
    }
}

fn tc_binop_with_env(
    rest_exp: &[lexpr::Value],
    in1_typ: Type,
    in2_typ: Type,
    out_typ: Type,
    env: &mut Env,
) -> Result<Type, TypeCheckError> {
    if rest_exp.len() != 2 {
        return Err(TypeCheckError::from(
            "Binary operation has incorrect number of parameters.",
        ));
    };
    tc_with_env(&rest_exp[0], env).and_then(|in1| {
        tc_with_env(&rest_exp[1], env).and_then(|in2| {
            if in1 != in1_typ || in2 != in2_typ {
                Err(TypeCheckError::from(
                    "Binary operation parameters do not match expected types.",
                ))
            } else {
                Ok(out_typ)
            }
        })
    })
}

fn tc_if_with_env(rest_exp: &[lexpr::Value], env: &mut Env) -> Result<Type, TypeCheckError> {
    if rest_exp.len() != 3 {
        return Err(TypeCheckError::from(
            "If expression has incorrect number of values.",
        ));
    }
    tc_with_env(&rest_exp[0], env).and_then(|predicate| {
        tc_with_env(&rest_exp[1], env).and_then(|consequent| {
            tc_with_env(&rest_exp[2], env).and_then(|alternate| {
                if predicate != Type::Bool {
                    Err(TypeCheckError::from(
                        "Predicate in if expression does not evaluate to a boolean value.",
                    ))
                } else if consequent != alternate {
                    Err(TypeCheckError::from(
                        "Consequent and alternate values in if expression do not match types.",
                    ))
                } else {
                    Ok(consequent)
                }
            })
        })
    })
}

// TODO: add support for let expressions with more than one binding
fn tc_let_with_env(rest_exp: &[lexpr::Value], env: &mut Env) -> Result<Type, TypeCheckError> {
    if rest_exp.len() != 2 {
        return Err(TypeCheckError::from(
            "Let expression has incorrect number of values.",
        ));
    }
    let bindings = &rest_exp[0]; // ([x 23]) as vec of 1 element
    let body = &rest_exp[1];

    // ([x 23]) as vec of 1 element
    let bindings = match bindings.to_vec() {
        Some(vec) => vec,
        None => {
            return Err(TypeCheckError::from(
                "Let expression bindings are not in a proper list.",
            ))
        }
    };

    // [x 23] as lexpr value
    let binding = &bindings[0];

    // [x 23] as vec of 2 elements
    let binding = match binding.to_vec() {
        Some(vec) => vec,
        None => {
            return Err(TypeCheckError::from(
                "Let expression binding is not a proper list.",
            ))
        }
    };
    let var_name = match binding[0].as_symbol() {
        Some(val) => val,
        None => {
            return Err(TypeCheckError::from(
                "Let expression binding does not have a symbol on the left hand side.",
            ))
        }
    };
    let exp_type = match tc_with_env(&binding[1], env) {
        Ok(typ) => typ,
        Err(e) => return Err(e),
    };
    env.push_frame(vec![(String::from(var_name), exp_type)]);
    let typ = tc_with_env(body, env);
    env.pop_frame();
    typ
}

fn convert_annotation_to_type(annotation: &lexpr::Value) -> Result<Type, TypeCheckError> {
    match annotation {
        lexpr::Value::Symbol(val) => match val.as_ref() {
            "int" => Ok(Type::Int),
            "bool" => Ok(Type::Bool),
            "string" => Ok(Type::Str),
            _ => Err(TypeCheckError::from(
                "Type annotation not recognized as a valid type.",
            )),
        },
        lexpr::Value::Cons(_) => {
            // an array, ex. [Symbol(->), Symbol(int), Symbol(int), Symbol(bool)]
            let lst_vec = match annotation.to_vec() {
                Some(vec) => vec,
                None => {
                    return Err(TypeCheckError::from(
                        "Type annotation for function is not a valid list.",
                    ))
                }
            };
            // ensure that the function annotation has at least -> and a return type as elements
            if lst_vec.is_empty() {
                return Err(TypeCheckError::from("Type annotation is missing values."));
            }
            match lst_vec[0].as_symbol() {
                Some("->") => {
                    if lst_vec.len() < 2 {
                        return Err(TypeCheckError::from(
                            "Type annotation for function is missing values.",
                        ));
                    }
                    let input_types: Result<Vec<Type>, TypeCheckError> = lst_vec
                        [1..(lst_vec.len() - 1)]
                        .iter()
                        .map(|val| convert_annotation_to_type(val))
                        .collect();
                    let input_types_unwrapped = match input_types {
                        Ok(val) => val,
                        Err(e) => return Err(e),
                    };
                    let return_type = match convert_annotation_to_type(&lst_vec[lst_vec.len() - 1])
                    {
                        Ok(val) => val,
                        Err(e) => return Err(e),
                    };
                    Ok(Type::Func(input_types_unwrapped, Box::from(return_type)))
                }
                Some("list") => {
                    if lst_vec.len() != 2 {
                        return Err(TypeCheckError::from(
                            "Type annotation for list has incorrect number of values.",
                        ));
                    }
                    let lst_type = match convert_annotation_to_type(&lst_vec[1]) {
                        Ok(typ) => typ,
                        Err(e) => return Err(e),
                    };
                    Ok(Type::List(Box::from(lst_type)))
                }
                _ => Err(TypeCheckError::from(
                    r#"Type annotation for function does not have "->" or "list" as first symbol."#,
                )),
            }
        }
        _ => Err(TypeCheckError::from(
            "Type annotation is invalid or is missing.",
        )),
    }
}

// example
// input: lexpr::from_str("([x : int] [y : bool])")
// output: vec![(String::from("x"), Type::Int), (String::from("y"), Type::Bool)]
fn unwrap_lambda_args(args: &lexpr::Value) -> Result<Vec<(String, Type)>, TypeCheckError> {
    let arg_list = match args.to_vec() {
        Some(vec) => vec,
        None => {
            return Err(TypeCheckError::from(
                "Lambda arguments are not in a valid list.",
            ))
        }
    };
    arg_list
        .iter()
        .map(|arg| {
            // [x : int] as a vec
            let arg_vec = match arg.to_vec() {
                Some(vec) => vec,
                None => return Err(TypeCheckError::from("Lambda argument is not a valid list.")),
            };
            if arg_vec.len() != 3 {
                return Err(TypeCheckError::from(
                    "Lambda argument is missing values or contains extra values.",
                ));
            }

            // check there is a separator
            let separator = match arg_vec[1].as_symbol() {
                Some(val) => val,
                None => {
                    return Err(TypeCheckError::from(
                        "Lambda argument does not contain the correct : separator.",
                    ))
                }
            };
            if separator != ":" {
                return Err(TypeCheckError::from(
                    "Lambda argument does not contain the correct : separator.",
                ));
            }

            let arg_name = match arg_vec[0].as_symbol() {
                Some(val) => val,
                None => {
                    return Err(TypeCheckError::from(
                        "Lambda argument does not have a valid name.",
                    ))
                }
            };
            let arg_type = match convert_annotation_to_type(&arg_vec[2]) {
                Ok(typ) => typ,
                Err(e) => return Err(e),
            };
            Ok((String::from(arg_name), arg_type))
        })
        .collect()
}

fn tc_lambda_with_env(rest_exp: &[lexpr::Value], env: &mut Env) -> Result<Type, TypeCheckError> {
    if rest_exp.len() != 4 {
        return Err(TypeCheckError::from("Lambda expression contains incorrect number of values. Perhaps you are missing the return type?"));
    }
    let args = match unwrap_lambda_args(&rest_exp[0]) {
        Ok(val) => val,
        Err(e) => return Err(e),
    };

    // add arg types to the type environment for use in the body
    env.push_frame(args.clone());

    // check there is a separator
    let separator = match rest_exp[1].as_symbol() {
        Some(val) => val,
        None => return Err(TypeCheckError::from("Lambda expression does not have a separator between the arguments list and return type.")),
    };
    if separator != ":" {
        return Err(TypeCheckError::from("Lambda expression does not have the correct separator : between the arguments list and return type."));
    }

    // get the (annotated) return type
    let ret_type = match convert_annotation_to_type(&rest_exp[2]) {
        Ok(typ) => typ,
        Err(e) => return Err(e),
    };
    // type check lambda body
    let body_type = match tc_with_env(&rest_exp[3], env) {
        Ok(typ) => typ,
        Err(e) => return Err(e),
    };
    if ret_type != body_type {
        return Err(TypeCheckError::from(
            "Lambda expression body type does not match the expected return type.",
        ));
    }

    // remove local variables from environment since body has been type checked
    env.pop_frame();

    let arg_types: Vec<Type> = args.iter().map(|pair| pair.1.clone()).collect();
    Ok(Type::Func(arg_types, Box::from(ret_type)))
}

fn check_type_arrays_equal(arr1: &[Type], arr2: &[Type]) -> bool {
    if arr1.len() != arr2.len() {
        return false;
    }
    arr1.iter().zip(arr2.iter()).all(|(typ1, typ2)| match typ1 {
        Type::Func(arg_types1, ret_type_boxed1) => match typ2 {
            Type::Func(arg_types2, ret_type_boxed2) => {
                check_type_arrays_equal(arg_types1, arg_types2)
                    && (*ret_type_boxed1 == *ret_type_boxed2)
            }
            _ => false,
        },
        _ => typ1 == typ2,
    })
}

fn check_lambda_type_with_inputs(
    fn_type: &Type,
    param_types: &[Type],
) -> Result<Type, TypeCheckError> {
    match fn_type {
        Type::Func(arg_types, ret_type_boxed) => {
            let ret_type = ret_type_boxed.as_ref();
            if check_type_arrays_equal(&arg_types, &param_types) {
                Ok((*ret_type).clone())
            } else {
                Err(TypeCheckError::from(
                    "Argument types and parameter types of function application do not match.",
                ))
            }
        }
        _ => Err(TypeCheckError::from("Expected a function type.")),
    }
}

fn tc_apply_with_env(
    first_exp: &lexpr::Value,
    rest_exp: &[lexpr::Value],
    env: &mut Env,
) -> Result<Type, TypeCheckError> {
    match tc_with_env(first_exp, env) {
        Ok(typ) => {
            let param_types_wrapped = tc_array_with_env(&rest_exp, env);
            let param_types = match param_types_wrapped {
                Ok(val) => val,
                Err(e) => return Err(e),
            };
            check_lambda_type_with_inputs(&typ, &param_types)
        }
        Err(e) => Err(e),
    }
}

fn tc_array_with_env(values: &[lexpr::Value], env: &mut Env) -> Result<Vec<Type>, TypeCheckError> {
    values.iter().map(|val| tc_with_env(val, env)).collect()
}

fn tc_cons_with_env(rest_exp: &[lexpr::Value], env: &mut Env) -> Result<Type, TypeCheckError> {
    if rest_exp.len() != 2 {
        return Err(TypeCheckError::from(
            "Cons contains incorrect number of expressions.",
        ));
    }
    let car_type = match tc_with_env(&rest_exp[0], env) {
        Ok(typ) => typ,
        Err(e) => return Err(e),
    };
    let cdr_type = match tc_with_env(&rest_exp[1], env) {
        Ok(typ) => typ,
        Err(e) => return Err(e),
    };
    match cdr_type {
        Type::List(boxed_type) => {
            if *boxed_type == car_type {
                Ok(Type::List(boxed_type))
            } else {
                Err(TypeCheckError::from(
                    "Car of cons does not match type of cdr.",
                ))
            }
        }
        _ => Err(TypeCheckError::from(
            "Cdr of cons expression is not a list type.",
        )),
    }
}

fn tc_car_with_env(rest_exp: &[lexpr::Value], env: &mut Env) -> Result<Type, TypeCheckError> {
    if rest_exp.len() != 1 {
        return Err(TypeCheckError::from(
            "Car contains incorrect number of expressions.",
        ));
    }
    match tc_with_env(&rest_exp[0], env) {
        Ok(lst_type) => match lst_type {
            Type::List(boxed_type) => Ok(*boxed_type),
            _ => Err(TypeCheckError::from(
                "Expression in car is not a list type.",
            )),
        },
        Err(e) => Err(e),
    }
}

fn tc_cdr_with_env(rest_exp: &[lexpr::Value], env: &mut Env) -> Result<Type, TypeCheckError> {
    if rest_exp.len() != 1 {
        return Err(TypeCheckError::from(
            "Cdr contains incorrect number of expressions.",
        ));
    }
    match tc_with_env(&rest_exp[0], env) {
        Ok(lst_type) => match lst_type {
            Type::List(boxed_type) => Ok(Type::List(boxed_type)),
            _ => Err(TypeCheckError::from(
                "Expression in cdr is not a list type.",
            )),
        },
        Err(e) => Err(e),
    }
}

fn tc_null(rest_exp: &[lexpr::Value]) -> Result<Type, TypeCheckError> {
    if rest_exp.len() != 1 {
        return Err(TypeCheckError::from("Invalid null expression."));
    }
    match convert_annotation_to_type(&rest_exp[0]) {
        Ok(typ) => Ok(Type::List(Box::from(typ))),
        Err(e) => Err(e),
    }
}

fn tc_begin_with_env(rest_exp: &[lexpr::Value], env: &mut Env) -> Result<Type, TypeCheckError> {
    if rest_exp.is_empty() {
        return Err(TypeCheckError::from("Begin contains no expressions."));
    }
    // Note: it's important that even though we only return the type of the
    // last expression within the 'begin' S-expression, we still want to
    // type-check the entire array in case any type errors pop up
    match tc_array_with_env(rest_exp, env) {
        // the alternative here is:
        // Ok(types) => Ok(types[types.len() - 1].clone()),
        Ok(mut types) => Ok(types.remove(types.len() - 1)),
        Err(e) => Err(e),
    }
}

// set! returns the value that is being assigned, for lack of better options
fn tc_set_bang_with_env(rest_exp: &[lexpr::Value], env: &mut Env) -> Result<Type, TypeCheckError> {
    if rest_exp.len() != 2 {
        return Err(TypeCheckError::from(
            "Set contains incorrect number of expressions.",
        ));
    }
    let var_name = match rest_exp[0].as_symbol() {
        Some(val) => val,
        None => {
            return Err(TypeCheckError::from(
                "Variable in set! expression is not a valid symbol.",
            ))
        }
    };
    let expected_type = match env.find(var_name) {
        Some(typ) => typ.clone(),
        None => {
            return Err(TypeCheckError::from(
                "Variable assignment cannot occur before it has been defined!",
            ))
        }
    };
    match tc_with_env(&rest_exp[1], env) {
        Ok(typ) => {
            if typ == expected_type {
                Ok(typ)
            } else {
                Err(TypeCheckError::from(
                    "Type of set! body does not match variable's initialized type.",
                ))
            }
        }
        Err(e) => Err(e),
    }
}

// TODO: current "env" model only modifies the existing environment;
// this is likely unsafe once we try evaluating expressions that reuse
// binding names in different local scopes
// need to migrate to copying and adding items to environments where needed,
// - ideally use smaller data struct than hashmap
//   (but the typechecker performance is not important, so skip this)
// - perhaps use immutable environment to enforce type environment changes
//   only penetrating inner scopes
pub fn tc_with_env(value: &lexpr::Value, env: &mut Env) -> Result<Type, TypeCheckError> {
    match value {
        lexpr::Value::Number(_) => Ok(Type::Int),
        lexpr::Value::Bool(_) => Ok(Type::Bool),
        lexpr::Value::String(_) => Ok(Type::Str),
        lexpr::Value::Cons(_) => {
            let lst = match value.to_vec() {
                Some(vec) => vec,
                None => return Err(TypeCheckError::from("Cons expression is not a valid list.")),
            };
            // our language currently does not assign () to any meaning
            if lst.is_empty() {
                return Err(TypeCheckError::from("Empty list found."));
            }
            let lst_parts = lst.split_at(1);
            let first = &(lst_parts.0)[0];
            let rest = lst_parts.1;

            // type-checking an S-expression can be two cases, depending on the
            // type of the first value
            // - it could be a primitive operation or a Special Form, in which case
            //   we check the symbol manually and perform the right type check
            // - it could be an expression which needs to be evaluated (ex. a lambda)
            //   in which case we perform a general function-application type check
            match first.as_symbol() {
                Some(val) => match val {
                    "and" | "or" => {
                        tc_binop_with_env(&rest, Type::Bool, Type::Bool, Type::Bool, env)
                    }
                    "+" | "*" | "-" | "/" => {
                        tc_binop_with_env(&rest, Type::Int, Type::Int, Type::Int, env)
                    }
                    ">" | "<" | ">=" | "<=" | "=" => {
                        tc_binop_with_env(&rest, Type::Int, Type::Int, Type::Bool, env)
                    }
                    "concat" => tc_binop_with_env(&rest, Type::Str, Type::Str, Type::Str, env),
                    "if" => tc_if_with_env(&rest, env),
                    "let" => tc_let_with_env(&rest, env),
                    "lambda" => tc_lambda_with_env(&rest, env),
                    "begin" => tc_begin_with_env(&rest, env),
                    "set!" => tc_set_bang_with_env(&rest, env),
                    "cons" => tc_cons_with_env(&rest, env),
                    "car" => tc_car_with_env(&rest, env),
                    "cdr" => tc_cdr_with_env(&rest, env),
                    "null?" => Ok(Type::Bool),
                    "null" => tc_null(&rest),
                    s => {
                        let fn_type = match env.find(s) {
                            Some(fn_type) => fn_type,
                            None => {
                                return Err(TypeCheckError::from("Not a recognized function name."))
                            }
                        }
                        .clone();
                        let param_types = match tc_array_with_env(&rest, env) {
                            Ok(val) => val,
                            Err(e) => return Err(e),
                        };
                        check_lambda_type_with_inputs(&fn_type, &param_types)
                    }
                },
                None => tc_apply_with_env(&first, &rest, env),
            }
        }
        lexpr::Value::Symbol(x) => match &x[..] {
            "true" => Ok(Type::Bool),
            "false" => Ok(Type::Bool),
            s => match env.find(s) {
                Some(val) => Ok(val.clone()),
                None => Err(TypeCheckError::from("Not a recognized function name.")),
            },
        },
        _ => Err(TypeCheckError::from(
            "Unrecognized form of expression found.",
        )),
    }
}

pub fn type_check(value: &lexpr::Value) -> Result<Type, TypeCheckError> {
    tc_with_env(value, &mut Env::new())
}

// Only put tests for private helper functions here
// General type checker tests can go in tests/test_type_checker.rs
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_convert_annotations() {
        let exp = lexpr::from_str("int").unwrap();
        assert_eq!(convert_annotation_to_type(&exp).unwrap(), Type::Int);

        let exp = lexpr::from_str("bool").unwrap();
        assert_eq!(convert_annotation_to_type(&exp).unwrap(), Type::Bool);

        let exp = lexpr::from_str("string").unwrap();
        assert_eq!(convert_annotation_to_type(&exp).unwrap(), Type::Str);

        let exp = lexpr::from_str("(list int)").unwrap();
        assert_eq!(
            convert_annotation_to_type(&exp).unwrap(),
            Type::List(Box::from(Type::Int))
        );

        let exp = lexpr::from_str("(list (list int))").unwrap();
        assert_eq!(
            convert_annotation_to_type(&exp).unwrap(),
            Type::List(Box::from(Type::List(Box::from(Type::Int))))
        );

        let exp = lexpr::from_str("(-> int)").unwrap();
        assert_eq!(
            convert_annotation_to_type(&exp).unwrap(),
            Type::Func(vec![], Box::from(Type::Int))
        );

        let exp = lexpr::from_str("(-> int int)").unwrap();
        assert_eq!(
            convert_annotation_to_type(&exp).unwrap(),
            Type::Func(vec![Type::Int], Box::from(Type::Int))
        );

        let exp = lexpr::from_str("(-> string int bool)").unwrap();
        assert_eq!(
            convert_annotation_to_type(&exp).unwrap(),
            Type::Func(vec![Type::Str, Type::Int], Box::from(Type::Bool))
        );

        let exp = lexpr::from_str("(-> (-> int int bool) int int bool)").unwrap();
        assert_eq!(
            convert_annotation_to_type(&exp).unwrap(),
            Type::Func(
                vec![
                    Type::Func(vec![Type::Int, Type::Int], Box::from(Type::Bool)),
                    Type::Int,
                    Type::Int
                ],
                Box::from(Type::Bool)
            )
        );
    }

    #[test]
    fn test_check_type_arrays_equal_happy() {
        let types1 = vec![Type::Int, Type::Bool, Type::Str];
        let types2 = vec![Type::Int, Type::Bool, Type::Str];
        assert_eq!(check_type_arrays_equal(&types1, &types2), true);

        let types1 = vec![Type::Func(vec![], Box::from(Type::Int))];
        let types2 = vec![Type::Func(vec![], Box::from(Type::Int))];
        assert_eq!(check_type_arrays_equal(&types1, &types2), true);

        let types1 = vec![Type::Func(
            vec![Type::Int, Type::Int],
            Box::from(Type::Bool),
        )];
        let types2 = vec![Type::Func(
            vec![Type::Int, Type::Int],
            Box::from(Type::Bool),
        )];
        assert_eq!(check_type_arrays_equal(&types1, &types2), true);
    }

    #[test]
    fn test_check_type_arrays_equal_sad() {
        // types reordered
        let types1 = vec![Type::Int, Type::Bool, Type::Str];
        let types2 = vec![Type::Int, Type::Str, Type::Bool];
        assert_eq!(check_type_arrays_equal(&types1, &types2), false);

        // arr1 shorter
        let types1 = vec![Type::Int];
        let types2 = vec![Type::Int, Type::Str, Type::Bool];
        assert_eq!(check_type_arrays_equal(&types1, &types2), false);

        // arr2 shorter
        let types1 = vec![Type::Int, Type::Bool, Type::Str];
        let types2 = vec![Type::Int];
        assert_eq!(check_type_arrays_equal(&types1, &types2), false);

        // return types differ
        let types1 = vec![Type::Func(
            vec![Type::Int, Type::Int],
            Box::from(Type::Bool),
        )];
        let types2 = vec![Type::Func(vec![Type::Int, Type::Int], Box::from(Type::Str))];
        assert_eq!(check_type_arrays_equal(&types1, &types2), false);

        // input types differ
        let types1 = vec![Type::Func(
            vec![Type::Int, Type::Int],
            Box::from(Type::Bool),
        )];
        let types2 = vec![Type::Func(
            vec![Type::Int, Type::Str],
            Box::from(Type::Bool),
        )];
        assert_eq!(check_type_arrays_equal(&types1, &types2), false);
    }
}
