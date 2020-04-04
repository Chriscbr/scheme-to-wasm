### Error Handling
We try to use idiomatic Rust to handle error propagation at different stages of the compiler, by constructing specific structs (like `TypeCheckError`, `CodeGenerateError`, etc.) implementing `std::error::Error` to distinguish where an error occurred.
It might be slightly more idiomatic if we changed errors to be explicit enums (like `TypeCheckError::InvalidArgumentTypes`, `TypeCheckError::UnrecognizedIdentifier`, etc.) but we chose against this for sake of development speed.
It would also be more helpful if messages provided by errors included more specific information about the specific expressions or context causing the error, e.g. `TypeCheckError: 'foo' was not recognized as a function name.`. 

### Style guide

A lot of the compiler passes operate heavily on types and transforming and validating different type annotations.
Some of my initial code was inconsistent with the naming, but presently I am naming variables in new code (and trying to update older code) according to the following rules:
- No variables named named after keywords in Rust, such as `type` or `fn`
- The variable name `typ` alone is okay, but in all other cases the phrase `type` should be spelled out, e.g. `type_vec` or `inner_type`, not `typ_vec` or `inner_typ`.
