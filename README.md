### Style guide

A lot of the compiler passes operate heavily on types and transforming and validating different type annotations. Some of my initial code was inconsistent with the naming, but presently I am naming variables in new code (and trying to update older code) according to the following rules:
- No variables named named after keywords in Rust, such as `type` or `fn`
- The variable name `typ` alone is okay, but in all other cases the phrase `type` should be spelled out, e.g. `type_vec` or `inner_type`, not `typ_vec` or `inner_typ`.
