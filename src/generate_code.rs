// Just experimenting with WASM

// Some useful examples at
// https://github.com/paritytech/parity-wasm/tree/master/examples

use parity_wasm::{builder, elements};

use wasmer_runtime::{error, imports, instantiate, Value};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_foo() {
        let module = builder::module()
            .function()
            .signature()
            .with_param(elements::ValueType::I32)
            .with_return_type(Some(elements::ValueType::I32))
            .build()
            .body()
            .with_instructions(elements::Instructions::new(vec![
                elements::Instruction::GetLocal(0),
                elements::Instruction::I32Const(1),
                elements::Instruction::I32Add,
                elements::Instruction::End,
            ]))
            .build()
            .build()
            .export()
            .field("add_one")
            .internal()
            .func(0)
            .build()
            .build();

        let output = parity_wasm::serialize(module.clone()).unwrap();

        // Can output to a file for debugging/decompiling if necessary.
        // parity_wasm::serialize_to_file(std::env::current_dir().unwrap().join("foo.wasm"), module)
        //     .unwrap();

        let import_object = imports! {};
        let instance = instantiate(&output, &import_object).unwrap();
        let values = instance
            .dyn_func("add_one")
            .unwrap()
            .call(&[Value::I32(42)])
            .unwrap();

        assert_eq!(values[0], Value::I32(43));
    }
}
