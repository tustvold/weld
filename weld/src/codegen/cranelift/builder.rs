use cranelift::prelude::*;
use cranelift::codegen::ir::FuncRef;

pub fn call_single(builder: &mut FunctionBuilder, func: FuncRef, args: &[Value]) -> Value {
    let inst = builder.ins().call(func, args);

    let results = builder.inst_results(inst);
    assert_eq!(results.len(), 1);
    results[0]
}