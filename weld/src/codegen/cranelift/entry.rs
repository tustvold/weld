use cranelift::prelude::*;
use cranelift_module::FuncId;

use crate::codegen::cranelift::module::{Module, SysFunction};
use crate::codegen::{WeldInputArgs, WeldOutputArgs};
use crate::runtime;
use crate::sir::SirProgram;
use crate::codegen::cranelift::builder::call_single;

pub fn gen_entry(
    module: &mut Module,
    func_ctx: &mut FunctionBuilderContext,
    program: &SirProgram,
) -> FuncId {
    let id = program.funcs.len();

    let func_id = module.declare_function(id, &[types::I64], &[types::I64]);
    let mut func = module.define_function(id);

    let alloc_ref = module.import_sys_func(&mut func, SysFunction::Malloc);
    let init_ref = module.import_sys_func(&mut func, SysFunction::Init);
    let get_result = module.import_sys_func(&mut func, SysFunction::GetResult);
    let get_errno = module.import_sys_func(&mut func, SysFunction::GetErrno);

    let mut builder = FunctionBuilder::new(&mut func, func_ctx);

    let block0 = builder.create_block();
    let block1 = builder.create_block();
    let block2 = builder.create_block();

    builder.append_block_param(block2, types::I64);
    builder.append_block_param(block1, types::I64);

    {
        builder.append_block_params_for_function_params(block0);
        builder.switch_to_block(block0);
        builder.seal_block(block0);

        let input_args_ref = builder.block_params(block0)[0];

        let runtime_ctx = builder.ins().load(
            types::I64,
            MemFlags::new(),
            input_args_ref,
            offset_of!(WeldInputArgs, run) as i32,
        );

        let zero = builder.ins().iconst(types::I64, 0);
        builder
            .ins()
            .br_icmp(IntCC::Equal, runtime_ctx, zero, block1, &[input_args_ref]);

        builder.ins().jump(block2, &[runtime_ctx]);
    }

    {
        builder.switch_to_block(block1);
        builder.seal_block(block1);

        let input_args_ref = builder.block_params(block1)[0];

        let nworkers = builder.ins().load(
            types::I32,
            MemFlags::new(),
            input_args_ref,
            offset_of!(WeldInputArgs, nworkers) as i32,
        );

        let memlimit = builder.ins().load(
            types::I64,
            MemFlags::new(),
            input_args_ref,
            offset_of!(WeldInputArgs, mem_limit) as i32,
        );

        let runtime_ctx = call_single(&mut builder, init_ref, &[nworkers, memlimit]);

        builder.ins().jump(block2, &[runtime_ctx]);
    }

    {
        builder.switch_to_block(block2);
        builder.seal_block(block2);

        let runtime_ctx = builder.block_params(block2)[0];

        let out_size = builder
            .ins()
            .iconst(types::I64, std::mem::size_of::<WeldOutputArgs>() as i64);

        let ret = call_single(&mut builder, alloc_ref, &[runtime_ctx, out_size]);

        let result  = call_single(&mut builder, get_result, &[runtime_ctx]);
        let errno = call_single(&mut builder, get_errno, &[runtime_ctx]);

        builder.ins().store(MemFlags::new(), result, ret, offset_of!(WeldOutputArgs, output) as i32);
        builder.ins().store(MemFlags::new(), runtime_ctx, ret, offset_of!(WeldOutputArgs, run) as i32);
        builder.ins().store(MemFlags::new(), errno, ret, offset_of!(WeldOutputArgs, errno) as i32);

        builder.ins().return_(&[ret]);
    }

    builder.finalize();

    module.compile(id, func);

    println!("{}", module.display());

    func_id
}
