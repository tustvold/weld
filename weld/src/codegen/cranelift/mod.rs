use std::collections::HashMap;

use cranelift::prelude::*;

use entry::gen_entry;
use layout::convert_type;
pub use layout::size_of;
use module::Module;

use crate::ast::{BinOpKind, LiteralKind};
use crate::codegen::cranelift::builder::call_single;
use crate::codegen::cranelift::module::SysFunction;
use crate::codegen::Runnable;
use crate::conf::ParsedConf;
use crate::error::WeldResult;
use crate::sir::{SirFunction, SirProgram, StatementKind, Terminator};
use crate::util::stats::CompilationStats;

mod builder;
mod entry;
mod layout;
mod module;

pub struct CraneliftRunnable {
    func: fn(i64) -> i64,
}

impl Runnable for CraneliftRunnable {
    fn run(&self, arg: i64) -> i64 {
        (self.func)(arg)
    }
}

pub fn load_library(libname: &str) -> WeldResult<()> {
    unimplemented!()
}

fn gen_function(
    module: &mut Module,
    func_ctx: &mut FunctionBuilderContext,
    id: usize,
    func: &SirFunction,
) {
    let mut func_def = module.define_function(id);
    let mut builder = FunctionBuilder::new(&mut func_def, func_ctx);

    let alloc_ref = module.import_sys_func(builder.func, SysFunction::Malloc);

    let mut variables = HashMap::with_capacity(func.locals.len());

    let mut max_idx = 0;
    for (s, t) in func.params.iter().chain(func.locals.iter()) {
        let variable = Variable::with_u32(max_idx);
        builder.declare_var(variable, convert_type(t));
        variables.insert(s.clone(), variable);
        max_idx += 1;
    }

    let runtime_ctx_var = Variable::with_u32(max_idx + 1);
    builder.declare_var(runtime_ctx_var, types::I64);

    for (block_idx, sir_block) in func.blocks.iter().enumerate() {
        let block = builder.create_block();
        builder.switch_to_block(block);

        if block_idx == 0 {
            builder.append_block_params_for_function_params(block);
            for (param_idx, (s, _)) in func.params.iter().enumerate() {
                let value = builder.block_params(block)[param_idx];
                builder.def_var(variables[s], value);
            }

            // The runtime_ctx is appended to the end of the parameter list
            let value = builder.block_params(block)[func.params.len()];
            builder.def_var(runtime_ctx_var, value);
        }

        for ins in sir_block.statements.iter() {
            match &ins.kind {
                StatementKind::AssignLiteral(x) => {
                    let out = variables[ins.output.as_ref().unwrap()];
                    let value = match x {
                        LiteralKind::BoolLiteral(l) => builder.ins().bconst(types::B8, *l),
                        LiteralKind::I8Literal(l) => builder.ins().iconst(types::I8, *l as i64),
                        LiteralKind::I16Literal(l) => builder.ins().iconst(types::I16, *l as i64),
                        LiteralKind::I32Literal(l) => builder.ins().iconst(types::I32, *l as i64),
                        LiteralKind::I64Literal(l) => builder.ins().iconst(types::I64, *l as i64),
                        LiteralKind::U8Literal(l) => builder.ins().iconst(types::I8, *l as i64),
                        LiteralKind::U16Literal(l) => builder.ins().iconst(types::I16, *l as i64),
                        LiteralKind::U32Literal(l) => builder.ins().iconst(types::I32, *l as i64),
                        LiteralKind::U64Literal(l) => builder.ins().iconst(types::I64, *l as i64),
                        LiteralKind::F32Literal(l) => builder.ins().f32const(f32::from_bits(*l)),
                        LiteralKind::F64Literal(l) => builder.ins().f64const(f64::from_bits(*l)),
                        LiteralKind::StringLiteral(_) => unimplemented!(),
                    };
                    builder.def_var(out, value);
                }
                StatementKind::Not(s) => {
                    let out = variables[ins.output.as_ref().unwrap()];
                    let var = variables[s];
                    let value = builder.use_var(var);
                    let cond = builder.ins().icmp_imm(IntCC::Equal, value, 0);
                    let new_value = builder.ins().bint(types::I8, cond);

                    builder.def_var(out, new_value)
                }
                StatementKind::BinOp { op, left, right } => {
                    match op {
                        BinOpKind::Add => {
                            let out = variables[ins.output.as_ref().unwrap()];

                            let l = builder.use_var(variables[left]);
                            let r = builder.use_var(variables[right]);
                            let new_value = builder.ins().iadd(l, r);

                            builder.def_var(out, new_value);
                        },
                        _ => unimplemented!()
                    }
                }
                _ => unimplemented!(),
            }
        }

        match &sir_block.terminator {
            Terminator::ProgramReturn(s) => {
                let set_ref = module.import_sys_func(builder.func, SysFunction::SetResult);
                let runtime_ctx = builder.use_var(runtime_ctx_var);

                let value = builder.use_var(variables[s]);

                let heap_size = size_of(&func.return_type);
                let heap_size_value = builder.ins().iconst(types::I64, heap_size as i64);
                let heap_value =
                    call_single(&mut builder, alloc_ref, &[runtime_ctx, heap_size_value]);

                builder.ins().store(MemFlags::new(), value, heap_value, 0);
                builder.ins().call(set_ref, &[runtime_ctx, heap_value]);
                builder.ins().return_(&[value]);
            }
            _ => unimplemented!(),
        }
    }

    builder.seal_all_blocks();
    builder.finalize();

    module.compile(id, func_def);

    println!("{}", module.display());
}

pub fn compile(
    program: &SirProgram,
    conf: &ParsedConf,
    stats: &mut CompilationStats,
) -> WeldResult<Box<dyn Runnable + Send + Sync>> {
    let mut module = Module::new(program.funcs.len() + 1);
    let mut func_ctx = FunctionBuilderContext::new();

    for (idx, f) in program.funcs.iter().enumerate() {
        let mut params = f
            .params
            .iter()
            .map(|(_, t)| convert_type(t))
            .collect::<Vec<_>>();

        // Push the run handle
        params.push(types::I64);

        let ret = convert_type(&f.return_type);
        module.declare_function(idx, params.as_slice(), &[ret]);
    }

    for (idx, f) in program.funcs.iter().enumerate() {
        gen_function(&mut module, &mut func_ctx, idx, f);
    }

    let entry_func_id = gen_entry(&mut module, &mut func_ctx, program);

    module.finalize();

    let f_ptr = module.get_finalized_function(entry_func_id);
    let f = unsafe { std::mem::transmute::<_, fn(i64) -> i64>(f_ptr) };

    // FIXME: This leaks code memory
    Ok(Box::new(CraneliftRunnable { func: f }))
}
