use cranelift::prelude::*;

use entry::gen_entry;
use module::Module;

use crate::ast::{ScalarKind, Type};
use crate::codegen::Runnable;
use crate::conf::ParsedConf;
use crate::error::WeldResult;
use crate::sir::SirProgram;
use crate::util::stats::CompilationStats;

mod entry;
mod module;
mod builder;

pub struct CraneliftRunnable {
    func: fn(i64) -> i64,
}

impl Runnable for CraneliftRunnable {
    fn run(&self, arg: i64) -> i64 {
        (self.func)(arg)
    }
}

pub fn convert_scalar(s: &ScalarKind) -> types::Type {
    match s {
        ScalarKind::Bool => types::B8,
        ScalarKind::I8 => types::I8,
        ScalarKind::I16 => types::I16,
        ScalarKind::I32 => types::I32,
        ScalarKind::I64 => types::I64,
        ScalarKind::U8 => types::I8,
        ScalarKind::U16 => types::I16,
        ScalarKind::U32 => types::I32,
        ScalarKind::U64 => types::I64,
        ScalarKind::F32 => types::F32,
        ScalarKind::F64 => types::F64,
    }
}

pub fn convert_type(t: &Type) -> types::Type {
    match t {
        Type::Scalar(x) => convert_scalar(x),
        _ => unimplemented!(),
    }
}

pub fn load_library(libname: &str) -> WeldResult<()> {
    unimplemented!()
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

    let entry_func_id = gen_entry(&mut module, &mut func_ctx, program);

    module.finalize();

    let f_ptr = module.get_finalized_function(entry_func_id);
    let f = unsafe { std::mem::transmute::<_, fn(i64) -> i64>(f_ptr) };

    // FIXME: This leaks code memory
    Ok(Box::new(CraneliftRunnable { func: f }))
}

pub fn size_of(ty: &Type) -> usize {
    unimplemented!()
}
