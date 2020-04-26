use crate::runtime::ffi;
use cranelift::codegen::binemit::NullTrapSink;
use cranelift::codegen::ir::function::DisplayFunction;
use cranelift::codegen::ir::{FuncRef, Function};
use cranelift::codegen::Context;
use cranelift::prelude::*;
use cranelift_module::{FuncId, Linkage};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};

pub type CraneliftModule = cranelift_module::Module<SimpleJITBackend>;

#[derive(Copy, Clone)]
#[repr(u32)]
pub enum SysFunction {
    Malloc,
    Init,
    GetResult,
    SetResult,
    GetErrno,

    Max,
}

struct StaticFunc {
    key: SysFunction,
    name: &'static str,
    ptr: *const u8,
    params: Vec<types::Type>,
    returns: Vec<types::Type>,
}

pub struct Module {
    module: CraneliftModule,
    ctx: Context,
    sys_funcs: Vec<Option<(FuncId, Signature)>>,
    user_funcs: Vec<Option<(FuncId, Signature)>>,
}

impl Module {
    pub fn new(num_funcs: usize) -> Module {
        let isa = Self::get_isa();
        let symbols = Self::get_static();

        let mut jit = SimpleJITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        for symbol in symbols.iter() {
            jit.symbol(symbol.name, symbol.ptr);
        }

        let mut module = cranelift_module::Module::new(jit);
        let ctx = module.make_context();

        let mut sys_funcs = Vec::with_capacity(SysFunction::Max as usize);
        sys_funcs.resize(SysFunction::Max as usize, None);

        let mut user_funcs = Vec::with_capacity(num_funcs);
        user_funcs.resize(num_funcs, None);

        for symbol in symbols {
            let mut sig = module.make_signature();
            sig.params = symbol.params.into_iter().map(AbiParam::new).collect();
            sig.returns = symbol.returns.into_iter().map(AbiParam::new).collect();
            let func_id = module
                .declare_function(symbol.name, Linkage::Import, &sig)
                .unwrap();

            assert_eq!(sys_funcs[symbol.key as usize], None);
            sys_funcs[symbol.key as usize] = Some((func_id, sig));
        }

        Module {
            module,
            ctx,
            sys_funcs,
            user_funcs,
        }
    }

    fn get_isa() -> Box<dyn isa::TargetIsa> {
        let mut flag_builder = settings::builder();
        if cfg!(debug_assertions) {
            flag_builder.enable("enable_verifier").unwrap();
        }

        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });

        let isa = isa_builder.finish(settings::Flags::new(flag_builder));

        let pointer_type = types::Type::triple_pointer_type(isa.triple());
        assert_eq!(pointer_type, types::I64);

        isa
    }

    fn get_static() -> Vec<StaticFunc> {
        vec![
            StaticFunc {
                key: SysFunction::Malloc,
                name: "weld_runst_malloc",
                ptr: ffi::weld_runst_malloc as *const u8,
                params: vec![types::I64, types::I64],
                returns: vec![types::I64],
            },
            StaticFunc {
                key: SysFunction::Init,
                name: "weld_runst_init",
                ptr: ffi::weld_runst_init as *const u8,
                params: vec![types::I32, types::I64],
                returns: vec![types::I64],
            },
            StaticFunc {
                key: SysFunction::GetResult,
                name: "weld_runst_get_result",
                ptr: ffi::weld_runst_get_result as *const u8,
                params: vec![types::I64],
                returns: vec![types::I64],
            },
            StaticFunc {
                key: SysFunction::SetResult,
                name: "weld_runst_set_result",
                ptr: ffi::weld_runst_set_result as *const u8,
                params: vec![types::I64, types::I64],
                returns: vec![],
            },

            StaticFunc {
                key: SysFunction::GetErrno,
                name: "weld_runst_get_errno",
                ptr: ffi::weld_runst_get_errno as *const u8,
                params: vec![types::I64],
                returns: vec![types::I64],
            }
        ]
    }

    pub fn declare_function(
        &mut self,
        id: usize,
        params: &[types::Type],
        returns: &[types::Type],
    ) -> FuncId {
        let mut sig = self.module.make_signature();
        sig.params = params.into_iter().map(|x| AbiParam::new(*x)).collect();
        sig.returns = returns.into_iter().map(|x| AbiParam::new(*x)).collect();

        let func_id = self
            .module
            .declare_function(&format!("{}", id), Linkage::Local, &sig)
            .unwrap();

        assert_eq!(self.user_funcs[id], None);
        self.user_funcs[id] = Some((func_id, sig));
        func_id
    }

    pub fn define_function(&mut self, id: usize) -> Function {
        self.module.clear_context(&mut self.ctx);

        let (func_id, sig) = self.user_funcs[id].clone().unwrap();
        Function::with_name_signature(func_id.into(), sig)
    }

    pub fn compile(&mut self, id: usize, func: Function) {
        self.ctx.func = func;
        let func_id = self.get_user_function(id);

        let mut trap_sink = NullTrapSink {};
        self.module
            .define_function(func_id, &mut self.ctx, &mut trap_sink)
            .unwrap();
    }

    pub fn finalize(&mut self) {
        self.module.finalize_definitions()
    }

    pub fn import_func(&mut self, func: &mut Function, func_id: FuncId) -> FuncRef {
        self.module.declare_func_in_func(func_id, func)
    }

    pub fn import_sys_func(&mut self, func: &mut Function, id: SysFunction) -> FuncRef {
        self.import_func(func, self.get_sys_function(id))
    }

    pub fn get_finalized_function(&mut self, id: FuncId) -> *const u8 {
        self.module.get_finalized_function(id)
    }

    pub fn get_sys_function(&self, id: SysFunction) -> FuncId {
        self.sys_funcs[id as usize].as_ref().unwrap().0
    }

    pub fn get_user_function(&self, id: usize) -> FuncId {
        self.user_funcs[id].as_ref().unwrap().0
    }

    pub fn target_config(&self) -> isa::TargetFrontendConfig {
        self.module.target_config()
    }

    pub fn display(&self) -> DisplayFunction {
        self.ctx.func.display(None)
    }
}

#[cfg(test)]
mod test {
    use cranelift::prelude::*;

    use super::*;

    #[test]
    fn test() {
        let mut module = Module::new(1);
        let mut func_ctx = FunctionBuilderContext::new();

        let func_id = module.declare_function(0, &[types::I32], &[types::I32]);

        let mut func = module.define_function(0);

        let mut builder = FunctionBuilder::new(&mut func, &mut func_ctx);

        let block0 = builder.create_block();
        builder.append_block_params_for_function_params(block0);
        builder.switch_to_block(block0);
        builder.seal_block(block0);

        let a = builder.block_params(block0)[0];
        let b = builder.ins().iconst(types::I32, 2);
        let arg = builder.ins().iadd(a, b);
        builder.ins().return_(&[arg]);
        builder.finalize();

        module.compile(0, func);

        println!("{}", module.ctx.func.display(None));

        module.finalize();

        let f_ptr = module.get_finalized_function(func_id);
        let f = unsafe { std::mem::transmute::<_, fn(i32) -> i32>(f_ptr) };
        let ret = f(22);
        assert_eq!(ret, 24);
    }
}
