use crate::error::WeldResult;
use crate::sir::SirProgram;
use crate::conf::ParsedConf;
use crate::util::stats::CompilationStats;
use crate::codegen::Runnable;
use crate::ast::Type;

pub fn load_library(libname: &str) -> WeldResult<()> {
    unimplemented!()
}

pub fn compile(
    program: &SirProgram,
    conf: &ParsedConf,
    stats: &mut CompilationStats,
) -> WeldResult<Box<dyn Runnable + Send + Sync>> {
    unimplemented!()
}

pub fn size_of(ty: &Type) -> usize {
    unimplemented!()
}
