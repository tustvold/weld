use std::collections::HashMap;

use cranelift::prelude::*;

use entry::gen_entry;
use layout::convert_type;
pub use layout::size_of;
use module::Module;

use crate::ast::{BinOpKind, LiteralKind, ScalarKind, Type};
use crate::codegen::cranelift::builder::call_single;
use crate::codegen::cranelift::layout::convert_scalar;
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
) -> WeldResult<()> {
    let mut func_def = module.define_function(id);
    let mut builder = FunctionBuilder::new(&mut func_def, func_ctx);

    let alloc_ref = module.import_sys_func(builder.func, SysFunction::Malloc);

    let mut variables = HashMap::with_capacity(func.locals.len());

    let mut max_idx = 0;
    for (s, t) in func.params.iter().chain(func.locals.iter()) {
        let variable = Variable::with_u32(max_idx);
        builder.declare_var(variable, convert_type(t));
        variables.insert(s.clone(), (variable, t.clone()));
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
                builder.def_var(variables[s].0, value);
            }

            // The runtime_ctx is appended to the end of the parameter list
            let value = builder.block_params(block)[func.params.len()];
            builder.def_var(runtime_ctx_var, value);
        }

        for ins in sir_block.statements.iter() {
            match &ins.kind {
                StatementKind::AssignLiteral(x) => {
                    let out = variables[ins.output.as_ref().unwrap()].0;
                    let value = match x {
                        LiteralKind::BoolLiteral(l) => {
                            builder.ins().iconst(types::I8, if *l { 1 } else { 0 })
                        }
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
                    let out = variables[ins.output.as_ref().unwrap()].0;
                    let var = variables[s].0;
                    let value = builder.use_var(var);
                    let cond = builder.ins().icmp_imm(IntCC::Equal, value, 0);
                    let new_value = builder.ins().bint(types::I8, cond);

                    builder.def_var(out, new_value)
                }
                StatementKind::BinOp { op, left, right } => {
                    let out = variables[ins.output.as_ref().unwrap()].0;
                    let l_v = &variables[left];
                    let r_v = &variables[right];
                    let l = builder.use_var(l_v.0);
                    let r = builder.use_var(r_v.0);

                    if l_v.1 != r_v.1 {
                        return compile_err!(
                            "Cannot perform {} on disjoint types {} and {}",
                            op,
                            l_v.1,
                            r_v.1
                        );
                    }

                    let new_value = match l_v.1 {
                        Type::Scalar(s) | Type::Simd(s) => match op {
                            BinOpKind::Add if s.is_integer() => builder.ins().iadd(l, r),
                            BinOpKind::Add if s.is_float() => builder.ins().fadd(l, r),

                            BinOpKind::Subtract if s.is_integer() => builder.ins().isub(l, r),
                            BinOpKind::Subtract if s.is_float() => builder.ins().fsub(l, r),

                            BinOpKind::Multiply if s.is_integer() => builder.ins().imul(l, r),
                            BinOpKind::Multiply if s.is_float() => builder.ins().fmul(l, r),

                            BinOpKind::Divide if s.is_signed_integer() => builder.ins().sdiv(l, r),
                            BinOpKind::Divide if s.is_unsigned_integer() => {
                                builder.ins().udiv(l, r)
                            }
                            BinOpKind::Divide if s.is_float() => builder.ins().fdiv(l, r),

                            BinOpKind::Modulo if s.is_signed_integer() => builder.ins().srem(l, r),
                            BinOpKind::Modulo if s.is_unsigned_integer() => {
                                builder.ins().urem(l, r)
                            }
                            BinOpKind::Modulo if s.is_float() => unimplemented!(),

                            BinOpKind::Equal if s.is_integer() || s.is_bool() => {
                                let cond = builder.ins().icmp(IntCC::Equal, l, r);
                                builder.ins().bint(types::I8, cond)
                            }
                            BinOpKind::Equal if s.is_float() => {
                                let cond = builder.ins().fcmp(FloatCC::Equal, l, r);
                                builder.ins().bint(types::I8, cond)
                            }

                            BinOpKind::NotEqual if s.is_integer() || s.is_bool() => {
                                let cond = builder.ins().icmp(IntCC::NotEqual, l, r);
                                builder.ins().bint(types::I8, cond)
                            }
                            BinOpKind::NotEqual if s.is_float() => {
                                let cond = builder.ins().fcmp(FloatCC::OrderedNotEqual, l, r);
                                builder.ins().bint(types::I8, cond)
                            }

                            BinOpKind::LessThan if s.is_signed_integer() => {
                                let cond = builder.ins().icmp(IntCC::SignedLessThan, l, r);
                                builder.ins().bint(types::I8, cond)
                            }
                            BinOpKind::LessThan if s.is_unsigned_integer() => {
                                let cond = builder.ins().icmp(IntCC::UnsignedLessThan, l, r);
                                builder.ins().bint(types::I8, cond)
                            }
                            BinOpKind::LessThan if s.is_float() => {
                                let cond = builder.ins().fcmp(FloatCC::LessThan, l, r);
                                builder.ins().bint(types::I8, cond)
                            }

                            BinOpKind::LessThanOrEqual if s.is_signed_integer() => {
                                let cond = builder.ins().icmp(IntCC::SignedLessThanOrEqual, l, r);
                                builder.ins().bint(types::I8, cond)
                            }
                            BinOpKind::LessThanOrEqual if s.is_unsigned_integer() => {
                                let cond = builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, l, r);
                                builder.ins().bint(types::I8, cond)
                            }
                            BinOpKind::LessThanOrEqual if s.is_float() => {
                                let cond = builder.ins().fcmp(FloatCC::LessThanOrEqual, l, r);
                                builder.ins().bint(types::I8, cond)
                            }

                            BinOpKind::GreaterThan if s.is_signed_integer() => {
                                let cond = builder.ins().icmp(IntCC::SignedGreaterThan, l, r);
                                builder.ins().bint(types::I8, cond)
                            }
                            BinOpKind::GreaterThan if s.is_unsigned_integer() => {
                                let cond = builder.ins().icmp(IntCC::UnsignedGreaterThan, l, r);
                                builder.ins().bint(types::I8, cond)
                            }
                            BinOpKind::GreaterThan if s.is_float() => {
                                let cond = builder.ins().fcmp(FloatCC::GreaterThan, l, r);
                                builder.ins().bint(types::I8, cond)
                            }

                            BinOpKind::GreaterThanOrEqual if s.is_signed_integer() => {
                                let cond =
                                    builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, l, r);
                                builder.ins().bint(types::I8, cond)
                            }
                            BinOpKind::GreaterThanOrEqual if s.is_unsigned_integer() => {
                                let cond =
                                    builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, l, r);
                                builder.ins().bint(types::I8, cond)
                            }
                            BinOpKind::GreaterThanOrEqual if s.is_float() => {
                                let cond = builder.ins().fcmp(FloatCC::GreaterThanOrEqual, l, r);
                                builder.ins().bint(types::I8, cond)
                            }

                            BinOpKind::LogicalAnd if s.is_bool() => builder.ins().band(l, r),
                            BinOpKind::BitwiseAnd if s.is_integer() || s.is_bool() => {
                                builder.ins().band(l, r)
                            }

                            BinOpKind::LogicalOr if s.is_bool() => builder.ins().bor(l, r),
                            BinOpKind::BitwiseOr if s.is_integer() || s.is_bool() => {
                                builder.ins().bor(l, r)
                            }

                            BinOpKind::Xor if s.is_integer() || s.is_bool() => {
                                builder.ins().bxor(l, r)
                            }

                            BinOpKind::Max if s.is_signed_integer() => {
                                let cond = builder.ins().icmp(IntCC::SignedGreaterThan, l, r);
                                builder.ins().select(cond, l, r)
                            }
                            BinOpKind::Max if s.is_unsigned_integer() => {
                                let cond = builder.ins().icmp(IntCC::UnsignedGreaterThan, l, r);
                                builder.ins().select(cond, l, r)
                            }
                            BinOpKind::Max if s.is_float() => {
                                let cond = builder.ins().fcmp(FloatCC::GreaterThan, l, r);
                                builder.ins().select(cond, l, r)
                            }

                            BinOpKind::Min if s.is_signed_integer() => {
                                let cond = builder.ins().icmp(IntCC::SignedLessThan, l, r);
                                builder.ins().select(cond, l, r)
                            }
                            BinOpKind::Min if s.is_unsigned_integer() => {
                                let cond = builder.ins().icmp(IntCC::UnsignedLessThan, l, r);
                                builder.ins().select(cond, l, r)
                            }
                            BinOpKind::Min if s.is_float() => {
                                let cond = builder.ins().fcmp(FloatCC::LessThan, l, r);
                                builder.ins().select(cond, l, r)
                            }

                            _ => return compile_err!("Unsupported binary op: {} on {}", op, l_v.1),
                        },
                        _ => return compile_err!("Unsupported binary op: {} on {}", op, l_v.1),
                    };

                    builder.def_var(out, new_value)
                }
                StatementKind::Select {
                    cond,
                    on_true,
                    on_false,
                } => {
                    let out = variables[ins.output.as_ref().unwrap()].0;
                    let condition = builder.use_var(variables[cond].0);
                    let true_var = builder.use_var(variables[on_true].0);
                    let false_var = builder.use_var(variables[on_false].0);

                    let value = builder.ins().select(condition, true_var, false_var);

                    builder.def_var(out, value);
                }
                StatementKind::Cast(s, dst_t) => {
                    let out = variables[ins.output.as_ref().unwrap()].0;

                    let source = &variables[s];
                    let src_value = builder.use_var(source.0);
                    let src_t = &source.1;

                    let new_value = match (src_t, dst_t) {
                        (Type::Scalar(src), Type::Scalar(dst)) => {
                            let src_conv = convert_scalar(*src);
                            let dst_conv = convert_scalar(*dst);

                            match (src, dst) {
                                (_, _) if src_conv == dst_conv => src_value,

                                (ScalarKind::F32, ScalarKind::F64) => {
                                    builder.ins().fpromote(types::F32, src_value)
                                }
                                (ScalarKind::F64, ScalarKind::F32) => {
                                    builder.ins().fdemote(types::F64, src_value)
                                }

                                // Floating point to signed integer
                                (_, _) if src.is_float() && dst.is_signed_integer() => {
                                    builder.ins().fcvt_to_sint(dst_conv, src_value)
                                }

                                // Floating point to unsigned integer
                                (_, _) if src.is_float() && dst.is_unsigned_integer() => {
                                    builder.ins().fcvt_to_uint(dst_conv, src_value)
                                }

                                // Signed integer to floating point
                                (_, _) if src.is_signed_integer() && dst.is_float() => {
                                    builder.ins().fcvt_from_sint(dst_conv, src_value)
                                }

                                // Unsigned integer to floating point
                                (_, _) if src.is_unsigned_integer() && dst.is_float() => {
                                    builder.ins().fcvt_from_uint(dst_conv, src_value)
                                }

                                // Boolean to other integers. Since booleans are i8s, we either zero-extend them if
                                // the target type is larger, or simply return the same type otherwise.
                                (ScalarKind::Bool, _) if dst.is_integer() && dst.bits() > 8 => {
                                    builder.ins().uextend(dst_conv, src_value)
                                }
                                (ScalarKind::Bool, _) if dst.is_integer() => src_value,

                                // Zero-extension.
                                (_, _) if src.is_unsigned_integer() && dst.bits() > src.bits() => {
                                    builder.ins().uextend(dst_conv, src_value)
                                }

                                // Sign-extension.
                                (_, _) if src.is_signed_integer() && dst.bits() > src.bits() => {
                                    builder.ins().sextend(dst_conv, src_value)
                                }

                                // Truncation
                                (_, _) if dst.bits() < src.bits() => {
                                    builder.ins().ireduce(dst_conv, src_value)
                                }

                                // Bitcast
                                (_, _) if dst.bits() == src.bits() => {
                                    builder.ins().raw_bitcast(dst_conv, src_value)
                                }
                                _ => return compile_err!("Cannot cast {} to {}", src_t, dst_t),
                            }
                        }
                        _ => return compile_err!("Cannot cast {} to {}", src_t, dst_t),
                    };

                    builder.def_var(out, new_value)
                }
                _ => unimplemented!(),
            }
        }

        match &sir_block.terminator {
            Terminator::ProgramReturn(s) => {
                let set_ref = module.import_sys_func(builder.func, SysFunction::SetResult);
                let runtime_ctx = builder.use_var(runtime_ctx_var);

                let value = builder.use_var(variables[s].0);

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

    Ok(())
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
        gen_function(&mut module, &mut func_ctx, idx, f)?;
    }

    let entry_func_id = gen_entry(&mut module, &mut func_ctx, program);

    module.finalize();

    let f_ptr = module.get_finalized_function(entry_func_id);
    let f = unsafe { std::mem::transmute::<_, fn(i64) -> i64>(f_ptr) };

    // FIXME: This leaks code memory
    Ok(Box::new(CraneliftRunnable { func: f }))
}
