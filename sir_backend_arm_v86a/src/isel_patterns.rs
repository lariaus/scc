use sir_backend::{
    backend_ops::BackendRegToValOp,
    instruction_selection_pass::{
        ISelDAGHelper, InstructionSelectionOptions, InstructionSelectionPass,
    },
};
use sir_core::operation::OperationImpl;
use sir_core::{
    attributes::{Attribute, IntegerAttr, RegisterAttr},
    ir_builder::{BlockArgumentReplacements, InsertionPoint},
    ir_context::IRContext,
    ir_printer::IRPrintableObject,
    ir_visitor::{make_ir_visitor_mut, WalkOlder},
    types::{IntegerType, PointerType, Type},
    value::Value,
};
use sir_func::func_ops::{FunctionOp, ReturnOp};
use sir_lir::lir_ops::{LIRAllocaOp, LIRIAddOp, LIRLoadOp, LIRStoreOp};
use sir_transform::{dag_transformer::DAGPattern, ir_rewriter::IRRewriter, pass::PassRegistration};
use utils::bitmanip_utils::offset_align_p2;

use crate::armv86a_ops::{ArmAddIns, ArmLdrIns, ArmProcOp, ArmRetIns, ArmStrIns};

fn get_virtual_reg_name(_ty: &Type) -> String {
    // Need to support other types.
    return "mk_w".to_owned();
}

fn alloc_virtual_reg(helper: &mut ISelDAGHelper, ty: &Type) -> Attribute {
    helper.alloc_new_virtual_reg(get_virtual_reg_name(ty))
}

fn map_new_virtual_reg(helper: &mut ISelDAGHelper, val: Value) -> Attribute {
    let name = get_virtual_reg_name(val.get_type());
    helper.get_or_map_virtual_reg(val.as_id(), &name)
}

fn compute_alloca_sp_offset(helper: &ISelDAGHelper, val: Value) -> usize {
    let alloc_offset = helper.get_stack_mem_offset(val.as_id()).unwrap();
    let alloc_size = get_type_size(val.get_type().cast::<PointerType>().unwrap().element());
    helper.get_stack_frame_offset() - alloc_offset - alloc_size
}

// Returns the size of a type in bytes
// We probably want to move this somewhere else
fn get_type_size(ty: &Type) -> usize {
    match ty {
        Type::Int(integer_type) => integer_type.bitwidth() / 8,
        Type::Float(float_type) => float_type.bitdwith() / 8,
        Type::Ptr(_pointer_type) => 8,
        _ => panic!("Unsupported type {}", ty.to_string_repr()),
    }
}

struct ConvertFunctionOp;

impl DAGPattern<ISelDAGHelper> for ConvertFunctionOp {
    fn get_cost(&self) -> usize {
        0
    }

    fn match_op(
        &self,
        _helper: &ISelDAGHelper,
        op: sir_core::operation::GenericOperation,
    ) -> Option<Vec<sir_core::ir_data::OperationID>> {
        if op.isa::<FunctionOp>() {
            Some(vec![op.as_id()])
        } else {
            None
        }
    }

    fn rewrite_op(
        &self,
        helper: &mut ISelDAGHelper,
        rewriter: &mut IRRewriter,
        op: sir_core::ir_data::OperationID,
    ) {
        let func_op = rewriter.get_operation(op).cast::<FunctionOp>().unwrap();
        let loc = func_op.loc();
        let symbol_name = func_op.get_symbol_name().to_owned();
        let old_block = func_op.get_body().as_id();
        let func_type = func_op.get_function_type().clone();

        // Prepare the I/Os virtual regs.
        let mut inputs_regs = Vec::new();
        let mut outputs_regs = Vec::new();
        for arg_ty in func_type.arguments() {
            inputs_regs.push(alloc_virtual_reg(helper, arg_ty));
        }
        for ret_ty in func_type.results() {
            outputs_regs.push(alloc_virtual_reg(helper, ret_ty));
        }

        // Go through all ops to find and handle allocas.
        func_op.walk_mut(
            WalkOlder::PreOrder,
            &mut make_ir_visitor_mut(|alloc_op: LIRAllocaOp| {
                let alloc_size = get_type_size(
                    alloc_op
                        .get_result()
                        .get_type()
                        .cast::<PointerType>()
                        .unwrap()
                        .element(),
                );
                helper.get_or_alloc_stack_mem(
                    alloc_op.get_result().as_id(),
                    alloc_op.get_align() as usize,
                    alloc_size,
                );
            }),
        );
        // Compute the frame size.
        // It needs to be properly aligned.
        let frame_size = offset_align_p2(helper.get_stack_frame_offset(), 16);
        helper.set_stack_frame_offset(frame_size);

        // Create the new function.
        let new_func = rewriter
            .create_op(
                loc,
                ArmProcOp::build(
                    symbol_name,
                    Some(inputs_regs.iter().map(|attr| attr.clone()).collect()),
                    Some(outputs_regs.iter().map(|attr| attr.clone()).collect()),
                    Some(frame_size as u64),
                ),
            )
            .as_id();

        // Create a new block without arguments.
        let new_block = rewriter.create_block_at_end_of(new_func, loc, &[]).as_id();

        rewriter.set_insertion_point(InsertionPoint::AtEndOf(new_block));

        // Materialize all the arguments in the new block (ISEL will get rif of those ops).
        let mut new_inputs = Vec::new();
        for (input_reg, input_ty) in inputs_regs.into_iter().zip(func_type.arguments()) {
            let val = rewriter
                .create_op(
                    loc,
                    BackendRegToValOp::build(input_reg.clone(), input_ty.clone()),
                )
                .get_result()
                .as_id();
            helper.map_reg(val, input_reg);
            new_inputs.push(val);
        }

        // Move all ops to the new block.
        rewriter.splice_block_at(
            old_block,
            InsertionPoint::AtEndOf(new_block),
            BlockArgumentReplacements::ReplaceWith(new_inputs),
        );

        // Assign the outputs registers.
        let ret_op = rewriter.get_block(new_block).get_terminator_op().unwrap();
        for (output_reg, ret_val) in outputs_regs.into_iter().zip(ret_op.get_inputs()) {
            helper.map_reg(ret_val.as_id(), output_reg);
        }

        // Replace the op.
        rewriter.replace_op_with_op(op, new_func);

        // The new block must be rewriten using DAG.
        helper.set_dag_blocks(vec![new_block]);
    }
}

// convert lir.store(val, lir.alloca(...)) => arm.str rdst, [fp, #<stack-offset>]
struct ConvertStoreAlloca;

impl DAGPattern<ISelDAGHelper> for ConvertStoreAlloca {
    fn get_cost(&self) -> usize {
        0
    }

    fn match_op(
        &self,
        helper: &ISelDAGHelper,
        op: sir_core::operation::GenericOperation,
    ) -> Option<Vec<sir_core::ir_data::OperationID>> {
        let op = op.cast::<LIRStoreOp>()?;
        if !helper.is_mapped(op.get_value().as_id()) {
            return None;
        }
        let alloc_op = op.get_ptr().defining_op()?.cast::<LIRAllocaOp>()?;

        Some(vec![alloc_op.as_id(), op.as_id()])
    }

    fn rewrite_op(
        &self,
        helper: &mut ISelDAGHelper,
        rewriter: &mut IRRewriter,
        op: sir_core::ir_data::OperationID,
    ) {
        // Compute the offset.
        let op = rewriter.get_operation(op).cast::<LIRStoreOp>().unwrap();
        let alloc_op = op
            .get_ptr()
            .defining_op()
            .unwrap()
            .cast::<LIRAllocaOp>()
            .unwrap();
        let sp_offset = compute_alloca_sp_offset(helper, alloc_op.get_result());

        // Build the str instruction.
        let val_reg = helper.get_reg(op.get_value().as_id()).unwrap().clone();
        let offset_attr = IntegerAttr::new(sp_offset as u64, IntegerType::index_type());
        let sp_reg = RegisterAttr::new("sp".to_owned());
        rewriter.create_op(op.loc(), ArmStrIns::build(val_reg, sp_reg, offset_attr));
    }
}

// convert lir.load(lir.alloca(...)) => arm.ldr rdst, [fp, #<stack-offset>]
struct ConvertLoadAlloca;

impl DAGPattern<ISelDAGHelper> for ConvertLoadAlloca {
    fn get_cost(&self) -> usize {
        0
    }

    fn match_op(
        &self,
        _helper: &ISelDAGHelper,
        op: sir_core::operation::GenericOperation,
    ) -> Option<Vec<sir_core::ir_data::OperationID>> {
        let op = op.cast::<LIRLoadOp>()?;
        let alloc_op = op.get_ptr().defining_op()?.cast::<LIRAllocaOp>()?;
        Some(vec![alloc_op.as_id(), op.as_id()])
    }

    fn rewrite_op(
        &self,
        helper: &mut ISelDAGHelper,
        rewriter: &mut IRRewriter,
        op: sir_core::ir_data::OperationID,
    ) {
        // Compute the offset.
        let op = rewriter.get_operation(op).cast::<LIRLoadOp>().unwrap();
        let alloc_op = op
            .get_ptr()
            .defining_op()
            .unwrap()
            .cast::<LIRAllocaOp>()
            .unwrap();
        let sp_offset = compute_alloca_sp_offset(helper, alloc_op.get_result());

        // Build the ldr instruction.
        let out_reg = map_new_virtual_reg(helper, op.get_result());
        let offset_attr = IntegerAttr::new(sp_offset as u64, IntegerType::index_type());
        let sp_reg = RegisterAttr::new("sp".to_owned());
        rewriter.create_op(op.loc(), ArmLdrIns::build(out_reg, sp_reg, offset_attr));
    }
}

// convert lir.iadd(lhs, rhs) => arm.ldr rdst, rlhs, rhs
struct ConvertIadd;

impl DAGPattern<ISelDAGHelper> for ConvertIadd {
    fn get_cost(&self) -> usize {
        0
    }

    fn match_op(
        &self,
        helper: &ISelDAGHelper,
        op: sir_core::operation::GenericOperation,
    ) -> Option<Vec<sir_core::ir_data::OperationID>> {
        let op = op.cast::<LIRIAddOp>()?;
        if !helper.is_mapped(op.get_lhs().as_id()) || !helper.is_mapped(op.get_rhs().as_id()) {
            return None;
        }
        Some(vec![op.as_id()])
    }

    fn rewrite_op(
        &self,
        helper: &mut ISelDAGHelper,
        rewriter: &mut IRRewriter,
        op: sir_core::ir_data::OperationID,
    ) {
        let op = rewriter.get_operation(op).cast::<LIRIAddOp>().unwrap();
        let lhs_reg = helper.get_reg(op.get_lhs().as_id()).unwrap().clone();
        let rhs_reg = helper.get_reg(op.get_rhs().as_id()).unwrap().clone();
        let out_reg = map_new_virtual_reg(helper, op.get_result());
        rewriter.create_op(op.loc(), ArmAddIns::build(out_reg, lhs_reg, rhs_reg));
    }
}

// convert return(...) => arm.ret
struct ConvertReturnOp;

impl DAGPattern<ISelDAGHelper> for ConvertReturnOp {
    fn get_cost(&self) -> usize {
        0
    }

    fn match_op(
        &self,
        _helper: &ISelDAGHelper,
        op: sir_core::operation::GenericOperation,
    ) -> Option<Vec<sir_core::ir_data::OperationID>> {
        let op = op.cast::<ReturnOp>()?;
        Some(vec![op.as_id()])
    }

    fn rewrite_op(
        &self,
        _helper: &mut ISelDAGHelper,
        rewriter: &mut IRRewriter,
        op: sir_core::ir_data::OperationID,
    ) {
        let op = rewriter.get_operation(op).cast::<ReturnOp>().unwrap();
        rewriter.create_op(op.loc(), ArmRetIns::build());
    }
}

/// Register all transforms needed
pub fn register_isel_patterns(ctx: &mut IRContext) {
    sir_transform::context_registry::ContextRegistry::exec_register_fn(
        ctx,
        "__sir/backends/register_armv86a_isel_patterns",
        |mut registry| {
            registry.register_pass_config_callback(
                InstructionSelectionPass::get_pass_name(),
                |opts: &mut InstructionSelectionOptions| {
                    opts.patterns.add(ConvertFunctionOp);
                    opts.patterns.add(ConvertStoreAlloca);
                    opts.patterns.add(ConvertLoadAlloca);
                    opts.patterns.add(ConvertIadd);
                    opts.patterns.add(ConvertReturnOp);
                },
            );
        },
    );
}
