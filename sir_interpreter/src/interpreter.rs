use core::panic;

use sir_core::{
    attributes::Attribute,
    ir_context::IRContext,
    ir_data::{OperationID, ValueID},
    ir_printer::IRPrintableObject,
    op_interfaces::SymbolOp,
    operation::{GenericOperation, OperationImpl},
    symbol_table::SymbolTable,
    types::FunctionType,
    value::Value,
};
use utils::scoped_map::ScopedMap;

use crate::interfaces::{InterpretableComputeOp, InterpretableOp};

// Represent the program counter object
enum ProgramCounter {
    AtOp(OperationID),
    End,
    Error,
}

impl ProgramCounter {
    pub fn is_end(&self) -> bool {
        match self {
            ProgramCounter::End => true,
            _ => false,
        }
    }
}

// Basic class to interpret code / functions using the interpreter.
pub struct SIRInterpreter<'a> {
    ctx: &'a IRContext,
    syms: SymbolTable,
    vals: ScopedMap<ValueID, Attribute>,
    fun_inputs: Option<Vec<Attribute>>,
    fun_outputs: Option<Vec<Attribute>>,
    rets_stack: Vec<ProgramCounter>,
    pc: ProgramCounter,
}

impl<'a> SIRInterpreter<'a> {
    // Create a new interpreter from the root op of the IR.
    pub fn new(root: GenericOperation<'a>) -> Self {
        Self {
            ctx: root.get_context(),
            syms: SymbolTable::make_from_op(root),
            vals: ScopedMap::new(),
            fun_inputs: None,
            fun_outputs: None,
            rets_stack: Vec::new(),
            pc: ProgramCounter::Error,
        }
    }

    // Get the type of a function, or returns None if not found.
    pub fn get_function_type(&mut self, symbol_name: &str) -> Option<&'a FunctionType> {
        let fun = self.syms.find(symbol_name)?;
        let fun = self
            .ctx
            .get_generic_operation(fun)
            .get_interface::<SymbolOp>()
            .expect("should be a a symbol");
        let fun_type = fun
            .get_symbol_type()
            .cast::<FunctionType>()
            .expect("Symbol should have a function type");
        Some(fun_type)
    }

    // Get the input values of a function call.
    // Panic if there is no function call.
    pub fn take_function_inputs(&mut self) -> Vec<Attribute> {
        let mut inputs = None;
        std::mem::swap(&mut self.fun_inputs, &mut inputs);
        inputs.expect("No function inputs found")
    }

    // Set the input values of a function call.
    pub fn set_function_inputs(&mut self, inputs: Vec<Attribute>) {
        assert!(self.fun_inputs.is_none(), "function inputs must be none");
        self.fun_inputs = Some(inputs);
    }

    // Open a new scope for the values.
    pub fn open_values_scope(&mut self) {
        self.vals.open_scope();
    }

    // Close the op scope for the values.
    pub fn close_values_scope(&mut self) {
        self.vals.close_scope();
    }

    // Get the outputs values of a function call.
    // Panic if there is no function call.
    pub fn take_function_outputs(&mut self) -> Vec<Attribute> {
        let mut outputs = None;
        std::mem::swap(&mut self.fun_outputs, &mut outputs);
        outputs.expect("No function outputs found")
    }

    // Set the outputs values of a function call.
    pub fn set_function_outputs(&mut self, outputs: Vec<Attribute>) {
        assert!(self.fun_outputs.is_none(), "function outputs must be none");
        self.fun_outputs = Some(outputs);
    }

    pub fn interpret_function(
        &mut self,
        symbol_name: &str,
        inputs: Vec<Attribute>,
    ) -> Vec<Attribute> {
        let fun = self
            .syms
            .find(symbol_name)
            .expect(&format!("Function `{}` not found", symbol_name));

        // Prepare the interpreter
        self._reset_interpreter();
        self.rets_stack.push(ProgramCounter::End);
        self.pc = ProgramCounter::AtOp(fun);

        // Set the inputs
        self.set_function_inputs(inputs);

        // Loop until the end of the execution
        while !self.pc.is_end() {
            self.step();
        }

        // Return the outputs
        self.take_function_outputs()
    }

    pub fn assign_value(&mut self, val: Value, attr: Attribute) {
        let attr_ty = attr.get_type().expect("Value must be typed");
        if attr_ty != val.get_type() {
            panic!(
                "Invalid asignment to `{}`: value has type {} but attribute type is {}",
                val.to_string_repr(),
                val.get_type().to_string_repr(),
                attr_ty.to_string_repr()
            );
        }
        self.vals.insert(val.as_id(), attr);
    }

    pub fn get_value(&self, val: Value) -> &Attribute {
        self.vals.get(&val.as_id()).expect("Undefined value")
    }

    // Interpret the operation.
    pub fn interpret_op(&mut self, op: GenericOperation<'a>) {
        // Run with InterpretableComputeOp interface.
        if let Some(compute_op) = op.get_interface::<InterpretableComputeOp>() {
            let inputs: Vec<&Attribute> = op.get_inputs().map(|x| self.get_value(x)).collect();
            let outputs = compute_op.interpret(&inputs[..]);
            assert_eq!(outputs.len(), op.get_num_outputs());
            for (output_val, output_attr) in op.get_outputs().zip(outputs) {
                self.assign_value(output_val, output_attr);
            }
            self.move_pc_to_next_op();
        } else if let Some(interpret_op) = op.get_interface::<InterpretableOp>() {
            interpret_op.interpret(self);
        } else {
            panic!(
                "Unsupported op cannot be interpreted: {}",
                op.to_string_repr()
            );
        }
    }

    // Move program counter to the next operation.
    pub fn move_pc_to_next_op(&mut self) {
        let op = match self.pc {
            ProgramCounter::AtOp(op) => op,
            ProgramCounter::End => panic!("Program execution already finished"),
            ProgramCounter::Error => panic!("Invalid PC (usually when went past end of block"),
        };

        // Find the next op.
        let op = self.ctx.get_generic_operation(op);
        let next_op = op.get_op_after();
        self.pc = if let Some(next_op) = next_op {
            ProgramCounter::AtOp(next_op.as_id())
        } else {
            ProgramCounter::Error
        };
    }

    // Set the program counter (next executed operation) to op.
    pub fn set_pc_to_op(&mut self, op: OperationID) {
        self.pc = ProgramCounter::AtOp(op)
    }

    // Push an operation to the call stack, PC will be set to it after returning from the call.
    pub fn push_to_callstack(&mut self, op: OperationID) {
        self.rets_stack.push(ProgramCounter::AtOp(op));
    }

    // Push the next operation to the call stack, PC will be set to it after returning from the call.
    pub fn push_next_op_to_callstack(&mut self) {
        let op = match self.pc {
            ProgramCounter::AtOp(op) => op,
            ProgramCounter::End => panic!("Program execution already finished"),
            ProgramCounter::Error => panic!("Invalid PC (usually when went past end of block"),
        };

        // Find the next op.
        let op = self.ctx.get_generic_operation(op);
        let next_op = op.get_op_after();
        let pc = if let Some(next_op) = next_op {
            ProgramCounter::AtOp(next_op.as_id())
        } else {
            ProgramCounter::Error
        };
        self.rets_stack.push(pc);
    }

    // Pop the top of the callstack and update the PC.
    pub fn pop_callstack(&mut self) {
        self.pc = self.rets_stack.pop().expect("callstack is empty");
    }

    // Execute the next instruction.
    pub fn step(&mut self) {
        let op = match self.pc {
            ProgramCounter::AtOp(op) => op,
            ProgramCounter::End => panic!("Program execution already finished"),
            ProgramCounter::Error => panic!("Invalid PC (usually when went past end of block"),
        };

        // Interpret the op.
        let op = self.ctx.get_generic_operation(op);
        self.interpret_op(op);
    }

    fn _reset_interpreter(&mut self) {
        self.fun_inputs = None;
        self.fun_outputs = None;
        self.vals.clear();
        self.rets_stack.clear();
        self.pc = ProgramCounter::Error;
    }
}
