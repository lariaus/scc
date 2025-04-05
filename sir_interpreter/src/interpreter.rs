use std::collections::HashMap;

use sir_core::{
    attributes::Attribute,
    ir_context::IRContext,
    ir_data::{operation_id_from_opaque, operation_id_to_opaque, OperationID, ValueID},
    ir_printer::IRPrintableObject,
    op_interfaces::SymbolOp,
    operation::{GenericOperation, OperationImpl},
    symbol_table::SymbolTable,
    types::FunctionType,
    value::Value,
};

use crate::{
    interfaces::{InterpretableComputeOp, InterpretableOp},
    mem_emulator::MemEmulator,
};

const _INST_ADDRESS_RAW_INVALID: usize = usize::max_value() - 1;
const _INST_ADDRESS_RAW_END: usize = usize::max_value() - 2;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstructionAddress {
    Operation(OperationID),
    Invalid, // Indicates that the address is invalid
    End,     // Special value to indicate that it's the end of the program.
}

impl InstructionAddress {
    /// Get the associatied operation with the address.
    /// Or returns none if invalid / end.
    pub fn get_op(&self) -> Option<OperationID> {
        match self {
            InstructionAddress::Operation(op) => Some(*op),
            _ => None,
        }
    }

    /// Returns true if the instruction is invalid.
    pub fn is_invalid(&self) -> bool {
        match self {
            InstructionAddress::Invalid => true,
            _ => false,
        }
    }

    /// Returns true if the instruction is the end of the program.
    pub fn is_end(&self) -> bool {
        match self {
            InstructionAddress::End => true,
            _ => false,
        }
    }

    // Convert the instruction address to a raw value that can be stored in mem / register.
    pub fn to_raw(&self) -> usize {
        match self {
            InstructionAddress::Operation(op) => operation_id_to_opaque(*op),
            InstructionAddress::Invalid => _INST_ADDRESS_RAW_INVALID,
            InstructionAddress::End => _INST_ADDRESS_RAW_END,
        }
    }

    /// Build an instruction address from the raw value.
    pub fn from_raw(val: usize) -> Self {
        if val == _INST_ADDRESS_RAW_INVALID {
            Self::Invalid
        } else if val == _INST_ADDRESS_RAW_END {
            Self::End
        } else {
            Self::Operation(operation_id_from_opaque(val))
        }
    }
}

/// Represent the current frame of the executed function.
struct CallFrame {
    // Return address at the end of the function
    ret_addr: InstructionAddress,
    // Values of the function favariables.
    // @TODO[I10][SIR-INTERPRETER]: Use real types instead of Attributes in the interpreter.
    vals: HashMap<ValueID, Attribute>,
    // Address of the top of the stack.
    stack_top: usize,
}

// Basic class to interpret code / functions using the interpreter.
pub struct SIRInterpreter<'a> {
    ctx: &'a IRContext,
    syms: SymbolTable,
    fun_inputs: Option<Vec<Attribute>>,
    fun_outputs: Option<Vec<Attribute>>,
    mem: MemEmulator,
    calls_stack: Vec<CallFrame>,
    pc: InstructionAddress,
}

impl<'a> SIRInterpreter<'a> {
    // Create a new interpreter from the root op of the IR.
    pub fn new(root: GenericOperation<'a>) -> Self {
        Self {
            ctx: root.get_context(),
            syms: SymbolTable::make_from_op(root),
            fun_inputs: None,
            fun_outputs: None,
            mem: MemEmulator::new(),
            calls_stack: Vec::new(),
            pc: InstructionAddress::Invalid,
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

    // Returns the address of the top of the stack.
    pub fn get_stack_top_addr(&self) -> usize {
        match self.calls_stack.last() {
            Some(frame) => frame.stack_top,
            None => self.mem.stack_address(),
        }
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
        self.start_new_call_frame(InstructionAddress::Operation(fun), InstructionAddress::End);

        // Set the inputs
        self.set_function_inputs(inputs);

        // Loop until the end of the execution
        while !self.pc.is_end() {
            self.step();
        }

        // Return the outputs
        self.take_function_outputs()
    }

    pub fn set_value(&mut self, val: Value, attr: Attribute) {
        let attr_ty = attr.get_type().expect("Value must be typed");
        if attr_ty != val.get_type() {
            panic!(
                "Invalid asignment to `{}`: value has type {} but attribute type is {}",
                val.to_string_repr(),
                val.get_type().to_string_repr(),
                attr_ty.to_string_repr()
            );
        }

        let frame = self.calls_stack.last_mut().expect("no callframe open");
        frame.vals.insert(val.as_id(), attr);
    }

    pub fn get_value(&self, val: Value) -> &Attribute {
        let frame = self.calls_stack.last().expect("no callframe open");
        frame.vals.get(&val.as_id()).expect("Undefined value")
    }

    // Perform a stack allocation of size `size`.
    // Returns the address aligned by `align`.
    pub fn alloca(&mut self, size: usize, align: usize) -> usize {
        let frame = self.calls_stack.last_mut().expect("no callframe open");

        let min_align = std::mem::size_of::<usize>();
        let align = std::cmp::max(min_align, align);

        let mut addr = frame.stack_top;
        if addr % align != 0 {
            addr += align - addr % align;
        }
        frame.stack_top = addr + size;
        addr
    }

    /// Returns a mutable reference of type T from its address.
    /// Returns none if the address is invalid.
    /// The function is unsafe as we can't guarantee that the object was allocated before.
    /// Reading the reference is UB.
    pub unsafe fn get_mem_ref<T: Sized>(&self, addr: usize) -> &T {
        self.mem
            .get_ref(addr)
            .expect(&format!("Trying to access invalid address 0x{:x}", addr))
    }

    /// Version of `get_mem_ref` which is unsafe but marked as safe.
    /// SIR is an unsafe language, so obsiously interpreting it is unsafe.
    pub fn get_mem_ref_unsafe<T: Sized>(&self, addr: usize) -> &T {
        // @TODO[I9][SIR-INTERPRETER]: Make the Interpreter memory safe.
        unsafe { self.get_mem_ref::<T>(addr) }
    }

    /// Returns a mutable reference of type T from its address.
    /// Returns none if the address is invalid.
    /// The function is unsafe as we can't guarantee that the object was allocated before.
    /// Reading / Writing the reference is UB.
    pub unsafe fn get_mem_mut<T: Sized>(&mut self, addr: usize) -> &mut T {
        self.mem
            .get_mut_ref(addr)
            .expect(&format!("Trying to access invalid address 0x{:x}", addr))
    }

    /// Version of `get_mem_mut` which is unsafe but marked as safe.
    /// SIR is an unsafe language, so obsiously interpreting it is unsafe.
    pub unsafe fn get_mem_mut_unsafe<T: Sized>(&mut self, addr: usize) -> &mut T {
        // @TODO[I9][SIR-INTERPRETER]: Make the Interpreter memory safe.
        unsafe { self.get_mem_mut::<T>(addr) }
    }

    // Write `val` to memory address `addr`.
    /// Returns none if the address is invalid.
    /// Returns the number of bytes written.
    /// Differs from `self.get_mem_mut(addr) = val``, here the memory doesn't need to be initialized.
    pub fn write_mem_data<T: Sized>(&mut self, addr: usize, val: T) -> Option<usize> {
        self.mem.write_data(addr, val)
    }

    // Interpret the operation.
    pub fn interpret_op(&mut self, op: GenericOperation<'a>) {
        // Run with InterpretableComputeOp interface.
        if let Some(compute_op) = op.get_interface::<InterpretableComputeOp>() {
            let inputs: Vec<&Attribute> = op.get_inputs().map(|x| self.get_value(x)).collect();
            let outputs = compute_op.interpret(&inputs[..]);
            assert_eq!(outputs.len(), op.get_num_outputs());
            for (output_val, output_attr) in op.get_outputs().zip(outputs) {
                self.set_value(output_val, output_attr);
            }
            self.move_pc_to_next_instruction();
        } else if let Some(interpret_op) = op.get_interface::<InterpretableOp>() {
            interpret_op.interpret(self);
        } else {
            panic!(
                "Unsupported op cannot be interpreted: {}",
                op.to_string_repr()
            );
        }
    }

    /// Get the adress of the instruction after `addr`.
    pub fn get_next_ins_address(&self, addr: InstructionAddress) -> InstructionAddress {
        let op = match addr {
            InstructionAddress::Operation(op) => op,
            _ => return InstructionAddress::Invalid,
        };

        let op = self.ctx.get_generic_operation(op);
        let next_op = op.get_op_after();
        if let Some(next_op) = next_op {
            InstructionAddress::Operation(next_op.as_id())
        } else {
            InstructionAddress::Invalid
        }
    }

    /// Set the program counter to `pc`.
    pub fn set_pc(&mut self, addr: InstructionAddress) {
        self.pc = addr;
    }

    /// Move program counter to the next instruction.
    pub fn move_pc_to_next_instruction(&mut self) {
        self.pc = self.get_next_ins_address(self.pc);
    }

    // Call a new function.
    // Set the pc to `fn_start_addr`.
    // PC will be set to `ret_addr` after returning from the function.
    pub fn start_new_call_frame(
        &mut self,
        fn_start_addr: InstructionAddress,
        ret_addr: InstructionAddress,
    ) {
        let frame = CallFrame {
            ret_addr,
            vals: HashMap::new(),
            stack_top: self.get_stack_top_addr(),
        };
        self.calls_stack.push(frame);
        self.pc = fn_start_addr;
    }

    // Pop the top of the callstack and update the PC.
    pub fn pop_call_frame(&mut self) {
        let frame = self.calls_stack.pop().expect("callstack is empty");
        self.pc = frame.ret_addr;
    }

    // Execute the next instruction.
    pub fn step(&mut self) {
        let op = self.pc.get_op().expect("Current pc is invalid");

        // Interpret the op.
        let op = self.ctx.get_generic_operation(op);
        self.interpret_op(op);
    }

    fn _reset_interpreter(&mut self) {
        self.fun_inputs = None;
        self.fun_outputs = None;
        self.calls_stack.clear();
        self.pc = InstructionAddress::Invalid;
    }
}
