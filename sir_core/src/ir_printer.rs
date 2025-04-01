use std::collections::HashMap;

use iostreams::output_source_stream::OutputSourceStream;

use crate::{
    block::Block,
    ir_context::IRContext,
    ir_data::{BlockID, OperationID, ValueID},
    ir_verifier::{verify_block_with_options, verify_op_with_options, IRVerifierOptions},
    operation::{GenericOperation, OperationImpl},
};

#[derive(Clone, Copy)]
pub(crate) struct ValueDefInfo {
    is_arg: bool,
    idx: usize,
}

// Helper class to keep track of the values defined in a scope.
struct ValueDefsScope {
    defs: HashMap<ValueID, ValueDefInfo>,
    values_count: usize,
    args_count: usize,
}

// Helper class to keep track of the values defined in a scope.
pub(crate) struct ValueDefsMap {
    scopes: Vec<ValueDefsScope>,
    next_value_idx: usize,
    next_arg_idx: usize,
}

// Helper class used to fill the map with predecessors info.
// It indicates when we reached the root and can stop.
#[derive(Clone, Copy)]
enum PredecessorsTarget {
    Op(OperationID),
    Block(BlockID),
}

impl PredecessorsTarget {
    fn is_target_op(&self, op: GenericOperation) -> bool {
        match self {
            PredecessorsTarget::Op(uid) => op.as_id() == *uid,
            _ => false,
        }
    }

    fn is_target_block(&self, block: Block) -> bool {
        match self {
            PredecessorsTarget::Block(uid) => block.as_id() == *uid,
            _ => false,
        }
    }
}

impl ValueDefsMap {
    pub(crate) fn new() -> Self {
        Self {
            scopes: Vec::new(),
            next_value_idx: 0,
            next_arg_idx: 0,
        }
    }

    pub(crate) fn open_scope(&mut self) {
        self.scopes.push(ValueDefsScope {
            defs: HashMap::new(),
            values_count: 0,
            args_count: 0,
        });
    }

    pub(crate) fn close_scope(&mut self) {
        let scope = self.scopes.pop().unwrap();
        self.next_value_idx -= scope.values_count;
        self.next_arg_idx -= scope.args_count;
    }

    // Returns true if the value is in the map.
    pub(crate) fn contains_def(&self, val: ValueID) -> bool {
        self.scopes
            .iter()
            .any(|scope| scope.defs.contains_key(&val))
    }

    fn _find_def(&self, val: ValueID) -> Option<ValueDefInfo> {
        for scope in &self.scopes {
            if let Some(val) = scope.defs.get(&val) {
                return Some(*val);
            }
        }
        None
    }

    // Returns the name of the defined value for the IRPrinter.
    // Returns None if not found in the map.
    pub(crate) fn get_def_name(&self, val: ValueID) -> Option<String> {
        let def = self._find_def(val)?;
        if def.is_arg {
            Some(format!("arg{}", def.idx))
        } else {
            Some(format!("{}", def.idx))
        }
    }

    // Insert a new value in the map.
    pub(crate) fn insert_op_out_def(&mut self, val: ValueID) {
        assert!(!self.contains_def(val));
        let scope = self.scopes.last_mut().unwrap();

        let idx = self.next_value_idx;
        self.next_value_idx += 1;

        scope.defs.insert(val, ValueDefInfo { is_arg: false, idx });
        scope.values_count += 1;
    }

    // Insert a new value in the map.
    pub(crate) fn insert_block_arg_def(&mut self, val: ValueID) {
        assert!(!self.contains_def(val));
        let scope = self.scopes.last_mut().unwrap();

        let idx = self.next_arg_idx;
        self.next_arg_idx += 1;

        scope.defs.insert(val, ValueDefInfo { is_arg: true, idx });
        scope.args_count += 1;
    }

    // Fill the map with all the defs of the predecessors of op.
    // This is usefull if you want to do some analysis of op, but still need access to the context around it.
    pub(crate) fn fill_with_predecessors_defs(&mut self, op: GenericOperation) {
        self._fill_with_predecessors_defs(op.get_context(), PredecessorsTarget::Op(op.as_id()));
    }

    // Fill the map with all the defs of the predecessors of block.
    // This is usefull if you want to do some analysis of block, but still need access to the context around it.
    pub(crate) fn fill_with_predecessors_defs_block(&mut self, block: Block) {
        self._fill_with_predecessors_defs(block.ctx(), PredecessorsTarget::Block(block.as_id()));
    }

    fn _fill_with_predecessors_defs(&mut self, ctx: &IRContext, target: PredecessorsTarget) {
        // Find the root op or block of target.

        // Start by initializing it from target.
        let mut root_op = None;
        let mut root_block = None;
        match target {
            PredecessorsTarget::Block(block) => {
                let block = ctx.get_block(block);
                if let Some(parent) = block.parent() {
                    root_op = Some(parent);
                } else {
                    root_block = Some(block);
                }
            }
            PredecessorsTarget::Op(op) => {
                root_op = Some(ctx.get_generic_operation(op));
            }
        }

        // Now go up until finding the root op or block of the IR.
        while !root_op.is_none() {
            let parent = match root_op.unwrap().parent() {
                Some(block) => block,
                None => break,
            };
            match parent.parent() {
                Some(op) => {
                    root_op = Some(op);
                }
                None => {
                    root_op = None;
                    root_block = Some(parent);
                }
            }
        }

        if let Some(op) = root_op {
            self._fill_with_predecessors_op_rec(op, target);
        } else if let Some(block) = root_block {
            self._fill_with_predecessors_block_rec(block, target);
        } else {
            unreachable!("should have a root");
        }
    }

    fn _fill_with_predecessors_op_rec(
        &mut self,
        op: GenericOperation,
        target: PredecessorsTarget,
    ) -> bool {
        // @TODO[I0][SIR-CORE]: ValuesDefsScope: optimize `fill_with_predecessors_defs`

        // Stop once reaching the initial op.
        if target.is_target_op(op) {
            return true;
        }

        // Go through the blocks first.
        for block in op.get_blocks() {
            if self._fill_with_predecessors_block_rec(block, target) {
                return true;
            }
        }

        // Define the op outputs.
        for val in op.get_outputs() {
            self.insert_op_out_def(val.as_id());
        }

        false
    }

    fn _fill_with_predecessors_block_rec(
        &mut self,
        block: Block,
        target: PredecessorsTarget,
    ) -> bool {
        // @TODO[I0][SIR-CORE]: ValuesDefsScope: optimize `fill_with_predecessors_defs`

        // Stop once reaching the initial block.
        if target.is_target_block(block) {
            return true;
        }

        // Define the block arguments.
        self.open_scope();
        for arg in block.get_operands() {
            self.insert_block_arg_def(arg.as_id());
        }

        // Go through all ops.
        for op in block.get_ops() {
            if self._fill_with_predecessors_op_rec(op, target) {
                return true;
            }
        }

        self.close_scope();
        false
    }
}

// Options used to config IR printing.
pub struct IRPrinterOptions {
    pub ident_size: usize,
    pub use_generic_form: bool,
}

impl IRPrinterOptions {
    // Create the default options.
    pub fn new() -> Self {
        Self {
            ident_size: 4,
            use_generic_form: false,
        }
    }
}

// Base class used to print the AST
pub struct IRPrinter {
    opts: IRPrinterOptions,
    os: OutputSourceStream,
    ident: usize,
    value_defs: ValueDefsMap,
    context_initialized: bool,
}

impl IRPrinter {
    // Create a new AST printer.
    pub fn new(opts: IRPrinterOptions, os: Box<dyn std::io::Write>) -> Self {
        Self {
            opts,
            os: OutputSourceStream::make_writer(os),
            ident: 0,
            value_defs: ValueDefsMap::new(),
            context_initialized: false,
        }
    }

    pub fn new_string_builder(opts: IRPrinterOptions) -> Self {
        // TODO: Also check the verifier to ensure we can use the generic form.
        Self {
            opts,
            os: OutputSourceStream::make_buffer_builder(),
            ident: 0,
            value_defs: ValueDefsMap::new(),
            context_initialized: false,
        }
    }

    pub fn os(&mut self) -> &mut dyn std::io::Write {
        self.os.os()
    }

    pub(crate) fn always_use_generic_form(&self) -> bool {
        self.opts.use_generic_form
    }

    pub fn opts(&self) -> &IRPrinterOptions {
        &self.opts
    }

    pub fn newline(&mut self) -> Result<(), std::io::Error> {
        write!(self.os(), "\n")?;
        let num_spaces = self.ident * self.opts.ident_size;
        for _ in 0..num_spaces {
            write!(self.os(), " ")?;
        }
        Ok(())
    }

    pub fn inc_indent(&mut self) {
        self.ident += 1;
    }

    pub fn nl_inc_indent(&mut self) -> Result<(), std::io::Error> {
        self.inc_indent();
        self.newline()
    }

    pub fn dec_indent(&mut self) {
        assert!(self.ident > 0);
        self.ident -= 1;
    }

    pub fn nl_dec_indent(&mut self) -> Result<(), std::io::Error> {
        self.dec_indent();
        self.newline()
    }

    // Call this to if the root object is an operation.
    pub fn initialize_context_with_root_op(&mut self, op: GenericOperation) {
        assert!(!self.context_initialized, "context already initialized");
        self.context_initialized = true;

        self.value_defs.open_scope();
        self.value_defs.fill_with_predecessors_defs(op);

        if !self.opts.use_generic_form {
            // Force use of the generic form if the op is invalid
            // We always allow unregistered ops.
            // It doesn't matter that much here since we're just printing.
            let mut verif_opts = IRVerifierOptions::new();
            verif_opts.allow_unregistered_ops = true;
            if verify_op_with_options(op, verif_opts).has_errors() {
                self.opts.use_generic_form = true;
            }
        }
    }

    // Call this to if the root object is a block.
    pub fn initialize_context_with_root_block(&mut self, block: Block) {
        assert!(!self.context_initialized, "context already initialized");
        self.context_initialized = true;

        self.value_defs.open_scope();
        self.value_defs.fill_with_predecessors_defs_block(block);

        if !self.opts.use_generic_form {
            // Force use of the generic form if the block is invalid
            // We always allow unregistered ops.
            // It doesn't matter that much here since we're just printing.
            let mut verif_opts = IRVerifierOptions::new();
            verif_opts.allow_unregistered_ops = true;
            if verify_block_with_options(block, verif_opts).has_errors() {
                self.opts.use_generic_form = true;
            }
        }
    }

    // Initialize the context without any specific informations.
    pub fn initialize_context(&mut self) {
        assert!(!self.context_initialized, "context already initialized");
        self.context_initialized = true;
    }

    // Call this when starting to print a block.
    pub fn start_printing_block(&mut self) {
        self.value_defs.open_scope();
    }

    // Call this after finishing to print a block.
    pub fn end_printing_block(&mut self) {
        self.value_defs.close_scope();
    }

    // Print the label associated to `uid`, or <<UNKNOWN> if it's not mapped
    pub fn print_value_label_or_unknown(&mut self, uid: ValueID) -> Result<(), std::io::Error> {
        match self.value_defs.get_def_name(uid) {
            Some(label) => write!(self.os.os(), "%{}", label),
            None => write!(self.os.os(), "<<UNKNOWN>"),
        }
    }

    // Assign a label to `uid` and then print it.
    // Panics if `uid` already has a label assigned.
    pub fn assign_and_print_value_label(
        &mut self,
        uid: ValueID,
        // TODO: We could use a &'static prefix instead
        is_block_arg: bool,
    ) -> Result<(), std::io::Error> {
        // Add it to the map.
        if is_block_arg {
            self.value_defs.insert_block_arg_def(uid);
        } else {
            self.value_defs.insert_op_out_def(uid);
        }

        // Then get and print the label.
        let label = self.value_defs.get_def_name(uid).unwrap();
        write!(self.os(), "%{}", label)
    }

    // Do indent + newline + print_all_ops + reverse indent + newline.
    pub fn print_block_content(&mut self, block: Block) -> Result<(), std::io::Error> {
        self.nl_inc_indent()?;
        for (idx, op) in block.get_ops().enumerate() {
            self.print(&op)?;
            if idx + 1 == block.get_num_ops() {
                self.dec_indent();
            }
            self.newline()?;
        }

        Ok(())
    }

    // Take the buffer built by the writer if there is one.
    pub fn take_output_buffer(self) -> Option<Vec<u8>> {
        self.os.take_output_buffer()
    }

    // Take the string built by the writer if there is one.
    // Panics if the output isn't UTF-8.
    pub fn take_output_string(self) -> Option<String> {
        self.os.take_output_string()
    }

    // Print `obj`.
    pub fn print<T: IRPrintableObject>(&mut self, obj: &T) -> Result<(), std::io::Error> {
        assert!(
            self.context_initialized,
            "root context not initialized: You should start by calling print_root"
        );
        obj.print(self)
    }

    // Print `obj`
    pub fn print_root<T: IRPrintableObject>(&mut self, obj: &T) -> Result<(), std::io::Error> {
        assert!(
            !self.context_initialized,
            "print_root must be called only once"
        );
        obj.initialize_context_on_root(self);
        self.print(obj)
    }

    // Print an op using the generic form.
    // Must be called for custom_print implementation of ops don't have a custom printer
    pub fn print_op_generic_form_impl(
        &mut self,
        op: GenericOperation,
    ) -> Result<(), std::io::Error> {
        // Print outputs and opname
        self.print_op_results_and_opname(op, false)?;

        // Print op inputs
        write!(self.os.os(), "(")?;
        for (idx, input) in op.get_inputs().enumerate() {
            self.print_value_label_or_unknown(input.as_id())?;
            if idx + 1 < op.get_num_inputs() {
                write!(self.os.os(), ", ")?;
            }
        }
        write!(self.os.os(), ")")?;

        // Print optional op attrs.
        if !op.get_attrs_dict().is_empty() {
            write!(self.os.os(), " ")?;
            self.print(op.get_attrs_dict())?;
        }

        // Print the signature
        write!(self.os.os(), " : (")?;
        for (idx, input) in op.get_inputs().enumerate() {
            self.print(input.get_type())?;
            if idx + 1 < op.get_num_inputs() {
                write!(self.os.os(), ", ")?;
            }
        }
        write!(self.os.os(), ") -> (")?;
        for (idx, output) in op.get_outputs().enumerate() {
            self.print(output.get_type())?;
            if idx + 1 < op.get_num_outputs() {
                write!(self.os.os(), ", ")?;
            }
        }
        write!(self.os.os(), ")")?;

        // Print the blocks
        if op.get_num_blocks() > 0 {
            write!(self.os.os(), " {{")?;
            self.nl_inc_indent()?;
            for block in op.get_blocks() {
                self.print(&block)?;
                self.newline()?;
            }
            self.nl_dec_indent()?;
            write!(self.os.os(), "}}")?;
        }

        Ok(())
    }

    /// Print the op results and its opname.
    /// Should be called by custom printer of ops, to start printing.
    pub fn print_op_results_and_opname(
        &mut self,
        op: GenericOperation,
        use_custom_form: bool,
    ) -> Result<(), std::io::Error> {
        // Print optional op outputs.
        if op.get_num_outputs() > 0 {
            for (idx, input) in op.get_outputs().enumerate() {
                self.assign_and_print_value_label(input.as_id(), false)?;
                if idx + 1 < op.get_num_outputs() {
                    write!(self.os.os(), ", ")?;
                }
            }
            write!(self.os.os(), " = ")?;
        }

        // Print opname.
        if use_custom_form {
            write!(self.os.os(), "{} ", op.opname())
        } else {
            write!(self.os.os(), "\"{}\"", op.opname())
        }
    }
}

// Implement this trait for an object to be printable in the IR.
pub trait IRPrintableObject: Sized {
    // Print object to the IR
    fn print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error>;

    // Initialize the IRPrinter context from the root object.
    // Few objects need this, for example to support printing a subgraph of the IR and access parent information.
    fn initialize_context_on_root(&self, printer: &mut IRPrinter) {
        printer.initialize_context();
    }

    // Returns the string IR for the following object
    fn to_string_repr_with_opts(&self, opts: IRPrinterOptions) -> String {
        let mut printer = IRPrinter::new_string_builder(opts);
        printer.print_root(self).unwrap();
        printer.take_output_string().unwrap()
    }

    // Returns the string IR for the following object
    fn to_string_repr(&self) -> String {
        self.to_string_repr_with_opts(IRPrinterOptions::new())
    }

    // Returns the string IR for the following object in generic form.
    fn to_generic_form_string_repr(&self) -> String {
        let mut opts = IRPrinterOptions::new();
        opts.use_generic_form = true;
        self.to_string_repr_with_opts(opts)
    }
}
