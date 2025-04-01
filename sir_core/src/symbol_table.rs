use std::collections::HashMap;

use crate::{
    ir_data::OperationID,
    op_interfaces::SymbolOp,
    operation::{GenericOperation, OperationImpl},
};

// A SymbolTable is used to keep tracks of all defined symbols in the IR.
pub struct SymbolTable {
    symbols: HashMap<String, OperationID>,
}

impl SymbolTable {
    // Create a new empty symbol table.
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }

    // Create a new symbol table filled with all children of `parent_op`.
    pub fn make_from_op(parent_op: GenericOperation) -> Self {
        let mut table = SymbolTable::new();
        table.fill(parent_op);
        table
    }

    // Go through all children of parent_op, and add them to the map.
    pub fn fill(&mut self, parent_op: GenericOperation) {
        for block in parent_op.get_blocks() {
            for op in block.get_ops() {
                if let Some(sym_op) = op.get_interface::<SymbolOp>() {
                    self.insert_symbol(sym_op);
                }
            }
        }
    }

    // Add the symbol to the map.
    // Returns True of the op was added.
    // Or false if it's already in the map.
    pub fn insert_symbol(&mut self, sym: SymbolOp) -> bool {
        self.symbols
            .insert(sym.get_symbol_name().to_owned(), sym.as_id())
            .is_none()
    }

    // Look for symbol `name`.
    // Returns None if not found.
    pub fn find(&mut self, name: &str) -> Option<OperationID> {
        self.symbols.get(name).map(|x| *x)
    }
}
