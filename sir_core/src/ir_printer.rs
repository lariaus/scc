use std::collections::{HashMap, HashSet};

use iostreams::output_source_stream::OutputSourceStream;

use crate::{block::Block, ir_data::ValueID};

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
    values_labels: HashMap<ValueID, String>,
    values_labels_set: HashSet<String>,
    use_generic_form: bool,
}

impl IRPrinter {
    // Create a new AST printer.
    pub fn new(opts: IRPrinterOptions, os: Box<dyn std::io::Write>) -> Self {
        // TODO: Also check the verifier to ensure we can use the generic form.
        let use_generic_form = opts.use_generic_form;
        Self {
            opts,
            os: OutputSourceStream::make_writer(os),
            ident: 0,
            values_labels: HashMap::new(),
            values_labels_set: HashSet::new(),
            use_generic_form,
        }
    }

    pub fn new_string_builder(opts: IRPrinterOptions) -> Self {
        // TODO: Also check the verifier to ensure we can use the generic form.
        let use_generic_form = opts.use_generic_form;
        Self {
            opts,
            os: OutputSourceStream::make_buffer_builder(),
            ident: 0,
            values_labels: HashMap::new(),
            values_labels_set: HashSet::new(),
            use_generic_form,
        }
    }

    pub fn os(&mut self) -> &mut dyn std::io::Write {
        self.os.os()
    }

    pub(crate) fn always_use_generic_form(&self) -> bool {
        self.use_generic_form
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

    // Print the label associated to `uid`, or <<UNKNOWN> if it's not mapped
    pub fn print_value_label_or_unknown(&mut self, uid: ValueID) -> Result<(), std::io::Error> {
        match self.values_labels.get(&uid) {
            Some(label) => write!(self.os.os(), "%{}", label),
            None => write!(self.os.os(), "<<UNKNOWN>"),
        }
    }

    // Assign a label to `uid` and then print it.
    // Panics if `uid` already has a label assigned.
    pub fn assign_and_print_value_label(
        &mut self,
        uid: ValueID,
        prefix: Option<&str>,
    ) -> Result<(), std::io::Error> {
        // Generate a unique label
        let prefix = prefix.unwrap_or("");
        // TODO: There might be something smarter than this
        let mut idx = 0;
        let unique_label = loop {
            let label = prefix.to_string() + &idx.to_string();
            idx += 1;
            if !self.values_labels_set.contains(&label) {
                break label;
            }
        };

        write!(self.os(), "%{}", &unique_label)?;

        self.values_labels_set.insert(unique_label.clone());
        match self.values_labels.insert(uid, unique_label) {
            Some(_old) => panic!("Redefinition of label for `{:?}`", uid),
            None => {}
        }

        Ok(())
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
        obj.print(self)
    }
}

// Implement this trait for an object to be printable in the IR.
pub trait IRPrintableObject: Sized {
    // Print object to the IR
    fn print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error>;

    // Returns the string IR for the following object
    fn to_string_repr_with_opts(&self, opts: IRPrinterOptions) -> String {
        let mut printer = IRPrinter::new_string_builder(opts);
        printer.print(self).unwrap();
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
