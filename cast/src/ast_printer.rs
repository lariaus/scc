use crate::ast::{ASTNode, ASTNodeImpl};

// Options used to config AST printing.
pub struct ASTPrinterOptions {
    ident_size: usize,
}

impl ASTPrinterOptions {
    // Create a printer with default options.
    pub fn new() -> Self {
        Self { ident_size: 4 }
    }
}

// Base class used to print the AST
pub struct ASTPrinter<'a, OS: std::io::Write> {
    opts: ASTPrinterOptions,
    os: &'a mut OS,
    ident: usize,
}

impl<'a, OS: std::io::Write> ASTPrinter<'a, OS> {
    // Create a new AST printer.
    pub fn new(opts: ASTPrinterOptions, os: &'a mut OS) -> Self {
        Self { opts, os, ident: 0 }
    }

    pub(crate) fn os(&mut self) -> &mut OS {
        self.os
    }

    pub fn opts(&self) -> &ASTPrinterOptions {
        &self.opts
    }

    pub(crate) fn inc_indent(&mut self) {
        self.ident += 1;
    }

    pub(crate) fn dec_indent(&mut self) {
        assert!(self.ident > 0);
        self.ident -= 1;
    }

    pub(crate) fn newline(&mut self) -> Result<(), std::io::Error> {
        write!(self.os(), "\n")?;
        let num_spaces = self.ident * self.opts.ident_size;
        for _ in 0..num_spaces {
            write!(self.os(), " ")?;
        }
        Ok(())
    }

    pub(crate) fn nl_dec_indent(&mut self) -> Result<(), std::io::Error> {
        self.dec_indent();
        self.newline()
    }

    // Print `node`.
    pub fn print(&mut self, node: &ASTNode) -> Result<(), std::io::Error> {
        node.print(self)
    }
}
