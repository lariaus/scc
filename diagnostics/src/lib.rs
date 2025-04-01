use std::io::stderr;
use std::process::exit;

use iostreams::location::{Location, RawFilePosition};
use iostreams::source_stream::SourceStream;
use iostreams::source_streams_set::SourceStreamsSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DiagnosticType {
    Info,
    Warning,
    Error,
}

impl DiagnosticType {
    fn str_rep(&self) -> &'static str {
        match self {
            Self::Info => "Info",
            Self::Warning => "Warning",
            Self::Error => "Error",
        }
    }
}

struct DiagnosticData {
    kind: DiagnosticType,
    emitter_name: String,
    message: String,
    location: Location,
}

// List of Diagnostic messages of a compiler.
// This class is used to emit messages / errors.
pub struct CompilerDiagnostics {
    data: Vec<DiagnosticData>,
    // If true, print simpler logs.
    reduced_log: bool,
    // Stop logging more errors after logging the first one.
    log_only_one_error: bool,
}

// Trait that must be implemented by all objects that can emit diagnostics
pub trait CompilerDiagnosticsEmitter: Sized {
    fn get_diagnostics_emitter_mut(&mut self) -> &mut CompilerDiagnostics;
    fn get_diagnostics_emitter_name(&self) -> &str;

    fn take_diagnostics(&mut self) -> CompilerDiagnostics {
        let mut res = CompilerDiagnostics::new();
        std::mem::swap(&mut res, self.get_diagnostics_emitter_mut());
        res
    }

    fn extend_diagnostics(&mut self, other: CompilerDiagnostics) {
        let mut other = other;
        self.get_diagnostics_emitter_mut()
            .data
            .append(&mut other.data)
    }
}

impl CompilerDiagnostics {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            reduced_log: false,
            log_only_one_error: false,
        }
    }

    // Enable reduced log mode, only needed for checking the logs.
    pub fn enable_reduced_log(&mut self) {
        self.reduced_log = true;
    }

    // This should never be used. Use this only because parser is not good enough now to resume after error.
    pub fn enable_log_only_one_error(&mut self) {
        self.log_only_one_error = true;
    }
}

// Emit a compiler error at `location`.
pub fn emit_error<T: CompilerDiagnosticsEmitter>(
    emitter: &mut T,
    location: Location,
    message: String,
) {
    let emitter_name = emitter.get_diagnostics_emitter_name().to_string();
    let diagnostics = emitter.get_diagnostics_emitter_mut();

    if diagnostics.log_only_one_error && diagnostics._has_any_errors() {
        return;
    }

    diagnostics.data.push(DiagnosticData {
        kind: DiagnosticType::Error,
        emitter_name,
        message,
        location,
    });
}

// Emit a compiler warning at `location`.
pub fn emit_warning<T: CompilerDiagnosticsEmitter>(
    emitter: &mut T,
    location: Location,
    message: String,
) {
    let emitter_name = emitter.get_diagnostics_emitter_name().to_string();
    emitter
        .get_diagnostics_emitter_mut()
        .data
        .push(DiagnosticData {
            kind: DiagnosticType::Warning,
            emitter_name,
            message,
            location,
        });
}

// Emit a compiler info at `location`.
pub fn emit_info<T: CompilerDiagnosticsEmitter>(
    emitter: &mut T,
    location: Location,
    message: String,
) {
    let emitter_name = emitter.get_diagnostics_emitter_name().to_string();
    emitter
        .get_diagnostics_emitter_mut()
        .data
        .push(DiagnosticData {
            kind: DiagnosticType::Info,
            emitter_name,
            message,
            location,
        });
}

// Move all compiler diagnostics from src to dst.
pub fn take_diagnostics<DstT: CompilerDiagnosticsEmitter, SrcT: CompilerDiagnosticsEmitter>(
    dst: &mut DstT,
    src: &mut SrcT,
) {
    dst.get_diagnostics_emitter_mut()
        .data
        .append(&mut src.get_diagnostics_emitter_mut().data);
}

impl CompilerDiagnostics {
    fn _has_any_errors(&self) -> bool {
        for data in &self.data {
            if data.kind == DiagnosticType::Error {
                return true;
            }
        }
        false
    }

    // Print the exact content in loc, from begin to end.
    fn _print_source_at_loc<T: std::io::Write>(
        &self,
        ss: &SourceStreamsSet,
        ofs: &mut T,
        loc: Location,
    ) -> Result<(), std::io::Error> {
        let mut f = ss.open_stream(loc.file_id());
        f.set_position(loc.beg_pos());
        loop {
            let is_last = f.get_position() == loc.end_pos();

            let c = match f.getc() {
                Some(c) => c,
                None => break,
            };
            write!(ofs, "{}", c)?;

            if is_last {
                break;
            }
        }
        Ok(())
    }

    // Print the line of code at `line_pos`.
    // Also highligh the location from beg_loc to end_loc.
    // If beg_loc is none, highligh from the start of line.
    // if end_loc is none, highligh all the line.
    fn _print_highligh_line_at_loc<T: std::io::Write>(
        &self,
        ofs: &mut T,
        f: &mut dyn SourceStream,
        line_pos: RawFilePosition,
        beg_pos: Option<RawFilePosition>,
        end_pos: Option<RawFilePosition>,
    ) -> Result<(), std::io::Error> {
        let mut before_size = 0;
        let mut content_size = 0;
        let mut reached_beg = beg_pos.is_none();
        let mut reached_end = false;
        f.set_position_to_begin_of_line(line_pos);

        // Print the whole line
        loop {
            // Check for reaching beg_pos.
            if !reached_beg {
                reached_beg = f.get_position() == beg_pos.unwrap();
                if !reached_beg {
                    before_size += 1;
                }
            }
            // Check for reaching end_pos.
            if reached_beg {
                reached_end =
                    reached_end || (end_pos.is_some() && f.get_position() == end_pos.unwrap());
                if !reached_end {
                    content_size += 1;
                }
            }

            let c = match f.getc() {
                Some(c) if c != '\n' => c,
                _ => break,
            };

            write!(ofs, "{}", c)?;
        }
        write!(ofs, "\n")?;

        // Write the highlight
        for _ in 0..before_size {
            write!(ofs, " ")?;
        }
        for _ in 0..content_size + 1 {
            write!(ofs, "^")?;
        }
        write!(ofs, "\n")?;

        Ok(())
    }

    // Print all diagnostics messages to stderr.
    // It will also exit in case of error.
    pub fn resolve(&mut self, ss: &SourceStreamsSet) {
        self.resolve_with_writer(ss, &mut stderr())
    }

    // Print all diagnostics messages to writer.
    // It will also exit in case of error.
    pub fn resolve_with_writer<W: std::io::Write>(&mut self, ss: &SourceStreamsSet, os: &mut W) {
        let has_errs = self._has_any_errors();
        self.logs_all_messages(ss, os);
        if has_errs {
            exit(1);
        }
    }

    // Print all diagnostics messages to writer.
    // Returns true if any error was found.
    pub fn check_has_any_errors<W: std::io::Write>(
        &mut self,
        ss: &SourceStreamsSet,
        os: &mut W,
    ) -> bool {
        let has_errs = self._has_any_errors();
        self.logs_all_messages(ss, os);
        has_errs
    }

    // Print all diagnostics messages to stderr.
    pub fn logs_all_messages<W: std::io::Write>(&mut self, ss: &SourceStreamsSet, os: &mut W) {
        // TODO: Consider sorting the diagnostics before printing.
        for data in &self.data {
            let loc = data.location;
            write!(
                os,
                "{}: {}: {} at {}:{}",
                data.emitter_name,
                data.kind.str_rep(),
                data.message,
                ss.get_file_label(loc.file_id()),
                data.location
            )
            .unwrap();
            if self.reduced_log {
                write!(os, ": `").unwrap();
                self._print_source_at_loc(ss, os, loc).unwrap();
                write!(os, "`\n").unwrap();
            } else if loc.beg_line() == loc.end_line() {
                write!(os, ":\n").unwrap();
                let mut f = ss.open_stream(loc.file_id());
                self._print_highligh_line_at_loc(
                    os,
                    &mut *f,
                    loc.beg_pos(),
                    Some(loc.beg_pos()),
                    Some(loc.end_pos()),
                )
                .unwrap();
            } else {
                todo!("multi-lines loc print");
            }
        }

        self.data.clear();
    }

    // Convert all log messages into a string.
    pub fn logs_to_string(&mut self, ss: &SourceStreamsSet) -> String {
        let mut buf = Vec::new();
        self.logs_all_messages(ss, &mut buf);
        String::from_utf8(buf).unwrap()
    }
}
