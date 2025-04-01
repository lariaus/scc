use iostreams::location::{Location, RawFileIdentifier, RawFilePosition};
use iostreams::source_stream::SourceStream;
use iostreams::source_streams_set::SourceStreamsSet;

// This struct is implemented for objects that have a location, and an optional "string representation".
pub trait LocatableObject {
    fn get_location(&self) -> Location;
    fn get_string_repr(&self) -> Option<String>;
}

impl LocatableObject for Location {
    fn get_location(&self) -> Location {
        *self
    }

    fn get_string_repr(&self) -> Option<String> {
        None
    }
}

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

// Extra data that can be attached to a diagnostic.
struct DiagnosticAttachedData {
    message: String,
    obj_location: Location,
    obj_repr: Option<String>,
}

// Diagnostic object created by emit_... functions.
pub struct DiagnosticData {
    kind: DiagnosticType,
    emitter_name: String,
    message: String,
    obj_location: Location,
    obj_repr: Option<String>,

    extra_data: Vec<DiagnosticAttachedData>,
}

impl DiagnosticData {
    // Attach more informations to a diagnostic message.
    pub fn attach_infos<Obj: LocatableObject>(&mut self, obj: &Obj, message: String) {
        self.extra_data.push(DiagnosticAttachedData {
            message,
            obj_location: obj.get_location(),
            obj_repr: obj.get_string_repr(),
        });
    }
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

pub struct DiagnosticsEmitter<'a> {
    diagnostics: &'a mut CompilerDiagnostics,
    name: &'a str,
}

impl<'a> DiagnosticsEmitter<'a> {
    pub fn new(diagnostics: &'a mut CompilerDiagnostics, name: &'a str) -> Self {
        Self { diagnostics, name }
    }
}

impl<'a> CompilerDiagnosticsEmitter for DiagnosticsEmitter<'a> {
    fn get_diagnostics_emitter_mut(&mut self) -> &mut CompilerDiagnostics {
        self.diagnostics
    }

    fn get_diagnostics_emitter_name(&self) -> &str {
        self.name
    }
}

// Represent the input files of the compiler.
// This is needed to pretty print the locations.
#[derive(Clone, Copy)]
pub enum CompilerInputs<'a> {
    Sources(&'a SourceStreamsSet),
    Unknown,
}

impl<'a> CompilerInputs<'a> {
    // Returns the label of a file.
    fn get_file_label(&self, fid: RawFileIdentifier) -> String {
        match self {
            Self::Sources(ss) => ss.get_file_label(fid).to_owned(),
            Self::Unknown => format!("I#{}", fid.0),
        }
    }

    // Try to open the source stream of the file, or none if it's not available
    fn open_stream(&self, fid: RawFileIdentifier) -> Option<Box<dyn SourceStream>> {
        match self {
            Self::Sources(ss) => Some(ss.open_stream(fid)),
            Self::Unknown => None,
        }
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

    // Take all diagnostics from others and append it to self.
    pub fn merge_with(&mut self, mut others: CompilerDiagnostics) {
        self.data.append(&mut others.data);
    }
}

// Emit a compiler error at `location`.
pub fn emit_error<'a, T: CompilerDiagnosticsEmitter, Obj: LocatableObject>(
    emitter: &'a mut T,
    obj: &Obj,
    message: String,
) -> &'a mut DiagnosticData {
    let emitter_name = emitter.get_diagnostics_emitter_name().to_string();
    let diagnostics = &mut emitter.get_diagnostics_emitter_mut().data;

    diagnostics.push(DiagnosticData {
        kind: DiagnosticType::Error,
        emitter_name,
        message,
        obj_location: obj.get_location(),
        obj_repr: obj.get_string_repr(),
        extra_data: vec![],
    });
    diagnostics.last_mut().unwrap()
}

// Emit a compiler warning at `location`.
pub fn emit_warning<'a, T: CompilerDiagnosticsEmitter, Obj: LocatableObject>(
    emitter: &'a mut T,
    obj: &Obj,
    message: String,
) -> &'a mut DiagnosticData {
    let emitter_name = emitter.get_diagnostics_emitter_name().to_string();
    let diagnostics = &mut emitter.get_diagnostics_emitter_mut().data;

    diagnostics.push(DiagnosticData {
        kind: DiagnosticType::Warning,
        emitter_name,
        message,
        obj_location: obj.get_location(),
        obj_repr: obj.get_string_repr(),
        extra_data: vec![],
    });
    diagnostics.last_mut().unwrap()
}

// Emit a compiler info at `location`.
pub fn emit_info<'a, T: CompilerDiagnosticsEmitter, Obj: LocatableObject>(
    emitter: &'a mut T,
    obj: &Obj,
    message: String,
) -> &'a mut DiagnosticData {
    let emitter_name = emitter.get_diagnostics_emitter_name().to_string();
    let diagnostics = &mut emitter.get_diagnostics_emitter_mut().data;

    diagnostics.push(DiagnosticData {
        kind: DiagnosticType::Info,
        emitter_name,
        message,
        obj_location: obj.get_location(),
        obj_repr: obj.get_string_repr(),
        extra_data: vec![],
    });
    diagnostics.last_mut().unwrap()
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
    // Print the exact content in loc, from begin to end.
    fn _print_source_at_loc<T: std::io::Write>(
        &self,
        f: &mut dyn SourceStream,
        ofs: &mut T,
        loc: Location,
    ) -> Result<(), std::io::Error> {
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

    // Print the location source code.
    fn _print_loc_source<W: std::io::Write>(
        &self,
        inputs: CompilerInputs,
        os: &mut W,
        loc: Location,
    ) -> Result<(), std::io::Error> {
        let mut f = match inputs.open_stream(loc.file_id()) {
            Some(f) => f,
            None => {
                // No source code available, don't print it.
                return write!(os, "\n");
            }
        };

        if self.reduced_log {
            write!(os, ": `")?;
            self._print_source_at_loc(&mut *f, os, loc)?;
            write!(os, "`\n")
        } else if loc.beg_line() == loc.end_line() {
            write!(os, ":\n")?;
            self._print_highligh_line_at_loc(
                os,
                &mut *f,
                loc.beg_pos(),
                Some(loc.beg_pos()),
                Some(loc.end_pos()),
            )
        } else {
            todo!("multi-lines loc print")
        }
    }

    // Print all diagnostics messages to os.
    pub fn logs_all_messages<W: std::io::Write>(&self, inputs: CompilerInputs, os: &mut W) {
        let mut nerrors = 0;

        // TODO: Consider sorting the diagnostics before printing.
        for data in &self.data {
            if data.kind == DiagnosticType::Error {
                nerrors += 1;
                // Hack for some parser testing, only log the first error.
                if self.log_only_one_error && nerrors > 1 {
                    return;
                }
            }
            let loc = data.obj_location;
            // Print the loc infos.
            write!(
                os,
                "{}: {}: {} at {}:{}",
                data.emitter_name,
                data.kind.str_rep(),
                data.message,
                inputs.get_file_label(loc.file_id()),
                loc
            )
            .unwrap();
            // Print the loc source code.
            self._print_loc_source(inputs, os, loc).unwrap();

            // Print extra information if available.
            if let Some(extra_info) = &data.obj_repr {
                write!(os, "  See {}.\n", extra_info).unwrap();
            }

            // Print attached data.
            for extra_data in &data.extra_data {
                write!(
                    os,
                    "  {} at {}:{}",
                    extra_data.message,
                    inputs.get_file_label(extra_data.obj_location.file_id()),
                    extra_data.obj_location
                )
                .unwrap();
                self._print_loc_source(inputs, os, extra_data.obj_location)
                    .unwrap();
                // Print extra information if available.
                if let Some(extra_info) = &extra_data.obj_repr {
                    write!(os, "    See {}.\n", extra_info).unwrap();
                }
            }
        }
    }

    // Convert all log messages into a string.
    pub fn logs_to_string(&self, inputs: CompilerInputs) -> String {
        let mut buf = Vec::new();
        self.logs_all_messages(inputs, &mut buf);
        String::from_utf8(buf).unwrap()
    }

    // Clear all the existing logs data.
    pub fn clear_diagnostics(&mut self) {
        self.data.clear();
    }

    // Returns true if there is any error diagnostics.
    pub fn has_any_errors(&self) -> bool {
        for data in &self.data {
            if data.kind == DiagnosticType::Error {
                return true;
            }
        }
        false
    }

    // Retruns true if it has any diagnostic messages.
    pub fn has_any_diagnostics(&self) -> bool {
        self.data.len() > 0
    }
}

// Alias for a type that only indicates success or error.
pub type ErrorOrSuccess = Result<(), ()>;
