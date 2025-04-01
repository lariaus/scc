use std::{collections::HashMap, fs::File};

use crate::location::RawFileIdentifier;

use crate::source_stream::{FileSourceStream, SourceStream, StringSourceStream};

enum SourceData {
    File(String),
    RawCode(String),
}

// Class to keep track of all the source files used in the program.
// Helps with keeping of all source files around without having them open.
pub struct SourceStreamsSet {
    files: HashMap<RawFileIdentifier, SourceData>,
}

impl SourceStreamsSet {
    // Create a new empty set.
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
        }
    }

    // Create a set with a single raw_source_string
    pub fn with_unique_raw_source_string(code: String) -> Self {
        let mut res = Self::new();
        res.add_raw_source_string(code);
        res
    }

    // Add a new input stream from a raw string of source code.
    pub fn add_raw_source_string(&mut self, code: String) -> RawFileIdentifier {
        let uid = RawFileIdentifier(self.files.len() as u64);
        self.files.insert(uid, SourceData::RawCode(code));
        uid
    }

    // Add a new input stream from a file path.
    pub fn add_source_file(&mut self, path: &str) -> RawFileIdentifier {
        let uid = RawFileIdentifier(self.files.len() as u64);
        self.files.insert(uid, SourceData::File(path.to_string()));
        uid
    }

    // Open the stream matching with this file identifier.
    pub fn open_stream(&self, fid: RawFileIdentifier) -> Box<dyn SourceStream> {
        let data = self.files.get(&fid).expect("Invalid file identifier");
        match data {
            SourceData::File(path) => Box::new(FileSourceStream::new(
                File::open(path).expect("Failed to open input file"),
                fid,
            )),
            SourceData::RawCode(code) => Box::new(StringSourceStream::from_str(code, fid)),
        }
    }

    // Open the stream matching with this file identifier.
    pub fn open_raw_source_stream(&self, fid: RawFileIdentifier) -> StringSourceStream {
        let data = self.files.get(&fid).expect("Invalid file identifier");
        match data {
            SourceData::File(_) => panic!("Expected RawCode but got file"),
            SourceData::RawCode(code) => StringSourceStream::from_str(code, fid),
        }
    }

    // Open the first stream.
    pub fn open_main_stream(&self) -> Box<dyn SourceStream> {
        self.open_stream(RawFileIdentifier(0))
    }

    // Get the label of a specific file
    pub fn get_file_label(&self, fid: RawFileIdentifier) -> &str {
        let data = self.files.get(&fid).expect("Invalid FileIdentifier");
        match data {
            SourceData::File(path) => path,
            SourceData::RawCode(_) => "(source)",
        }
    }
}
