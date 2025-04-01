use std::{
    fs::File,
    io::{BufRead, BufReader, Seek},
};

use crate::location::{RawFileIdentifier, RawFilePosition};

// Represent any input source code stream
pub trait SourceStream {
    // Return the char at the current position.
    // Or None if EOF is reached.
    fn peekc(&mut self) -> Option<char>;

    // Return the char at the current position.
    // Or None if EOF is reached.
    // Then advance to next char if not EOF.
    fn getc(&mut self) -> Option<char>;

    // Returns true if the stream is currently at the last position.
    fn is_eof(&self) -> bool;

    // Get the current position in the stream.
    fn get_position(&self) -> RawFilePosition;

    // Set the current position in the stream.
    fn set_position(&mut self, pos: RawFilePosition);

    // Set the current position in the stream to the beginning of the line corresponding to `pos`.
    fn set_position_to_begin_of_line(&mut self, pos: RawFilePosition);

    // Returns the stream identifier.
    fn get_file_id(&self) -> RawFileIdentifier;
}

// Source input stream from a source code string.
pub struct StringSourceStream {
    data: Vec<char>,
    pos: usize,
    rid: RawFileIdentifier,
}

impl StringSourceStream {
    pub fn from_str(s: &str, rid: RawFileIdentifier) -> Self {
        StringSourceStream {
            data: s.chars().collect(),
            pos: 0,
            rid,
        }
    }
}

impl SourceStream for StringSourceStream {
    fn peekc(&mut self) -> Option<char> {
        if self.pos == self.data.len() {
            None
        } else {
            Some(self.data[self.pos])
        }
    }

    fn getc(&mut self) -> Option<char> {
        let res = self.peekc();
        if res.is_some() {
            self.pos += 1;
        }
        res
    }

    fn is_eof(&self) -> bool {
        self.pos == self.data.len()
    }

    fn get_position(&self) -> RawFilePosition {
        RawFilePosition(self.pos as u64)
    }

    fn set_position(&mut self, pos: RawFilePosition) {
        assert!((pos.0 as usize) <= self.data.len());
        self.pos = pos.0 as usize;
    }

    fn set_position_to_begin_of_line(&mut self, pos: RawFilePosition) {
        self.set_position(pos);
        // Go backward until the previous char is a '\n'.
        while self.pos > 0 && self.data[self.pos - 1] != '\n' {
            self.pos -= 1;
        }
    }

    fn get_file_id(&self) -> RawFileIdentifier {
        self.rid
    }
}

// Source input stream from a source code file.
pub struct FileSourceStream {
    reader: BufReader<File>,
    line: Vec<char>,
    file_pos: u64,
    line_pos: usize,
    reached_eof: bool,
    rid: RawFileIdentifier,
}

impl FileSourceStream {
    pub fn new(fs: File, rid: RawFileIdentifier) -> Self {
        let mut res = Self {
            reader: BufReader::new(fs),
            line: Vec::new(),
            file_pos: 0,
            line_pos: 0,
            reached_eof: false,
            rid,
        };
        res._fill_line();
        res
    }

    // Ensure we fill the line String for the next read.
    fn _fill_line(&mut self) {
        // Don't fill if we are already at EOF.
        if self.reached_eof {
            return;
        }

        // Don't fill if we have some data left in the line
        if self.line_pos < self.line.len() {
            return;
        }

        // Read a new line;
        self.file_pos = self.reader.seek(std::io::SeekFrom::Current(0)).unwrap();
        let mut line_str = String::new();
        self.reader
            .read_line(&mut line_str)
            .expect("Failed to read line from input file");
        self.line = line_str.chars().collect();
        self.line_pos = 0;

        // If it's empty means we reached eof.
        self.reached_eof = self.line.len() == 0;
    }
}

impl SourceStream for FileSourceStream {
    fn peekc(&mut self) -> Option<char> {
        // Fill the buffer first.
        self._fill_line();

        // Check for EOF.
        if self.reached_eof {
            return None;
        }

        assert!(self.line_pos < self.line.len());
        Some(self.line[self.line_pos])
    }

    fn getc(&mut self) -> Option<char> {
        let res = self.peekc();
        // Move to the next char if we didn't reach EOF
        if res.is_some() {
            assert!(self.line_pos < self.line.len());
            self.line_pos += 1;
            // We need to read the next line already.
            // This ensured the eof flag gets updated correctly.
            self._fill_line();
        }

        res
    }

    fn is_eof(&self) -> bool {
        self.reached_eof
    }

    fn get_position(&self) -> RawFilePosition {
        let line_pos = self.line_pos as u64;
        // Put both in the same field
        let pos = (self.file_pos << 32) | line_pos;
        RawFilePosition(pos)
    }

    fn set_position(&mut self, pos: RawFilePosition) {
        let file_pos = pos.0 >> 32;
        let line_pos = pos.0 & 0xFFFFFFFF;

        // Update file position.
        self.reader
            .seek(std::io::SeekFrom::Start(file_pos))
            .unwrap();

        // Reset fields before fetching data again.
        self.reached_eof = false;
        self.line_pos = 0;
        self.line.clear();
        self._fill_line();

        // Update position in the line.
        self.line_pos = line_pos as usize;
    }

    fn set_position_to_begin_of_line(&mut self, pos: RawFilePosition) {
        self.set_position(pos);
        self.line_pos = 0;
    }

    fn get_file_id(&self) -> RawFileIdentifier {
        self.rid
    }
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    use rand::distributions::DistString;

    use super::*;

    fn write_string_to_file(path: &str, s: &str) {
        let mut os = File::create(path).expect("Failed to create output file");
        write!(os, "{}", s).unwrap();
    }

    fn gen_stream(data: &str, use_file: bool) -> Box<dyn SourceStream> {
        if !use_file {
            return Box::new(StringSourceStream::from_str(data, RawFileIdentifier(0)));
        }

        let tmp_file_path = format!(
            "{}/test_{}.txt",
            std::env::temp_dir().to_str().unwrap(),
            rand::distributions::Alphanumeric.sample_string(&mut rand::thread_rng(), 16)
        );

        write_string_to_file(&tmp_file_path, data);

        Box::new(FileSourceStream::new(
            File::open(tmp_file_path).unwrap(),
            RawFileIdentifier(0),
        ))
    }

    fn stream_to_str(is: &mut dyn SourceStream, beg: RawFilePosition) -> String {
        // Save the current position.
        let old_pos = is.get_position();
        is.set_position(beg);

        // Read all characters.
        let mut res = String::new();
        while !is.is_eof() {
            res.push(is.getc().unwrap());
        }

        // Restore the original position.
        is.set_position(old_pos);
        return res;
    }

    fn impl_test_stream_basic(use_file: bool) {
        let source_str = "Hello!";
        let mut is = gen_stream(source_str, use_file);
        let beg_pos = is.get_position();

        assert_eq!(is.peekc(), Some('H'));
        assert_eq!(is.peekc(), Some('H'));
        assert!(!is.is_eof());
        assert_eq!(is.getc(), Some('H'));
        assert_eq!(is.peekc(), Some('e'));
        let e_pos = is.get_position();
        assert_eq!(is.getc(), Some('e'));
        assert_eq!(is.getc(), Some('l'));
        assert_eq!(is.getc(), Some('l'));
        assert_eq!(source_str, stream_to_str(is.as_mut(), beg_pos));
        assert_eq!(is.getc(), Some('o'));
        assert!(!is.is_eof());
        assert_eq!(is.peekc(), Some('!'));
        assert!(!is.is_eof());
        assert_eq!(is.getc(), Some('!'));
        assert!(is.is_eof());
        assert_eq!(is.peekc(), None);
        assert_eq!(is.getc(), None);
        assert_eq!(is.getc(), None);
        assert!(is.is_eof());

        is.set_position(e_pos);
        assert!(!is.is_eof());
        assert_eq!(is.peekc(), Some('e'));
        assert_eq!(is.getc(), Some('e'));
        assert_eq!(is.getc(), Some('l'));
    }

    #[test]
    fn test_str_stream_basic() {
        impl_test_stream_basic(false);
    }

    #[test]
    fn test_file_stream_basic() {
        impl_test_stream_basic(true);
    }

    fn impl_test_stream_empty(use_file: bool) {
        let source_str = "";
        let mut is = gen_stream(source_str, use_file);
        let beg_pos = is.get_position();
        assert!(is.is_eof());
        assert_eq!(is.peekc(), None);
        assert_eq!(is.getc(), None);
        assert_eq!(is.getc(), None);
        assert!(is.is_eof());
        assert_eq!(source_str, stream_to_str(is.as_mut(), beg_pos));
    }

    #[test]
    fn test_str_stream_empty() {
        impl_test_stream_empty(false);
    }

    #[test]
    fn test_file_stream_empty() {
        impl_test_stream_empty(true);
    }

    fn impl_test_stream_multiline(use_file: bool) {
        let source_str = "\nfoo x\nbar\n\nlol ipop\n";
        let mut is = gen_stream(source_str, use_file);
        let beg_pos = is.get_position();

        assert_eq!(is.peekc(), Some('\n'));
        assert_eq!(is.peekc(), Some('\n'));
        assert!(!is.is_eof());
        assert_eq!(is.getc(), Some('\n'));
        assert_eq!(is.peekc(), Some('f'));
        assert_eq!(is.getc(), Some('f'));
        assert_eq!(is.getc(), Some('o'));
        assert_eq!(is.getc(), Some('o'));
        assert_eq!(is.getc(), Some(' '));
        let x_pos = is.get_position();
        assert_eq!(is.getc(), Some('x'));
        assert_eq!(is.getc(), Some('\n'));
        assert_eq!(is.getc(), Some('b'));
        assert_eq!(source_str, stream_to_str(is.as_mut(), beg_pos));
        assert_eq!(is.getc(), Some('a'));
        assert_eq!(is.getc(), Some('r'));
        assert_eq!(is.getc(), Some('\n'));
        let before_l_pos = is.get_position();
        assert_eq!(is.getc(), Some('\n'));
        assert_eq!(is.getc(), Some('l'));

        is.set_position(x_pos);
        assert_eq!(is.getc(), Some('x'));
        assert_eq!(is.getc(), Some('\n'));
        assert_eq!(is.getc(), Some('b'));

        is.set_position(before_l_pos);
        assert_eq!(is.getc(), Some('\n'));
        assert_eq!(is.getc(), Some('l'));
        assert_eq!(is.getc(), Some('o'));
    }

    #[test]
    fn test_str_stream_multiline() {
        impl_test_stream_multiline(false);
    }

    #[test]
    fn test_file_stream_multiline() {
        impl_test_stream_multiline(true);
    }

    fn impl_test_stream_empty_lines(use_file: bool) {
        let source_str = "\n\n\n\n";
        let mut is = gen_stream(source_str, use_file);
        let beg_pos = is.get_position();
        assert!(!is.is_eof());
        assert_eq!(is.peekc(), Some('\n'));
        assert_eq!(is.getc(), Some('\n'));
        assert_eq!(is.getc(), Some('\n'));
        assert_eq!(source_str, stream_to_str(is.as_mut(), beg_pos));
        assert_eq!(is.getc(), Some('\n'));
        assert_eq!(is.getc(), Some('\n'));
        assert!(is.is_eof());
        assert_eq!(is.peekc(), None);
        assert_eq!(is.getc(), None);
        assert_eq!(is.getc(), None);
        assert!(is.is_eof());
    }

    #[test]
    fn test_str_stream_empty_lines() {
        impl_test_stream_empty_lines(false);
    }

    #[test]
    fn test_file_stream_empty_lines() {
        impl_test_stream_empty_lines(true);
    }
}
