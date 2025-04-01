use std::fmt::Display;

// Encode the position in a specific file.
// The representation is completely opaque.
// It depends on the underlying source_stream implementation.
// Also, this differs from file tell / seek (which only returns a position in bytes).
// The file might have multi-bytes encoding (eg ASCII), and this represent a char position.
// Also you can't assume doing -n / +n will move the position by <n> chars.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RawFilePosition(pub u64);

// Encode unique identifier for a file.
// This tricks avoid having to keep files around when manipulating locations.
// We only need the file when logging.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RawFileIdentifier(pub u64);

// Represent a range of characters in an input file.
// This can be used to then extract extra informations from a file.
#[derive(Debug, Clone, Copy)]
pub struct Location {
    // Raw position of the beginning of the range.
    beg_pos: RawFilePosition,
    // Raw position of the end of the range (included).
    end_pos: RawFilePosition,

    // Unique identifier for the file.
    file_id: RawFileIdentifier,

    // The line number where the location starts.
    beg_line: usize,

    // The line number where the location ends (included).
    end_line: usize,

    // The column number where the location starts.
    beg_col: usize,

    // The column number where the location ends.
    end_col: usize,
}

impl Location {
    pub fn new(
        beg_pos: RawFilePosition,
        end_pos: RawFilePosition,
        file_id: RawFileIdentifier,
        beg_line: usize,
        end_line: usize,
        beg_col: usize,
        end_col: usize,
    ) -> Self {
        Self {
            beg_pos,
            end_pos,
            file_id,
            beg_line,
            end_line,
            beg_col,
            end_col,
        }
    }

    // Return a location with a unknown position.
    // Should be used only for testing.
    // Try to read this will result in an error.
    pub fn unknown_test_loc() -> Self {
        Self::new(
            RawFilePosition(0),
            RawFilePosition(0),
            RawFileIdentifier(0),
            0,
            0,
            0,
            0,
        )
    }

    // Create a new location [beg - end].
    pub fn join(beg: Self, end: Self) -> Self {
        assert!(beg.file_id == end.file_id);
        Self::new(
            beg.beg_pos,
            end.end_pos,
            beg.file_id,
            beg.beg_line,
            end.end_line,
            beg.beg_col,
            end.end_col,
        )
    }

    pub fn beg_pos(&self) -> RawFilePosition {
        self.beg_pos
    }

    pub fn end_pos(&self) -> RawFilePosition {
        self.end_pos
    }

    pub fn file_id(&self) -> RawFileIdentifier {
        self.file_id
    }

    pub fn beg_line(&self) -> usize {
        self.beg_line
    }

    pub fn end_line(&self) -> usize {
        self.end_line
    }

    pub fn beg_col(&self) -> usize {
        self.beg_col
    }

    pub fn end_col(&self) -> usize {
        self.end_col
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.beg_line == self.end_line && self.beg_col == self.end_col {
            write!(f, "{}:{}", self.beg_line, self.beg_col)
        } else if self.beg_line == self.end_line {
            write!(f, "{}:{}-{}", self.beg_line, self.beg_col, self.end_col)
        } else {
            write!(
                f,
                "{}:{}-{}:{}",
                self.beg_line, self.beg_col, self.end_line, self.end_col
            )
        }
    }
}
