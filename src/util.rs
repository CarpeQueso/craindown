use std::fmt;

#[derive(Clone, PartialEq)]
pub struct FilePosition {
    /// The line number of this token
    pub line: usize,
    /// The number of unicode scalars between the last line break and the beginning of this token
    pub position_in_line: usize,
    /// The number of unicode scalars from the beginning of the file to this token
    pub absolute_position: usize,
}

impl FilePosition {
    pub fn new(line: usize, position_in_line: usize, absolute_position: usize) -> Self {
        FilePosition {
            line,
            position_in_line,
            absolute_position,
        }
    }
}

impl fmt::Display for FilePosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Position({},{},{})",
            self.line, self.position_in_line, self.absolute_position
        )
    }
}

impl fmt::Debug for FilePosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Position({},{},{})",
            self.line, self.position_in_line, self.absolute_position
        )
    }
}
