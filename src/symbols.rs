// --------
// KEYWORDS
// --------
// These must be at the start of a line.
pub const METADATA_INDICATOR: &'static str = ">>+";
pub const SECTION_HEADING_INDICATOR: &'static str = "#";
pub const MATH_BLOCK_DELIM: &'static str = "$$$";
pub const EXPORT_BLOCK_DELIM: &'static str = ">>>";
pub const CODE_BLOCK_DELIM: &'static str = "```";
pub const LITERAL_BLOCK_DELIM: &'static str = "%%%";
pub const QUOTE_BLOCK_DELIM: &'static str = "\"\"\"";

// These can be after the start of a line, but are sometimes context-dependent.
pub const METADATA_SEPARATOR: &'static str = ":";
pub const INLINE_MATH_DELIM: &'static str = "$$";
pub const INLINE_CODE_DELIM: &'static str = "``";
pub const INLINE_LITERAL_DELIM: &'static str = "%%";
pub const BOLD_DELIM: &'static str = "**";
pub const ITALIC_DELIM: &'static str = "//";
pub const UNDERLINE_DELIM: &'static str = "__";
pub const STRIKETHROUGH_DELIM: &'static str = "~~";
pub const LINK_OPEN: &'static str = "[[";
pub const LINK_CLOSE: &'static str = "]]";
pub const LINK_INTERMEDIATE: &'static str = "][";

pub const SPACE: &'static str = " ";
pub const TAB: &'static str = "\t";
pub const WINDOWS_LINE_BREAK: &'static str = "\r\n";
pub const LINE_BREAK: &'static str = "\n";

pub const _INLINE_WHITESPACE_PATTERNS: [&'static str; 2] = [SPACE, TAB];
pub const LINE_BREAK_PATTERNS: [&'static str; 2] = [WINDOWS_LINE_BREAK, LINE_BREAK];
