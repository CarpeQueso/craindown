use std::collections::HashSet;

pub struct Craindown {}

impl Craindown {
    pub fn new() -> Self {
        Craindown {}
    }
}

// TODO: Kind of needs to be changed since metadata doesn't really feel like a "block".
#[derive(Clone, Debug)]
pub enum BlockElement {
    Metadata(Metadata),
    SectionHeading(SectionHeading),
    TextBlock(TextBlock),
    MathBlock(MathBlock),
    ExportBlock(ExportBlock),
    CodeBlock(CodeBlock),
    LiteralBlock(LiteralBlock),
    QuoteBlock(QuoteBlock),
}

// TODO: Change name, but "inline" is the right idea.
#[derive(Clone, Debug, PartialEq)]
pub enum InlineElement {
    Text(Text),
    Link(Link),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Text {
    text_type: TextType,
    text: String,
}

impl Text {
    pub fn new(text_type: TextType, text: &str) -> Self {
        Text {
            text_type,
            text: text.to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TextType {
    Math,
    Code,
    Literal,
    Formatted(HashSet<FormatSpecifier>),
    Plain,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FormatSpecifier {
    Bold,
    Italic,
    Underline,
    Strikethrough,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Metadata {
    pub key: String,
    pub value: String,
}

impl Metadata {
    pub fn new(key: &str, value: &str) -> Self {
        Metadata {
            key: key.to_string(),
            value: value.to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SectionHeading {
    pub level: usize,
    pub contents: Vec<InlineElement>,
}

impl SectionHeading {
    pub fn new(level: usize, contents: Vec<InlineElement>) -> Self {
        SectionHeading { level, contents }
    }
}

#[derive(Clone, Debug)]
pub struct TextBlock {
    pub contents: Vec<InlineElement>,
}

impl TextBlock {
    pub fn new(contents: Vec<InlineElement>) -> Self {
        TextBlock { contents }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MathBlock {
    // One day this may become slightly more standardized. Not today...
    pub options: Option<String>,
    pub contents: String,
}

impl MathBlock {
    pub fn new(options: Option<&str>, contents: &str) -> Self {
        let options = match options {
            Some(s) => Some(s.to_string()),
            None => None,
        };

        Self {
            options,
            contents: contents.to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExportBlock {
    pub options: Option<String>,
    pub contents: String,
}

impl ExportBlock {
    pub fn new(options: Option<&str>, contents: &str) -> Self {
        let options = match options {
            Some(s) => Some(s.to_string()),
            None => None,
        };

        Self {
            options,
            contents: contents.to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CodeBlock {
    pub options: Option<String>,
    pub contents: String,
}

impl CodeBlock {
    pub fn new(options: Option<&str>, contents: &str) -> Self {
        let options = match options {
            Some(s) => Some(s.to_string()),
            None => None,
        };

        Self {
            options,
            contents: contents.to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LiteralBlock {
    pub options: Option<String>,
    pub contents: String,
}

impl LiteralBlock {
    pub fn new(options: Option<&str>, contents: &str) -> Self {
        let options = match options {
            Some(s) => Some(s.to_string()),
            None => None,
        };

        Self {
            options,
            contents: contents.to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct QuoteBlock {}

impl QuoteBlock {
    pub fn new() -> Self {
        QuoteBlock {}
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Link {
    description: Option<String>,
    uri: String,
}

impl Link {
    pub fn new(description: Option<&str>, uri: &str) -> Self {
        let description = match description {
            Some(s) => Some(s.to_string()),
            None => None,
        };

        Link {
            description,
            uri: uri.to_string(),
        }
    }
}
