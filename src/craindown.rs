use std::collections::HashSet;

pub struct Craindown {}

impl Craindown {
    pub fn new() -> Self {
        Craindown {}
    }
}

// TODO: Kind of needs to be changed since metadata doesn't really feel like a "block".
#[derive(Clone)]
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
#[derive(Clone)]
pub enum InlineElement {
    Text(Text),
    Link(Link),
}

#[derive(Clone)]
pub struct Text {
    text_type: TextType,
    text: String,
}

#[derive(Clone)]
pub enum TextType {
    Math,
    Code,
    Literal,
    Formatted(HashSet<FormatSpecifier>),
    Plain,
}

#[derive(Clone, PartialEq)]
pub enum FormatSpecifier {
    Bold,
    Italic,
    Underline,
    Strikethrough,
}

#[derive(Clone, PartialEq)]
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

#[derive(Clone)]
pub struct SectionHeading {
    pub level: usize,
    pub contents: Vec<InlineElement>,
}

impl SectionHeading {
    pub fn new(contents: Vec<InlineElement>) -> Self {
        SectionHeading { level: 1, contents }
    }
}

#[derive(Clone)]
pub struct TextBlock {
    pub contents: Vec<InlineElement>,
}

impl TextBlock {
    pub fn new(contents: Vec<InlineElement>) -> Self {
        TextBlock { contents }
    }
}

#[derive(Clone, PartialEq)]
pub struct MathBlock {
    // One day this may become slightly more standardized. Not today...
    pub options: Option<String>,
    pub contents: String,
}

impl MathBlock {
    pub fn new(options: Option<String>, contents: String) -> Self {
        Self { options, contents }
    }
}

#[derive(Clone, PartialEq)]
pub struct ExportBlock {
    pub options: Option<String>,
    pub contents: String,
}

impl ExportBlock {
    pub fn new(options: Option<String>, contents: String) -> Self {
        Self { options, contents }
    }
}

#[derive(Clone, PartialEq)]
pub struct CodeBlock {
    pub options: Option<String>,
    pub contents: String,
}

impl CodeBlock {
    pub fn new(options: Option<String>, contents: String) -> Self {
        Self { options, contents }
    }
}

#[derive(Clone, PartialEq)]
pub struct LiteralBlock {
    pub options: Option<String>,
    pub contents: String,
}

impl LiteralBlock {
    pub fn new(options: Option<String>, contents: String) -> Self {
        Self { options, contents }
    }
}

#[derive(Clone, PartialEq)]
pub struct QuoteBlock {}

impl QuoteBlock {
    pub fn new() -> Self {
        QuoteBlock {}
    }
}

#[derive(Clone, PartialEq)]
pub struct Link {
    description: Option<String>,
    uri: String,
}

impl Link {
    pub fn new(description: Option<String>, uri: String) -> Self {
        Link { description, uri }
    }
}
