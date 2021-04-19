pub struct Craindown {}

impl Craindown {
    pub fn new() -> Self {
        Craindown {}
    }
}

pub enum Expr {
    Metadata(Metadata),
    SectionHeading(SectionHeading),
    TextBlock(TextBlock),
    MathBlock(MathBlock),
    ExportBlock(ExportBlock),
    CodeBlock(CodeBlock),
    LiteralBlock(LiteralBlock),
    QuoteBlock(QuoteBlock),
}

pub struct Metadata {
    key: String,
    value: String,
}

impl Metadata {
    pub fn new(key: &str, value: &str) -> Self {
        Metadata {
            key: key.to_string(),
            value: value.to_string(),
        }
    }
}

pub struct SectionHeading {
    pub level: usize,
}

impl SectionHeading {
    pub fn new() -> Self {
        SectionHeading { level: 1 }
    }
}

pub struct TextBlock {}

impl TextBlock {
    pub fn new() -> Self {
        TextBlock {}
    }
}

pub struct MathBlock {
    pub options: Option<String>,
    pub contents: String,
}

impl MathBlock {
    pub fn new() -> Self {
        MathBlock {
            options: None,
            contents: String::new(),
        }
    }
}

pub struct ExportBlock {}

impl ExportBlock {
    pub fn new() -> Self {
        ExportBlock {}
    }
}

pub struct CodeBlock {}

impl CodeBlock {
    pub fn new() -> Self {
        CodeBlock {}
    }
}

pub struct LiteralBlock {}

impl LiteralBlock {
    pub fn new() -> Self {
        LiteralBlock {}
    }
}

pub struct QuoteBlock {}

impl QuoteBlock {
    pub fn new() -> Self {
        QuoteBlock {}
    }
}
