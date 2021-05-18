use craindown::lex::{tokenize, Token, TokenType};
use craindown::symbols::*;
use craindown::util::FilePosition;

// TODO: It's good that the const patterns at the top are used for tokens,
// but is there a good way to also programmatically set the expected tokens'
// positions, too, based on the keyword lengths? That might be complicated enough
// to need testing of its own...

// Metadata
#[test]
fn tokenizes_metadata() {
    let s = [METADATA_INDICATOR, " key", METADATA_SEPARATOR, " value"].concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(
            TokenType::MetadataIndicator,
            METADATA_INDICATOR,
            &FilePosition::new(0, 0, 0),
        ),
        Token::new(TokenType::Text, "key", &FilePosition::new(0, 3, 3)),
        Token::new(
            TokenType::MetadataSeparator,
            METADATA_SEPARATOR,
            &FilePosition::new(0, 6, 6),
        ),
        Token::new(TokenType::Text, "value", &FilePosition::new(0, 8, 8)),
        Token::new(TokenType::EOF, "", &FilePosition::new(0, 13, 13)),
    ];

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn tokenizes_section_heading() {
    let s = [SECTION_HEADING_INDICATOR, " Heading"].concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(
            TokenType::SectionHeadingIndicator,
            SECTION_HEADING_INDICATOR,
            &FilePosition::new(0, 0, 0),
        ),
        Token::new(TokenType::Text, "Heading", &FilePosition::new(0, 2, 2)),
        Token::new(TokenType::EOF, "", &FilePosition::new(0, 9, 9)),
    ];

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn tokenizes_delims_in_section_heading() {
    let s = [
        SECTION_HEADING_INDICATOR,
        SECTION_HEADING_INDICATOR,
        SPACE,
        BOLD_DELIM,
        "Bold Heading",
        BOLD_DELIM,
    ]
    .concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(
            TokenType::SectionHeadingIndicator,
            SECTION_HEADING_INDICATOR,
            &FilePosition::new(0, 0, 0),
        ),
        Token::new(
            TokenType::SectionHeadingIndicator,
            SECTION_HEADING_INDICATOR,
            &FilePosition::new(0, 1, 1),
        ),
        Token::new(
            TokenType::BoldDelim,
            BOLD_DELIM,
            &FilePosition::new(0, 3, 3),
        ),
        Token::new(TokenType::Text, "Bold Heading", &FilePosition::new(0, 5, 5)),
        Token::new(
            TokenType::BoldDelim,
            BOLD_DELIM,
            &FilePosition::new(0, 17, 17),
        ),
        Token::new(TokenType::EOF, "", &FilePosition::new(0, 19, 19)),
    ];

    assert_eq!(tokens, expected_tokens);
}

// Lots of additional heading tests needed...

// Math block
#[test]
fn tokenizes_math_block() {
    let s = [
        MATH_BLOCK_DELIM,
        LINE_BREAK,
        "math",
        LINE_BREAK,
        MATH_BLOCK_DELIM,
    ]
    .concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(
            TokenType::MathBlockDelim,
            MATH_BLOCK_DELIM,
            &FilePosition::new(0, 0, 0),
        ),
        Token::new(
            TokenType::StructuralLineBreak,
            LINE_BREAK,
            &FilePosition::new(0, 3, 3),
        ),
        Token::new(TokenType::Text, "math", &FilePosition::new(1, 0, 4)),
        Token::new(
            TokenType::StructuralLineBreak,
            LINE_BREAK,
            &FilePosition::new(1, 4, 8),
        ),
        Token::new(
            TokenType::MathBlockDelim,
            MATH_BLOCK_DELIM,
            &FilePosition::new(2, 0, 9),
        ),
        Token::new(TokenType::EOF, "", &FilePosition::new(2, 3, 12)),
    ];

    assert_eq!(tokens, expected_tokens);
}

// Export block
#[test]
fn tokenizes_export_block() {
    let s = [
        EXPORT_BLOCK_DELIM,
        LINE_BREAK,
        "export",
        LINE_BREAK,
        EXPORT_BLOCK_DELIM,
    ]
    .concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(
            TokenType::ExportBlockDelim,
            EXPORT_BLOCK_DELIM,
            &FilePosition::new(0, 0, 0),
        ),
        Token::new(
            TokenType::StructuralLineBreak,
            LINE_BREAK,
            &FilePosition::new(0, 3, 3),
        ),
        Token::new(TokenType::Text, "export", &FilePosition::new(1, 0, 4)),
        Token::new(
            TokenType::StructuralLineBreak,
            LINE_BREAK,
            &FilePosition::new(1, 6, 10),
        ),
        Token::new(
            TokenType::ExportBlockDelim,
            EXPORT_BLOCK_DELIM,
            &FilePosition::new(2, 0, 11),
        ),
        Token::new(TokenType::EOF, "", &FilePosition::new(2, 3, 14)),
    ];

    assert_eq!(tokens, expected_tokens);
}

// Code block
#[test]
fn tokenizes_code_block() {
    let s = [
        CODE_BLOCK_DELIM,
        LINE_BREAK,
        "code",
        LINE_BREAK,
        CODE_BLOCK_DELIM,
    ]
    .concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(
            TokenType::CodeBlockDelim,
            CODE_BLOCK_DELIM,
            &FilePosition::new(0, 0, 0),
        ),
        Token::new(
            TokenType::StructuralLineBreak,
            LINE_BREAK,
            &FilePosition::new(0, 3, 3),
        ),
        Token::new(TokenType::Text, "code", &FilePosition::new(1, 0, 4)),
        Token::new(
            TokenType::StructuralLineBreak,
            LINE_BREAK,
            &FilePosition::new(1, 4, 8),
        ),
        Token::new(
            TokenType::CodeBlockDelim,
            CODE_BLOCK_DELIM,
            &FilePosition::new(2, 0, 9),
        ),
        Token::new(TokenType::EOF, "", &FilePosition::new(2, 3, 12)),
    ];

    assert_eq!(tokens, expected_tokens);
}

// Literal block
#[test]
fn tokenizes_literal_block() {
    let s = [
        LITERAL_BLOCK_DELIM,
        LINE_BREAK,
        "literal",
        LINE_BREAK,
        LITERAL_BLOCK_DELIM,
    ]
    .concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(
            TokenType::LiteralBlockDelim,
            LITERAL_BLOCK_DELIM,
            &FilePosition::new(0, 0, 0),
        ),
        Token::new(
            TokenType::StructuralLineBreak,
            LINE_BREAK,
            &FilePosition::new(0, 3, 3),
        ),
        Token::new(TokenType::Text, "literal", &FilePosition::new(1, 0, 4)),
        Token::new(
            TokenType::StructuralLineBreak,
            LINE_BREAK,
            &FilePosition::new(1, 7, 11),
        ),
        Token::new(
            TokenType::LiteralBlockDelim,
            LITERAL_BLOCK_DELIM,
            &FilePosition::new(2, 0, 12),
        ),
        Token::new(TokenType::EOF, "", &FilePosition::new(2, 3, 15)),
    ];

    assert_eq!(tokens, expected_tokens);
}

// Quote block
#[test]
fn tokenizes_quote_block() {
    let s = [QUOTE_BLOCK_INDICATOR, " Some text"].concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(
            TokenType::QuoteBlockIndicator,
            QUOTE_BLOCK_INDICATOR,
            &FilePosition::new(0, 0, 0),
        ),
        Token::new(TokenType::Text, "Some text", &FilePosition::new(0, 2, 2)),
        Token::new(TokenType::EOF, "", &FilePosition::new(0, 11, 11)),
    ];

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn tokenizes_preceding_space() {
    let s = [SPACE, TAB].concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(
            TokenType::StructuralSpace,
            SPACE,
            &FilePosition::new(0, 0, 0),
        ),
        Token::new(TokenType::StructuralTab, TAB, &FilePosition::new(0, 1, 1)),
        Token::new(TokenType::EOF, "", &FilePosition::new(0, 2, 2)),
    ];

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn tokenizes_inline_bold_delimiters() {
    let s = [BOLD_DELIM, "bold", BOLD_DELIM].concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(
            TokenType::BoldDelim,
            BOLD_DELIM,
            &FilePosition::new(0, 0, 0),
        ),
        Token::new(TokenType::Text, "bold", &FilePosition::new(0, 2, 2)),
        Token::new(
            TokenType::BoldDelim,
            BOLD_DELIM,
            &FilePosition::new(0, 6, 6),
        ),
        Token::new(TokenType::EOF, "", &FilePosition::new(0, 8, 8)),
    ];

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn tokenizes_inline_italic_delimiters() {
    let s = [ITALIC_DELIM, "italic", ITALIC_DELIM].concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(
            TokenType::ItalicDelim,
            ITALIC_DELIM,
            &FilePosition::new(0, 0, 0),
        ),
        Token::new(TokenType::Text, "italic", &FilePosition::new(0, 2, 2)),
        Token::new(
            TokenType::ItalicDelim,
            ITALIC_DELIM,
            &FilePosition::new(0, 8, 8),
        ),
        Token::new(TokenType::EOF, "", &FilePosition::new(0, 10, 10)),
    ];

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn tokenizes_inline_underline_delimiters() {
    let s = [UNDERLINE_DELIM, "underline", UNDERLINE_DELIM].concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(
            TokenType::UnderlineDelim,
            UNDERLINE_DELIM,
            &FilePosition::new(0, 0, 0),
        ),
        Token::new(TokenType::Text, "underline", &FilePosition::new(0, 2, 2)),
        Token::new(
            TokenType::UnderlineDelim,
            UNDERLINE_DELIM,
            &FilePosition::new(0, 11, 11),
        ),
        Token::new(TokenType::EOF, "", &FilePosition::new(0, 13, 13)),
    ];

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn tokenizes_inline_strikethrough_delimiters() {
    let s = [STRIKETHROUGH_DELIM, "strikethrough", STRIKETHROUGH_DELIM].concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(
            TokenType::StrikethroughDelim,
            STRIKETHROUGH_DELIM,
            &FilePosition::new(0, 0, 0),
        ),
        Token::new(
            TokenType::Text,
            "strikethrough",
            &FilePosition::new(0, 2, 2),
        ),
        Token::new(
            TokenType::StrikethroughDelim,
            STRIKETHROUGH_DELIM,
            &FilePosition::new(0, 15, 15),
        ),
        Token::new(TokenType::EOF, "", &FilePosition::new(0, 17, 17)),
    ];

    assert_eq!(tokens, expected_tokens);
}

// Inline Math
#[test]
fn tokenizes_inline_math() {
    let s = [INLINE_MATH_DELIM, "math", INLINE_MATH_DELIM].concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(
            TokenType::InlineMathDelim,
            INLINE_MATH_DELIM,
            &FilePosition::new(0, 0, 0),
        ),
        Token::new(TokenType::Text, "math", &FilePosition::new(0, 2, 2)),
        Token::new(
            TokenType::InlineMathDelim,
            INLINE_MATH_DELIM,
            &FilePosition::new(0, 6, 6),
        ),
        Token::new(TokenType::EOF, "", &FilePosition::new(0, 8, 8)),
    ];

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn inline_math_bypasses_other_delims() {
    let s = [
        INLINE_MATH_DELIM,
        BOLD_DELIM,
        "math",
        INLINE_CODE_DELIM,
        INLINE_MATH_DELIM,
    ]
    .concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(
            TokenType::InlineMathDelim,
            INLINE_MATH_DELIM,
            &FilePosition::new(0, 0, 0),
        ),
        Token::new(
            TokenType::Text,
            &[BOLD_DELIM, "math", INLINE_CODE_DELIM].concat(),
            &FilePosition::new(0, 2, 2),
        ),
        Token::new(
            TokenType::InlineMathDelim,
            INLINE_MATH_DELIM,
            &FilePosition::new(0, 10, 10),
        ),
        Token::new(TokenType::EOF, "", &FilePosition::new(0, 12, 12)),
    ];

    assert_eq!(tokens, expected_tokens);
}

// Inline Code
#[test]
fn tokenizes_inline_code() {
    let s = [INLINE_CODE_DELIM, "code", INLINE_CODE_DELIM].concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(
            TokenType::InlineCodeDelim,
            INLINE_CODE_DELIM,
            &FilePosition::new(0, 0, 0),
        ),
        Token::new(TokenType::Text, "code", &FilePosition::new(0, 2, 2)),
        Token::new(
            TokenType::InlineCodeDelim,
            INLINE_CODE_DELIM,
            &FilePosition::new(0, 6, 6),
        ),
        Token::new(TokenType::EOF, "", &FilePosition::new(0, 8, 8)),
    ];

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn inline_code_bypasses_other_delims() {
    let s = [
        INLINE_CODE_DELIM,
        BOLD_DELIM,
        "code",
        INLINE_MATH_DELIM,
        INLINE_CODE_DELIM,
    ]
    .concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(
            TokenType::InlineCodeDelim,
            INLINE_CODE_DELIM,
            &FilePosition::new(0, 0, 0),
        ),
        Token::new(
            TokenType::Text,
            &[BOLD_DELIM, "code", INLINE_MATH_DELIM].concat(),
            &FilePosition::new(0, 2, 2),
        ),
        Token::new(
            TokenType::InlineCodeDelim,
            INLINE_CODE_DELIM,
            &FilePosition::new(0, 10, 10),
        ),
        Token::new(TokenType::EOF, "", &FilePosition::new(0, 12, 12)),
    ];

    assert_eq!(tokens, expected_tokens);
}

// Inline Literal
#[test]
fn tokenizes_inline_literal() {
    let s = [INLINE_LITERAL_DELIM, "literal", INLINE_LITERAL_DELIM].concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(
            TokenType::InlineLiteralDelim,
            INLINE_LITERAL_DELIM,
            &FilePosition::new(0, 0, 0),
        ),
        Token::new(TokenType::Text, "literal", &FilePosition::new(0, 2, 2)),
        Token::new(
            TokenType::InlineLiteralDelim,
            INLINE_LITERAL_DELIM,
            &FilePosition::new(0, 9, 9),
        ),
        Token::new(TokenType::EOF, "", &FilePosition::new(0, 11, 11)),
    ];

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn inline_literal_bypasses_other_delims() {
    let s = [
        INLINE_LITERAL_DELIM,
        BOLD_DELIM,
        "lit",
        INLINE_MATH_DELIM,
        INLINE_LITERAL_DELIM,
    ]
    .concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(
            TokenType::InlineLiteralDelim,
            INLINE_LITERAL_DELIM,
            &FilePosition::new(0, 0, 0),
        ),
        Token::new(
            TokenType::Text,
            &[BOLD_DELIM, "lit", INLINE_MATH_DELIM].concat(),
            &FilePosition::new(0, 2, 2),
        ),
        Token::new(
            TokenType::InlineLiteralDelim,
            INLINE_LITERAL_DELIM,
            &FilePosition::new(0, 9, 9),
        ),
        Token::new(TokenType::EOF, "", &FilePosition::new(0, 11, 11)),
    ];

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn tokenizes_link_without_description() {
    let s = [LINK_OPEN, "link", LINK_CLOSE].concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(TokenType::LinkOpen, LINK_OPEN, &FilePosition::new(0, 0, 0)),
        Token::new(TokenType::Text, "link", &FilePosition::new(0, 2, 2)),
        Token::new(
            TokenType::LinkClose,
            LINK_CLOSE,
            &FilePosition::new(0, 6, 6),
        ),
        Token::new(TokenType::EOF, "", &FilePosition::new(0, 8, 8)),
    ];

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn tokenizes_link_with_description() {
    let s = [
        LINK_OPEN,
        "description",
        LINK_INTERMEDIATE,
        "link",
        LINK_CLOSE,
    ]
    .concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(TokenType::LinkOpen, LINK_OPEN, &FilePosition::new(0, 0, 0)),
        Token::new(TokenType::Text, "description", &FilePosition::new(0, 2, 2)),
        Token::new(
            TokenType::LinkIntermediate,
            LINK_INTERMEDIATE,
            &FilePosition::new(0, 13, 13),
        ),
        Token::new(TokenType::Text, "link", &FilePosition::new(0, 15, 15)),
        Token::new(
            TokenType::LinkClose,
            LINK_CLOSE,
            &FilePosition::new(0, 19, 19),
        ),
        Token::new(TokenType::EOF, "", &FilePosition::new(0, 21, 21)),
    ];

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn tokenizes_structural_space() {
    let s = [SPACE, SPACE].concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(
            TokenType::StructuralSpace,
            SPACE,
            &FilePosition::new(0, 0, 0),
        ),
        Token::new(
            TokenType::StructuralSpace,
            SPACE,
            &FilePosition::new(0, 1, 1),
        ),
        Token::new(TokenType::EOF, "", &FilePosition::new(0, 2, 2)),
    ];

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn tokenizes_structural_tab() {
    let s = [TAB, TAB].concat();

    let tokens = tokenize(&s);

    let expected_tokens = vec![
        Token::new(TokenType::StructuralTab, TAB, &FilePosition::new(0, 0, 0)),
        Token::new(TokenType::StructuralTab, TAB, &FilePosition::new(0, 1, 1)),
        Token::new(TokenType::EOF, "", &FilePosition::new(0, 2, 2)),
    ];

    assert_eq!(tokens, expected_tokens);
}
