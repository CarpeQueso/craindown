use crate::craindown::*;
use crate::error::{ParseError, ParseErrorType};
use crate::lex::{Token, TokenType};
use crate::symbols::*;
use crate::util::FilePosition;
use crate::{formatted_text_type, parse_error};

use std::collections::HashSet;

const INLINE_ELEMENT_TOKENS: [TokenType; 9] = [
    TokenType::Text,
    TokenType::InlineMathDelim,
    TokenType::InlineCodeDelim,
    TokenType::InlineLiteralDelim,
    TokenType::BoldDelim,
    TokenType::ItalicDelim,
    TokenType::UnderlineDelim,
    TokenType::StrikethroughDelim,
    TokenType::LinkOpen,
];

pub fn parse(tokens: &[Token]) -> Result<Craindown, (Craindown, Vec<ParseError>)> {
    let mut parser = Parser::new(&tokens);

    parser.parse()
}

pub type ParseResult<T> = Result<T, ParseError>;

struct Parser<'a> {
    tokens: &'a [Token],
    idx: usize,
    blocks: Vec<BlockElement>,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        if tokens.len() == 0 {
            panic!("A valid token slice must at least contain the EOF token");
        }

        Parser {
            tokens,
            idx: 0,
            blocks: Vec::new(),
            errors: Vec::new(),
        }
    }

    // TODO: Rename to avoid confusion with rust iterators?
    fn next(&mut self) -> &Token {
        if self.peek_match(&TokenType::EOF) {
            self.peek()
        } else {
            let token = &self.tokens[self.idx];

            self.idx += 1;

            token
        }
    }

    fn prev(&mut self) -> Option<&Token> {
        if self.idx == 0 {
            None
        } else {
            Some(&self.tokens[self.idx - 1])
        }
    }

    fn rewind(&mut self) {
        if self.idx > 0 {
            self.idx -= 1;
        }
    }

    fn peek(&mut self) -> &Token {
        &self.tokens[self.idx]
    }

    fn peek_match(&mut self, token_type: &TokenType) -> bool {
        if self.peek().token_type == *token_type {
            true
        } else {
            false
        }
    }

    fn peek_match_any(&mut self, types_to_match: &[TokenType]) -> bool {
        for type_to_match in types_to_match {
            if self.peek_match(type_to_match) {
                return true;
            }
        }

        false
    }

    fn advance_match_any(&mut self, types_to_match: &[TokenType]) -> bool {
        for type_to_match in types_to_match {
            if self.peek_match(type_to_match) {
                self.next();
                return true;
            }
        }

        false
    }

    fn is_at_end(&mut self) -> bool {
        self.peek_match(&TokenType::EOF)
    }

    /// Continue advancing the iterator while the next token's type is contained in
    /// `types_to_match`.
    ///
    /// The matching token will not be consumed.
    fn advance_past(&mut self, types_to_match: &[TokenType]) {
        for type_to_match in types_to_match {
            if self.peek_match(type_to_match) {
                self.next();
            }
        }
    }

    /// Continue advancing the iterator _until_ the next token's type is contained in
    /// `types_to_match`.
    ///
    /// The matching token will not be consumed.
    fn advance_until(&mut self, types_to_match: &[TokenType]) {
        while !self.is_at_end() {
            for type_to_match in types_to_match {
                if self.peek_match(type_to_match) {
                    return;
                }
            }

            self.next();
        }
    }

    /// If you can't recover from an error, call this to move the iterator to the beginning
    /// of the next block.
    fn synchronize(&mut self) {
        // Reach the end of a block
        self.advance_until(&[TokenType::StructuralLineBreak]);
        // Bypass line breaks to get to the start of the next block
        self.advance_past(&[TokenType::StructuralLineBreak]);
    }

    fn parse(&mut self) -> Result<Craindown, (Craindown, Vec<ParseError>)> {
        // TODO: Find a clean way of advancing past all whitespace until the first block.

        // An empty file is a valid file, so we should check for that before continuing.
        if self.peek_match(&TokenType::EOF) {
            return Ok(Craindown::new(&[]));
        }

        while !self.is_at_end() {
            match self.block() {
                Ok(block) => self.blocks.push(block),
                Err(err) => {
                    self.errors.push(err);
                    // Advance the iterator to a point where it can continue parsing.
                    self.synchronize();
                }
            };
        }

        let craindown = Craindown::new(&self.blocks);

        if self.errors.len() > 0 {
            Err((craindown, self.errors.clone()))
        } else {
            Ok(craindown)
        }
    }

    fn block(&mut self) -> ParseResult<BlockElement> {
        let token = self.next();

        match &token.token_type {
            TokenType::MetadataIndicator => Ok(BlockElement::Metadata(self.metadata()?)),
            TokenType::SectionHeadingIndicator => {
                Ok(BlockElement::SectionHeading(self.section_heading()?))
            }
            TokenType::MathBlockDelim => Ok(BlockElement::MathBlock(self.math_block()?)),
            TokenType::ExportBlockDelim => Ok(BlockElement::ExportBlock(self.export_block()?)),
            TokenType::CodeBlockDelim => Ok(BlockElement::CodeBlock(self.code_block()?)),
            TokenType::LiteralBlockDelim => Ok(BlockElement::LiteralBlock(self.literal_block()?)),
            TokenType::QuoteBlockIndicator => Ok(BlockElement::QuoteBlock(self.quote_block()?)),
            // This list should contain anything that could start a text block.
            TokenType::InlineMathDelim
            | TokenType::InlineCodeDelim
            | TokenType::InlineLiteralDelim
            | TokenType::BoldDelim
            | TokenType::ItalicDelim
            | TokenType::UnderlineDelim
            | TokenType::StrikethroughDelim
            | TokenType::LinkOpen
            | TokenType::Text => Ok(BlockElement::TextBlock(self.text_block()?)),
            _ => parse_error!(ParseErrorType::UnexpectedToken, "", token), // TODO
        }
    }

    fn metadata(&mut self) -> ParseResult<Metadata> {
        let key = self.plain_text()?;

        self.metadata_separator()?;

        let value = self.plain_text()?;

        self.block_end()?;

        Ok(Metadata::new(&key, &value))
    }

    fn section_heading(&mut self) -> ParseResult<SectionHeading> {
        // Start at 1. We've already seen one heading token to get here.
        let mut level: usize = 1;

        while self.advance_match_any(&[TokenType::SectionHeadingIndicator]) {
            level += 1;
        }

        let content = self.formatted_text()?;

        self.block_end()?;

        Ok(SectionHeading::new(level, content))
    }

    fn math_block(&mut self) -> ParseResult<MathBlock> {
        let options = if self.peek_match(&TokenType::Text) {
            let s = self.plain_text()?;
            Some(s)
        } else {
            None
        };

        self.line_break()?;

        let contents = self.plain_text()?;

        self.line_break()?;

        self.math_block_delim()?;

        self.block_end()?;

        Ok(MathBlock::new(options.as_deref(), &contents))
    }

    fn export_block(&mut self) -> ParseResult<ExportBlock> {
        let options = if self.peek_match(&TokenType::Text) {
            let s = self.plain_text()?;
            Some(s)
        } else {
            None
        };

        self.line_break()?;

        let contents = self.plain_text()?;

        self.line_break()?;

        self.export_block_delim()?;

        self.block_end()?;

        Ok(ExportBlock::new(options.as_deref(), &contents))
    }

    fn code_block(&mut self) -> ParseResult<CodeBlock> {
        let options = if self.peek_match(&TokenType::Text) {
            let s = self.plain_text()?;
            Some(s)
        } else {
            None
        };

        self.line_break()?;

        let contents = self.plain_text()?;

        self.line_break()?;

        self.code_block_delim()?;

        self.block_end()?;

        Ok(CodeBlock::new(options.as_deref(), &contents))
    }

    fn literal_block(&mut self) -> ParseResult<LiteralBlock> {
        let options = if self.peek_match(&TokenType::Text) {
            let s = self.plain_text()?;
            Some(s)
        } else {
            None
        };

        self.line_break()?;

        let contents = self.plain_text()?;

        self.line_break()?;

        self.literal_block_delim()?;

        self.block_end()?;

        Ok(LiteralBlock::new(options.as_deref(), &contents))
    }

    fn quote_block(&mut self) -> ParseResult<QuoteBlock> {
        let inline_elements = self.formatted_text()?;

        self.block_end()?;

        Ok(QuoteBlock::new(inline_elements))
    }

    fn text_block(&mut self) -> ParseResult<TextBlock> {
        // Move the iterator back one element since the function calling this one
        // advanced past the first token in the sequence.
        self.rewind();

        let inline_elements = self.formatted_text()?;

        self.block_end()?;

        Ok(TextBlock::new(inline_elements))
    }

    /// Be careful with this function. It's the only ParseResult-returning function
    /// that expects the iterator to be positioned BEFORE the first token. This makes the
    /// code a bit cleaner, but better organization may change this later on.
    fn formatted_text(&mut self) -> ParseResult<Vec<InlineElement>> {
        let mut current_format: HashSet<FormatSpecifier> = HashSet::new();
        let mut inline_elements: Vec<InlineElement> = Vec::new();

        let toggle = |current_format: &mut HashSet<FormatSpecifier>, specifier| {
            if current_format.contains(&specifier) {
                current_format.remove(&specifier);
            } else {
                current_format.insert(specifier);
            }
        };

        // Check for, but don't advance past, the block end.
        while !self.peek_match_any(&[TokenType::StructuralLineBreak, TokenType::EOF]) {
            let token = self.next();

            match token.token_type {
                TokenType::InlineMathDelim => {
                    inline_elements.push(InlineElement::Text(self.inline_math()?));
                }
                TokenType::InlineCodeDelim => {
                    inline_elements.push(InlineElement::Text(self.inline_code()?));
                }
                TokenType::InlineLiteralDelim => {
                    inline_elements.push(InlineElement::Text(self.inline_literal()?));
                }
                TokenType::BoldDelim => {
                    toggle(&mut current_format, FormatSpecifier::Bold);
                }
                TokenType::ItalicDelim => {
                    toggle(&mut current_format, FormatSpecifier::Italic);
                }
                TokenType::UnderlineDelim => {
                    toggle(&mut current_format, FormatSpecifier::Underline);
                }
                TokenType::StrikethroughDelim => {
                    toggle(&mut current_format, FormatSpecifier::Strikethrough);
                }
                TokenType::LinkOpen => {
                    inline_elements.push(InlineElement::Link(self.link()?));
                }
                TokenType::Text => {
                    let text_type = if current_format.len() > 0 {
                        TextType::Formatted(current_format.clone())
                    } else {
                        TextType::Plain
                    };

                    let text = Text::new(text_type, &token.lexeme);

                    inline_elements.push(InlineElement::Text(text));
                }
                _ => {
                    return parse_error!(
                        ParseErrorType::UnexpectedToken,
                        "Expected text or text formatting. Found `{lexeme}` instead.",
                        token
                    )
                }
            }
        }

        if current_format.len() > 0 {
            let token = self.prev().unwrap();

            parse_error!(
                ParseErrorType::UnclosedFormatSpecifier,
                "Text block ended before all format specifiers were closed.",
                token
            )
        } else {
            Ok(inline_elements)
        }
    }

    fn inline_math(&mut self) -> ParseResult<Text> {
        let contents = self.plain_text()?;

        self.inline_math_delim()?;

        Ok(Text::new(TextType::Math, &contents))
    }

    fn inline_code(&mut self) -> ParseResult<Text> {
        let contents = self.plain_text()?;

        self.inline_code_delim()?;

        Ok(Text::new(TextType::Code, &contents))
    }

    fn inline_literal(&mut self) -> ParseResult<Text> {
        let contents = self.plain_text()?;

        self.inline_literal_delim()?;

        Ok(Text::new(TextType::Literal, &contents))
    }

    fn link(&mut self) -> ParseResult<Link> {
        let description_or_link = self.plain_text()?;

        if self.advance_match_any(&[TokenType::LinkIntermediate]) {
            let link = self.plain_text()?;

            self.link_close()?;

            Ok(Link::new(Some(&description_or_link), &link))
        } else {
            self.link_close()?;

            Ok(Link::new(None, &description_or_link))
        }
    }

    fn block_end(&mut self) -> ParseResult<()> {
        let token = self.next();

        match token.token_type {
            TokenType::EOF => Ok(()),
            TokenType::StructuralLineBreak => {
                // Consume the rest of the line breaks prior to the next block.
                self.advance_past(&[TokenType::StructuralLineBreak]);

                if self.peek_match(&TokenType::EOF) {
                    self.next();
                }

                Ok(())
            }
            _ => parse_error!(
                ParseErrorType::UnexpectedToken,
                "Expected LineBreak or EOF. Found `{lexeme}` instead.",
                token
            ),
        }
    }

    fn plain_text(&mut self) -> ParseResult<String> {
        let token = self.next();

        if token.token_type == TokenType::Text {
            Ok(token.lexeme.to_string())
        } else {
            parse_error!(
                ParseErrorType::UnexpectedToken,
                "Expected plain text token. Found `{{lexeme}}` instead.",
                token
            )
        }
    }

    fn generic_delim(&mut self, type_to_match: TokenType, descriptor: &str) -> ParseResult<()> {
        let token = self.next();

        // Could make this more general with a closure that returns a result,
        // but that's not necessary right now.
        if token.token_type == type_to_match {
            Ok(())
        } else {
            let message = format!(
                "Expected `{}` token. Found `{{lexeme}}` instead.",
                descriptor
            );

            parse_error!(ParseErrorType::UnexpectedToken, &message, token)
        }
    }

    fn metadata_separator(&mut self) -> ParseResult<()> {
        self.generic_delim(TokenType::MetadataSeparator, METADATA_SEPARATOR)
    }

    fn line_break(&mut self) -> ParseResult<()> {
        self.generic_delim(TokenType::StructuralLineBreak, "LineBreak")
    }

    fn math_block_delim(&mut self) -> ParseResult<()> {
        self.generic_delim(TokenType::MathBlockDelim, MATH_BLOCK_DELIM)
    }

    fn export_block_delim(&mut self) -> ParseResult<()> {
        self.generic_delim(TokenType::ExportBlockDelim, EXPORT_BLOCK_DELIM)
    }

    fn code_block_delim(&mut self) -> ParseResult<()> {
        self.generic_delim(TokenType::CodeBlockDelim, CODE_BLOCK_DELIM)
    }

    fn literal_block_delim(&mut self) -> ParseResult<()> {
        self.generic_delim(TokenType::LiteralBlockDelim, LITERAL_BLOCK_DELIM)
    }

    fn quote_block_indicator(&mut self) -> ParseResult<()> {
        self.generic_delim(TokenType::QuoteBlockIndicator, QUOTE_BLOCK_INDICATOR)
    }

    fn inline_math_delim(&mut self) -> ParseResult<()> {
        self.generic_delim(TokenType::InlineMathDelim, INLINE_MATH_DELIM)
    }

    fn inline_code_delim(&mut self) -> ParseResult<()> {
        self.generic_delim(TokenType::InlineCodeDelim, INLINE_CODE_DELIM)
    }

    fn inline_literal_delim(&mut self) -> ParseResult<()> {
        self.generic_delim(TokenType::InlineLiteralDelim, INLINE_LITERAL_DELIM)
    }

    fn link_open(&mut self) -> ParseResult<()> {
        self.generic_delim(TokenType::LinkOpen, LINK_OPEN)
    }

    fn link_close(&mut self) -> ParseResult<()> {
        self.generic_delim(TokenType::LinkClose, LINK_CLOSE)
    }

    fn link_intermediate(&mut self) -> ParseResult<()> {
        self.generic_delim(TokenType::LinkIntermediate, LINK_INTERMEDIATE)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_metadata() {
        let tokens = vec![
            Token::new(TokenType::Text, "key", &FilePosition::new(0, 4, 4)),
            Token::new(
                TokenType::MetadataSeparator,
                METADATA_SEPARATOR,
                &FilePosition::new(0, 7, 7),
            ),
            Token::new(TokenType::Text, "value", &FilePosition::new(0, 9, 9)),
            Token::new(TokenType::EOF, "", &FilePosition::new(0, 14, 14)),
        ];

        let expected_metadata = Metadata::new("key", "value");

        let mut parser = Parser::new(&tokens);

        assert_eq!(parser.metadata().unwrap(), expected_metadata)
    }

    #[test]
    fn parses_plain_section_heading() {
        let tokens = vec![
            Token::new(TokenType::Text, "Heading", &FilePosition::new(0, 2, 2)),
            Token::new(TokenType::EOF, "", &FilePosition::new(0, 9, 9)),
        ];

        let expected_heading = SectionHeading::new(
            1,
            vec![InlineElement::Text(Text::new(TextType::Plain, "Heading"))],
        );

        let mut parser = Parser::new(&tokens);

        assert_eq!(parser.section_heading().unwrap(), expected_heading)
    }

    #[test]
    fn parses_math_block() {
        let tokens = vec![
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

        let expected_math_block = MathBlock::new(None, "math");

        let mut parser = Parser::new(&tokens);

        assert_eq!(parser.math_block().unwrap(), expected_math_block)
    }

    #[test]
    fn parses_export_block() {
        let tokens = vec![
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

        let expected_export_block = ExportBlock::new(None, "export");

        let mut parser = Parser::new(&tokens);

        assert_eq!(parser.export_block().unwrap(), expected_export_block)
    }

    #[test]
    fn parses_code_block() {
        let tokens = vec![
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

        let expected_code_block = CodeBlock::new(None, "code");

        let mut parser = Parser::new(&tokens);

        assert_eq!(parser.code_block().unwrap(), expected_code_block)
    }

    #[test]
    fn parses_literal_block() {
        let tokens = vec![
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

        let expected_literal_block = LiteralBlock::new(None, "literal");

        let mut parser = Parser::new(&tokens);

        assert_eq!(parser.literal_block().unwrap(), expected_literal_block)
    }

    #[test]
    fn parses_formatted_text() {
        let tokens = vec![
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
            Token::new(TokenType::Text, " ", &FilePosition::new(0, 8, 8)),
            Token::new(
                TokenType::ItalicDelim,
                ITALIC_DELIM,
                &FilePosition::new(0, 9, 9),
            ),
            Token::new(TokenType::Text, "italic", &FilePosition::new(0, 11, 11)),
            Token::new(
                TokenType::ItalicDelim,
                ITALIC_DELIM,
                &FilePosition::new(0, 17, 17),
            ),
            Token::new(TokenType::Text, " ", &FilePosition::new(0, 19, 19)),
            Token::new(
                TokenType::UnderlineDelim,
                UNDERLINE_DELIM,
                &FilePosition::new(0, 20, 20),
            ),
            Token::new(TokenType::Text, "underline", &FilePosition::new(0, 22, 22)),
            Token::new(
                TokenType::UnderlineDelim,
                UNDERLINE_DELIM,
                &FilePosition::new(0, 31, 31),
            ),
            Token::new(TokenType::Text, " ", &FilePosition::new(0, 33, 33)),
            Token::new(
                TokenType::StrikethroughDelim,
                STRIKETHROUGH_DELIM,
                &FilePosition::new(0, 34, 34),
            ),
            Token::new(
                TokenType::Text,
                "strikethrough",
                &FilePosition::new(0, 36, 36),
            ),
            Token::new(
                TokenType::StrikethroughDelim,
                STRIKETHROUGH_DELIM,
                &FilePosition::new(0, 49, 49),
            ),
            Token::new(TokenType::Text, " ", &FilePosition::new(0, 51, 51)),
            Token::new(
                TokenType::InlineMathDelim,
                INLINE_MATH_DELIM,
                &FilePosition::new(0, 52, 52),
            ),
            Token::new(TokenType::Text, "math", &FilePosition::new(0, 54, 54)),
            Token::new(
                TokenType::InlineMathDelim,
                INLINE_MATH_DELIM,
                &FilePosition::new(0, 58, 58),
            ),
            Token::new(TokenType::Text, " ", &FilePosition::new(0, 60, 60)),
            Token::new(
                TokenType::InlineCodeDelim,
                INLINE_CODE_DELIM,
                &FilePosition::new(0, 61, 61),
            ),
            Token::new(TokenType::Text, "code", &FilePosition::new(0, 63, 63)),
            Token::new(
                TokenType::InlineCodeDelim,
                INLINE_CODE_DELIM,
                &FilePosition::new(0, 67, 67),
            ),
            Token::new(TokenType::Text, " ", &FilePosition::new(0, 69, 69)),
            Token::new(
                TokenType::InlineLiteralDelim,
                INLINE_LITERAL_DELIM,
                &FilePosition::new(0, 70, 70),
            ),
            Token::new(TokenType::Text, "literal", &FilePosition::new(0, 72, 72)),
            Token::new(
                TokenType::InlineLiteralDelim,
                INLINE_LITERAL_DELIM,
                &FilePosition::new(0, 79, 79),
            ),
            Token::new(TokenType::EOF, "", &FilePosition::new(0, 81, 81)),
        ];

        let space_element = InlineElement::Text(Text::new(TextType::Plain, " "));

        let expected_inline_elements = vec![
            InlineElement::Text(Text::new(
                formatted_text_type!(FormatSpecifier::Bold),
                "bold",
            )),
            space_element.clone(),
            InlineElement::Text(Text::new(
                formatted_text_type!(FormatSpecifier::Italic),
                "italic",
            )),
            space_element.clone(),
            InlineElement::Text(Text::new(
                formatted_text_type!(FormatSpecifier::Underline),
                "underline",
            )),
            space_element.clone(),
            InlineElement::Text(Text::new(
                formatted_text_type!(FormatSpecifier::Strikethrough),
                "strikethrough",
            )),
            space_element.clone(),
            InlineElement::Text(Text::new(TextType::Math, "math")),
            space_element.clone(),
            InlineElement::Text(Text::new(TextType::Code, "code")),
            space_element.clone(),
            InlineElement::Text(Text::new(TextType::Literal, "literal")),
        ];

        let mut parser = Parser::new(&tokens);

        assert_eq!(parser.formatted_text().unwrap(), expected_inline_elements)
    }

    #[test]
    fn parses_text_block() {
        let tokens = vec![
            Token::new(
                TokenType::Text,
                "A text block.",
                &FilePosition::new(0, 0, 0),
            ),
            Token::new(TokenType::EOF, "", &FilePosition::new(0, 13, 13)),
        ];

        let inline_elements = vec![InlineElement::Text(Text::new(
            TextType::Plain,
            "A text block.",
        ))];

        let expected_text_block = TextBlock::new(inline_elements);

        let mut parser = Parser::new(&tokens);

        // parser.text_block() is the only parsing function that rewinds the iterator.
        // We need to advance one token to be consistent with what the function expects.
        parser.next();

        assert_eq!(parser.text_block().unwrap(), expected_text_block)
    }

    #[test]
    fn parses_quote_block() {
        let tokens = vec![
            Token::new(
                TokenType::Text,
                "A quote block.",
                &FilePosition::new(0, 0, 0),
            ),
            Token::new(TokenType::EOF, "", &FilePosition::new(0, 14, 14)),
        ];

        let inline_elements = vec![InlineElement::Text(Text::new(
            TextType::Plain,
            "A quote block.",
        ))];

        let expected_quote_block = QuoteBlock::new(inline_elements);

        let mut parser = Parser::new(&tokens);

        assert_eq!(parser.quote_block().unwrap(), expected_quote_block)
    }

    // TODO: Inline math, code, literal

    #[test]
    fn parses_link_without_description() {
        let tokens = vec![
            Token::new(TokenType::Text, "link", &FilePosition::new(0, 2, 2)),
            Token::new(
                TokenType::LinkClose,
                LINK_CLOSE,
                &FilePosition::new(0, 6, 6),
            ),
        ];

        let expected_link = Link::new(None, "link");

        let mut parser = Parser::new(&tokens);

        assert_eq!(parser.link().unwrap(), expected_link)
    }

    #[test]
    fn parses_link_with_description() {
        let tokens = vec![
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
        ];

        let expected_link = Link::new(Some("description"), "link");

        let mut parser = Parser::new(&tokens);

        assert_eq!(parser.link().unwrap(), expected_link)
    }

    #[test]
    fn parses_eof_block_end() -> ParseResult<()> {
        let tokens = vec![Token::new(TokenType::EOF, "", &FilePosition::new(0, 0, 0))];

        let mut parser = Parser::new(&tokens);

        parser.block_end()?;

        Ok(())
    }

    #[test]
    fn parses_line_break_block_end() -> ParseResult<()> {
        let tokens = vec![
            Token::new(
                TokenType::StructuralLineBreak,
                "\n",
                &FilePosition::new(0, 0, 0),
            ),
            Token::new(TokenType::EOF, "", &FilePosition::new(1, 0, 1)),
        ];

        let mut parser = Parser::new(&tokens);

        parser.block_end()?;

        Ok(())
    }
}
