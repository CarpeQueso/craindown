use crate::craindown::*;
use crate::error::{ParseError, ParseErrorType};
use crate::lex::*;
use crate::parse_error;
use crate::util::FilePosition;

pub fn parse(tokens: &[Token]) -> Result<Craindown, (Craindown, Vec<ParseError>)> {
    let mut parser = Parser::new(&tokens);

    parser.parse()
}

pub type ParseResult<T> = Result<T, ParseError>;

struct Parser<'a> {
    tokens: &'a [Token],
    idx: usize,
    expressions: Vec<BlockElement>,
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
            expressions: Vec::new(),
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

    fn advance_match(&mut self, types_to_match: &[TokenType]) -> bool {
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
        // An empty file is a valid file, so we should check for that before continuing.
        if self.peek_match(&TokenType::EOF) {
            return Ok(Craindown::new());
        }

        while !self.is_at_end() {
            match self.block() {
                Ok(block) => self.expressions.push(block),
                Err(err) => {
                    self.errors.push(err);
                    // Advance the iterator to a point where it can continue parsing.
                    self.synchronize();
                }
            };
        }

        // TODO: Do we need some kind of intermediate form before the final return
        // value is generated?

        Ok(Craindown::new())
    }

    fn block(&mut self) -> ParseResult<BlockElement> {
        let token = self.next();

        match &token.token_type {
            TokenType::MetadataIndicator => self.metadata(),
            TokenType::SectionHeadingIndicator => self.section_heading(),
            TokenType::MathBlockDelim => self.math_block(),
            TokenType::ExportBlockDelim => self.export_block(),
            TokenType::CodeBlockDelim => self.code_block(),
            TokenType::LiteralBlockDelim => self.literal_block(),
            TokenType::QuoteBlockDelim => self._quote_block(),
            TokenType::InlineMathDelim
            | TokenType::InlineCodeDelim
            | TokenType::InlineLiteralDelim
            | TokenType::BoldDelim
            | TokenType::ItalicDelim
            | TokenType::UnderlineDelim
            | TokenType::StrikethroughDelim
            | TokenType::LinkOpen
            | TokenType::Text => self.text_block(),
            _ => parse_error!(ParseErrorType::UnexpectedToken, "", token), // TODO
        }
    }

    fn metadata(&mut self) -> ParseResult<BlockElement> {
        let key = self.plain_text()?;

        self.metadata_separator()?;

        let value = self.plain_text()?;

        self.block_end()?;

        Ok(BlockElement::Metadata(Metadata::new(&key, &value)))
    }

    fn section_heading(&mut self) -> ParseResult<BlockElement> {
        // Start at 1. We've already seen one heading token to get here.
        let mut level: usize = 1;

        while self.peek_match(&TokenType::SectionHeadingIndicator) {
            self.next();
            level += 1;
        }

        // let content = self.formatted_text()?;

        Ok(BlockElement::SectionHeading(
            SectionHeading::new(Vec::new()),
        ))
    }

    fn math_block(&mut self) -> ParseResult<BlockElement> {
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

        Ok(BlockElement::MathBlock(MathBlock::new(options, contents)))
    }

    fn export_block(&mut self) -> ParseResult<BlockElement> {
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

        Ok(BlockElement::ExportBlock(ExportBlock::new(
            options, contents,
        )))
    }

    fn code_block(&mut self) -> ParseResult<BlockElement> {
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

        Ok(BlockElement::CodeBlock(CodeBlock::new(options, contents)))
    }

    fn literal_block(&mut self) -> ParseResult<BlockElement> {
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

        Ok(BlockElement::LiteralBlock(LiteralBlock::new(
            options, contents,
        )))
    }

    fn _quote_block(&mut self) -> ParseResult<BlockElement> {
        Ok(BlockElement::QuoteBlock(QuoteBlock::new()))
    }

    fn text_block(&mut self) -> ParseResult<BlockElement> {
        Ok(BlockElement::TextBlock(TextBlock::new(Vec::new())))
    }

    fn formatted_text(&mut self) -> ParseResult<()> {
        Ok(())
    }

    fn link(&mut self) -> ParseResult<()> {
        let description_or_link = self.plain_text()?;

        if self.advance_match(&[TokenType::LinkIntermediate]) {
            let link = self.plain_text()?;

            self.link_close()?;

            // Return link with description.
            Ok(())
        } else {
            self.link_close()?;

            // Return link without description.
            Ok(())
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

    fn _quote_block_delim(&mut self) -> ParseResult<()> {
        self.generic_delim(TokenType::QuoteBlockDelim, QUOTE_BLOCK_DELIM)
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
