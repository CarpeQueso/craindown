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
    expressions: Vec<Expr>,
    errors: Vec<ParseError>,
}

macro_rules! validate_single_token {
    ($parser:ident, $token_type:expr, $token_descriptor:expr) => {
        if let Some(token) = $parser.next() {
            if token.token_type == $token_type {
                Ok(())
            } else {
                let message = format!(
                    "Expected `{}` token. Found `{{lexeme}}` instead.",
                    $token_descriptor
                );

                parse_error!(ParseErrorType::UnexpectedToken, &message, token)
            }
        } else {
            let prev_token = $parser.prev().unwrap();
            let message = format!(
                "Expected `{}` token, but nothing was there...",
                $token_descriptor
            );

            parse_error!(
                ParseErrorType::UnexpectedEndOfTokenStream,
                &message,
                prev_token
            )
        }
    };
}

impl<'a> Iterator for Parser<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx >= self.tokens.len() {
            None
        } else {
            let token = &self.tokens[self.idx];

            self.idx += 1;

            Some(token)
        }
    }
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Parser {
            tokens,
            idx: 0,
            expressions: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn prev(&mut self) -> Option<&Token> {
        if self.idx == 0 {
            None
        } else {
            Some(&self.tokens[self.idx - 1])
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        if self.idx >= self.tokens.len() {
            None
        } else {
            Some(&self.tokens[self.idx])
        }
    }

    fn peek_match(&mut self, token_type: TokenType) -> bool {
        if let Some(token) = self.peek() {
            if token.token_type == token_type {
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    /// Continue advancing the iterator while the next token's type is contained in
    /// `types_to_match`.
    ///
    /// The matching token will not be consumed.
    fn advance_past(&mut self, types_to_match: &[TokenType]) {
        'outer: while let Some(token) = self.peek() {
            for type_to_match in types_to_match {
                if token.token_type == *type_to_match {
                    self.next();
                    continue 'outer;
                }
            }

            break;
        }
    }

    /// Continue advancing the iterator _until_ the next token's type is contained in
    /// `types_to_match`.
    ///
    /// The matching token will not be consumed.
    fn advance_until(&mut self, types_to_match: &[TokenType]) {
        while let Some(token) = self.peek() {
            for type_to_match in types_to_match {
                if token.token_type == *type_to_match {
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
        if let Some(token) = self.peek() {
            if token.token_type == TokenType::EOF {
                return Ok(Craindown::new());
            }
        } else {
            // If the token list is empty, we should return some kind of error.
        }

        while let Some(_) = self.peek() {
            match self.expr() {
                Ok(expr) => self.expressions.push(expr),
                Err(err) => self.errors.push(err),
            };
        }

        // TODO: Do we need some kind of intermediate form before the final return
        // value is generated?

        Ok(Craindown::new())
    }

    fn expr(&mut self) -> ParseResult<Expr> {
        if let Some(token) = self.next() {
            match &token.token_type {
                TokenType::MetadataIndicator => self.metadata(),
                TokenType::SectionHeadingIndicator => self.section_heading(),
                TokenType::MathBlockDelim => self.math_block(),
                TokenType::ExportBlockDelim => self.export_block(),
                TokenType::CodeBlockDelim => self.code_block(),
                TokenType::LiteralBlockDelim => self.literal_block(),
                TokenType::QuoteBlockDelim => self.quote_block(),
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
        } else {
            // TODO: Should the previous token be the one we put into this error?
            let prev_token = self.prev().unwrap();
            parse_error!(ParseErrorType::UnexpectedEndOfTokenStream, "", prev_token)
        }
    }

    fn metadata(&mut self) -> ParseResult<Expr> {
        let key = self.plain_text()?.to_string();

        self.metadata_separator()?;

        let value = self.plain_text()?.to_string();

        self.block_end()?;

        Ok(Expr::Metadata(Metadata::new(&key, &value)))
    }

    fn section_heading(&mut self) -> ParseResult<Expr> {
        // Start at 1. We've already seen one heading token to get here.
        let mut level: usize = 1;

        while self.peek_match(TokenType::SectionHeadingIndicator) {
            self.next();
            level += 1;
        }

        // let content = self.formatted_text()?;

        Ok(Expr::SectionHeading(SectionHeading::new()))
    }

    fn math_block(&mut self) -> ParseResult<Expr> {
        let options = if self.peek_match(TokenType::Text) {
            let s = self.plain_text()?.to_string();
            Some(s)
        } else {
            None
        };

        self.line_break()?;

        let contents = self.plain_text()?.to_string();

        self.line_break()?;

        Ok(Expr::MathBlock(MathBlock::new()))
    }

    fn export_block(&mut self) -> ParseResult<Expr> {
        Ok(Expr::ExportBlock(ExportBlock::new()))
    }

    fn code_block(&mut self) -> ParseResult<Expr> {
        Ok(Expr::CodeBlock(CodeBlock::new()))
    }

    fn literal_block(&mut self) -> ParseResult<Expr> {
        Ok(Expr::LiteralBlock(LiteralBlock::new()))
    }

    fn quote_block(&mut self) -> ParseResult<Expr> {
        Ok(Expr::QuoteBlock(QuoteBlock::new()))
    }

    fn text_block(&mut self) -> ParseResult<Expr> {
        Ok(Expr::TextBlock(TextBlock::new()))
    }

    fn block_end(&mut self) -> ParseResult<()> {
        if let Some(token) = self.next() {
            match token.token_type {
                TokenType::EOF => Ok(()),
                TokenType::StructuralLineBreak => {
                    // Consume the rest of the line breaks prior to the next block.
                    while self.peek_match(TokenType::StructuralLineBreak) {
                        self.next();
                    }

                    if self.peek_match(TokenType::EOF) {
                        self.next();
                    }

                    Ok(())
                }
                _ => parse_error!(
                    ParseErrorType::UnexpectedToken,
                    "Expected line break or EOF. Found `{lexeme}` instead.",
                    token
                ),
            }
        } else {
            let prev_token = self.prev().unwrap();

            parse_error!(
                ParseErrorType::UnexpectedToken,
                "Expected line break or EOF, but nothing was there...",
                prev_token
            )
        }
    }

    fn plain_text(&mut self) -> ParseResult<&str> {
        if let Some(token) = self.next() {
            if token.token_type == TokenType::Text {
                Ok(&token.lexeme)
            } else {
                parse_error!(
                    ParseErrorType::UnexpectedToken,
                    "Expected plain text token. Found `{{lexeme}}` instead.",
                    token
                )
            }
        } else {
            let prev_token = self.prev().unwrap();

            parse_error!(
                ParseErrorType::UnexpectedEndOfTokenStream,
                "Expected plain text token, but nothing was there...",
                prev_token
            )
        }
    }

    fn metadata_separator(&mut self) -> ParseResult<()> {
        validate_single_token!(self, TokenType::MetadataSeparator, METADATA_SEPARATOR)
    }

    fn line_break(&mut self) -> ParseResult<()> {
        validate_single_token!(self, TokenType::StructuralLineBreak, "LineBreak")
    }
}
