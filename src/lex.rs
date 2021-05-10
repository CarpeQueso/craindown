use std::fmt;

use crate::symbols::*;
use crate::util::FilePosition;

pub fn tokenize(s: &str) -> Vec<Token> {
    let mut scanner = Scanner::new(&s);
    let tokens = scanner.scan_tokens();

    tokens
}

#[derive(Clone, PartialEq)]
pub struct Token {
    /// The token type also holds any type-specific data.
    pub token_type: TokenType,
    pub lexeme: String,
    pub position: FilePosition,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Token{{{:?},\"{}\",{}}}",
            self.token_type, self.lexeme, self.position
        )
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Token{{{:?},\"{}\",{}}}",
            self.token_type, self.lexeme, self.position
        )
    }
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: &str, position: &FilePosition) -> Self {
        Token {
            token_type,
            lexeme: lexeme.to_string(),
            position: position.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    // TODO: I don't know if "Indicator" is sufficiently descriptive...
    MetadataIndicator,
    MetadataSeparator,
    SectionHeadingIndicator,
    MathBlockDelim,
    ExportBlockDelim,
    CodeBlockDelim,
    LiteralBlockDelim,
    QuoteBlockIndicator,
    InlineMathDelim,
    InlineCodeDelim,
    InlineLiteralDelim,
    BoldDelim,
    ItalicDelim,
    UnderlineDelim,
    StrikethroughDelim,
    LinkOpen,
    LinkClose,
    LinkIntermediate,
    Text,
    StructuralLineBreak,
    StructuralSpace,
    StructuralTab,
    EOF,
}

struct MatchHandler {
    // When you match this string...
    pub string_to_match: String,
    // ... push this token...
    pub token_type: TokenType,
    // ... and handle any context-dependent processing this way.
    pub handler_fn: fn(&mut Scanner),
}

impl MatchHandler {
    fn new(str_to_match: &str, token_type: TokenType, handler_fn: fn(&mut Scanner)) -> Self {
        MatchHandler {
            string_to_match: str_to_match.to_string(),
            token_type,
            handler_fn,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct Scanner {
    data: Vec<char>,
    tokens: Vec<Token>,
    idx: usize,
    position: FilePosition,
    start_of_line: bool,
    // Separate state to handle anything that isn't a "keyword"
    current_string: String,
    current_string_position: FilePosition,
}

impl Iterator for Scanner {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx >= self.data.len() {
            None
        } else {
            let c = self.data[self.idx];

            // Keep track of the file position here so
            // that we don't have to do it anywhere else.
            if c == '\n' {
                self.position.line += 1;
                self.position.position_in_line = 0;
                self.start_of_line = true;
            } else {
                self.position.position_in_line += 1;
                self.start_of_line = false;
            }

            self.idx += 1;
            self.position.absolute_position += 1;

            Some(c)
        }
    }
}

// Note, don't manually advance the scanner. Call next().
// Just use scanner position for lookahead.
impl Scanner {
    fn new(data: &str) -> Self {
        Self {
            data: data.chars().collect(),
            tokens: Vec::new(),
            idx: 0,
            position: FilePosition::new(0, 0, 0),
            start_of_line: true,
            current_string: String::new(),
            current_string_position: FilePosition::new(0, 0, 0),
        }
    }

    // This is a big hairball, but I don't know if it can be broken up...
    fn match_and_maybe_process(
        &mut self,
        str_to_match: &str,
        token_type: &TokenType,
        handler_fn: fn(&mut Scanner),
    ) -> bool {
        if !self.peek_match(str_to_match) {
            return false;
        }
        // If we get here, we have a match, so we continue with token processing.

        // If text which isn't a reserved "keyword" has accumulated up to this point,
        // we should push it as a text token before going on with additional processing.
        if self.current_string.len() > 0 {
            self.push_text_token();
        }

        // Now push the token that matches the string we were looking for.
        self.push_token(token_type.clone(), str_to_match);

        // Advance past that token's position.
        self.advance(str_to_match.len());

        // Update the start of the next text accumulator.
        self.set_text_start_position();

        // Do any additional processing logic that's context dependent. This may advance
        // the iterator, and that's fine.
        handler_fn(self);

        // Update again in case the handler function pushed more text.
        self.set_text_start_position();

        true
    }

    /// Advances the scanner n elements or to the end.
    fn advance(&mut self, n: usize) {
        let l = self.len_to_end();
        let advance_by = if n < l { n } else { l };

        for _ in 0..advance_by {
            self.next();
        }
    }

    fn len_to_end(&mut self) -> usize {
        self.data.len() - self.idx
    }

    /// In our case, `next()` has side effects, so I'm adding my own side-effect-free
    /// peek function instead of using a native peekable iterator.
    fn peek(&mut self) -> Option<char> {
        if self.idx >= self.data.len() {
            return None;
        }

        Some(self.data[self.idx])
    }

    fn peek_match(&mut self, str_to_match: &str) -> bool {
        let mut peek_idx = self.idx;

        // Check whether the given string's unicode scalars match the unicode scalars
        // at the current scanner position.
        for c in str_to_match.chars() {
            if peek_idx >= self.data.len() || c != self.data[peek_idx] {
                return false;
            }

            peek_idx += 1;
        }

        true
    }

    fn push_token(&mut self, token_type: TokenType, lexeme: &str) {
        self.tokens
            .push(Token::new(token_type, &lexeme.to_string(), &self.position))
    }

    fn push_text_token(&mut self) {
        self.tokens.push(Token::new(
            TokenType::Text,
            self.current_string.as_str(),
            &self.current_string_position,
        ));

        self.current_string.clear();
    }

    fn set_text_start_position(&mut self) {
        self.current_string_position = self.position.clone();
    }

    fn has_next(&mut self) -> bool {
        if let Some(_) = self.peek() {
            true
        } else {
            false
        }
    }

    /// Advance past any whitespace that isn't '\r' or '\n'
    fn advance_past_inline_whitespace(&mut self) {
        if let Some(c) = self.peek() {
            // TODO: There are other unicode whitespace characters. Do we care about them?
            // '\u{000B}' '\u{000C}' '\u{0085}' '\u{200E}' '\u{200F}' '\u{2028}' '\u{2029}'
            if c == ' ' || c == '\t' {
                self.next();
            }
        }
    }

    fn collect_until(&mut self, patterns: &[&str]) {
        'outer: while self.has_next() {
            for pattern in patterns {
                if self.peek_match(pattern) {
                    break 'outer;
                }
            }

            if let Some(c) = self.next() {
                self.current_string.push(c);
            }
        }
    }

    fn match_and_push_subset(&mut self, patterns_and_tokens: &[(&str, TokenType)]) {
        while self.has_next() {
            for (pattern, token_type) in patterns_and_tokens {
                if self.peek_match(pattern) {
                    self.push_token(token_type.clone(), pattern);
                    self.advance(pattern.len());
                    continue;
                }
            }

            // If none of the patterns match, we're done.
            break;
        }
    }

    fn scan_tokens(&mut self) -> Vec<Token> {
        // These handlers will be processed in order. If any keywords are prefixes of others,
        // order them appropriately so that the longer keywords are checked first.
        let start_of_line_match_handlers = vec![
            MatchHandler::new(METADATA_INDICATOR, TokenType::MetadataIndicator, metadata),
            MatchHandler::new(
                SECTION_HEADING_INDICATOR,
                TokenType::SectionHeadingIndicator,
                section_heading,
            ),
            MatchHandler::new(MATH_BLOCK_DELIM, TokenType::MathBlockDelim, math_block),
            MatchHandler::new(
                EXPORT_BLOCK_DELIM,
                TokenType::ExportBlockDelim,
                export_block,
            ),
            MatchHandler::new(CODE_BLOCK_DELIM, TokenType::CodeBlockDelim, code_block),
            MatchHandler::new(
                LITERAL_BLOCK_DELIM,
                TokenType::LiteralBlockDelim,
                literal_block,
            ),
            MatchHandler::new(
                QUOTE_BLOCK_INDICATOR,
                TokenType::QuoteBlockIndicator,
                quote_block,
            ),
            MatchHandler::new(SPACE, TokenType::StructuralSpace, preceding_whitespace),
            MatchHandler::new(TAB, TokenType::StructuralTab, preceding_whitespace),
        ];

        let context_independent_match_handlers = vec![
            MatchHandler::new(INLINE_MATH_DELIM, TokenType::InlineMathDelim, inline_math),
            MatchHandler::new(INLINE_CODE_DELIM, TokenType::InlineCodeDelim, inline_code),
            MatchHandler::new(
                INLINE_LITERAL_DELIM,
                TokenType::InlineLiteralDelim,
                inline_literal,
            ),
            MatchHandler::new(BOLD_DELIM, TokenType::BoldDelim, no_op),
            MatchHandler::new(ITALIC_DELIM, TokenType::ItalicDelim, no_op),
            MatchHandler::new(UNDERLINE_DELIM, TokenType::UnderlineDelim, no_op),
            MatchHandler::new(STRIKETHROUGH_DELIM, TokenType::StrikethroughDelim, no_op),
            MatchHandler::new(LINK_OPEN, TokenType::LinkOpen, link),
            MatchHandler::new(WINDOWS_LINE_BREAK, TokenType::StructuralLineBreak, no_op),
            MatchHandler::new(LINE_BREAK, TokenType::StructuralLineBreak, no_op),
            // TODO: There are other unicode line break characters. Do we care about them?
            // '\u{000B}' '\u{000C}' '\u{0085}' '\u{200E}' '\u{200F}' '\u{2028}' '\u{2029}'
        ];

        // After scanning, the tokens vector will contain at minimum an EOF token.
        // So we use len() == 0 as a proxy for "hasn't been scanned yet".
        if self.tokens.len() == 0 {
            while self.idx < self.data.len() {
                if self.start_of_line {
                    // At the start of a line, we could match anything, including tokens that
                    // can only exist at the start of the line.
                    if self.check_match_handlers(&start_of_line_match_handlers) {
                        continue;
                    }

                    if self.check_match_handlers(&context_independent_match_handlers) {
                        continue;
                    }
                } else {
                    if self.check_match_handlers(&context_independent_match_handlers) {
                        continue;
                    }
                }

                // If we get here, none of the keywords matched, so we should store
                // this scalar as regular text.
                if let Some(c) = self.next() {
                    self.current_string.push(c);
                }
            }

            if self.current_string.len() > 0 {
                self.push_text_token();
            }

            self.push_token(TokenType::EOF, "");
        }

        self.tokens.clone()
    }

    fn check_match_handlers(&mut self, handlers: &[MatchHandler]) -> bool {
        for handler in handlers {
            if self.match_and_maybe_process(
                handler.string_to_match.as_str(),
                &handler.token_type,
                handler.handler_fn,
            ) {
                return true;
            }
        }

        false
    }
}

fn metadata(scanner: &mut Scanner) {
    scanner.advance_past_inline_whitespace();

    scanner.set_text_start_position();

    // Collect until we reach whitespace or a separator between a key, value pair.
    scanner.collect_until(&[
        METADATA_SEPARATOR,
        WINDOWS_LINE_BREAK,
        LINE_BREAK,
        SPACE,
        TAB,
    ]);

    if scanner.current_string.len() > 0 {
        scanner.push_text_token();
    }

    scanner.advance_past_inline_whitespace();

    if scanner.peek_match(METADATA_SEPARATOR) {
        scanner.push_token(TokenType::MetadataSeparator, METADATA_SEPARATOR);

        scanner.advance(METADATA_SEPARATOR.len());

        scanner.advance_past_inline_whitespace();

        scanner.set_text_start_position();
        // Keys must not have whitespace, but values can.
        scanner.collect_until(&[LINE_BREAK, WINDOWS_LINE_BREAK]);

        if scanner.current_string.len() > 0 {
            scanner.push_text_token();
        }

        scanner.advance_past_inline_whitespace();
    }
}

fn section_heading(scanner: &mut Scanner) {
    // At this point, we've already discovered one section heading token
    // We need to see if there are more.
    while scanner.peek_match(SECTION_HEADING_INDICATOR) {
        scanner.push_token(
            TokenType::SectionHeadingIndicator,
            SECTION_HEADING_INDICATOR,
        );

        scanner.next();
    }

    scanner.advance_past_inline_whitespace();

    // We don't push the text itself, because we want inline tokens in the heading
    // to be processed as well.
}

fn handle_verbatim_block(
    scanner: &mut Scanner,
    block_delim_token_type: TokenType,
    block_delim_pattern: &str,
) {
    scanner.advance_past_inline_whitespace();

    scanner.collect_until(&LINE_BREAK_PATTERNS);

    if scanner.current_string.len() > 0 {
        scanner.push_text_token();
    }

    // .collect_until could stop at the end of the text,
    if scanner.peek_match(WINDOWS_LINE_BREAK) {
        scanner.push_token(TokenType::StructuralLineBreak, WINDOWS_LINE_BREAK);
        scanner.advance(WINDOWS_LINE_BREAK.len());
    } else if scanner.peek_match(LINE_BREAK) {
        scanner.push_token(TokenType::StructuralLineBreak, &LINE_BREAK);
        scanner.advance(LINE_BREAK.len());
    } else {
        // Not what we expected... Just return control to the main loop.
        return;
    }

    scanner.set_text_start_position();
    scanner.collect_until(&[
        &[LINE_BREAK, block_delim_pattern].concat(),
        &[WINDOWS_LINE_BREAK, block_delim_pattern].concat(),
    ]);

    if scanner.current_string.len() > 0 {
        scanner.push_text_token();
    }

    if scanner.peek_match(WINDOWS_LINE_BREAK) {
        scanner.push_token(TokenType::StructuralLineBreak, WINDOWS_LINE_BREAK);
        scanner.advance(WINDOWS_LINE_BREAK.len());
    } else if scanner.peek_match(LINE_BREAK) {
        scanner.push_token(TokenType::StructuralLineBreak, &LINE_BREAK);
        scanner.advance(LINE_BREAK.len());
    }

    if scanner.peek_match(block_delim_pattern) {
        scanner.push_token(block_delim_token_type, block_delim_pattern);
        scanner.advance(block_delim_pattern.len());
    }
}

fn math_block(scanner: &mut Scanner) {
    handle_verbatim_block(scanner, TokenType::MathBlockDelim, MATH_BLOCK_DELIM);
}

fn export_block(scanner: &mut Scanner) {
    handle_verbatim_block(scanner, TokenType::ExportBlockDelim, EXPORT_BLOCK_DELIM);
}

fn code_block(scanner: &mut Scanner) {
    handle_verbatim_block(scanner, TokenType::CodeBlockDelim, CODE_BLOCK_DELIM);
}

fn literal_block(scanner: &mut Scanner) {
    handle_verbatim_block(scanner, TokenType::LiteralBlockDelim, LITERAL_BLOCK_DELIM);
}

fn quote_block(scanner: &mut Scanner) {
    // The quote block is introduced with a '>' character. After that character, we skip the
    // inline whitespace before the start of the content, and then we're done, because we
    // want to continue on with parsing (text formatting could exist in a quote block).
    scanner.advance_past_inline_whitespace();
}

fn preceding_whitespace(scanner: &mut Scanner) {
    let patterns_and_tokens: [(&str, TokenType); 2] = [
        (SPACE, TokenType::StructuralSpace),
        (TAB, TokenType::StructuralTab),
    ];

    scanner.match_and_push_subset(&patterns_and_tokens);
}

fn handle_verbatim_inline(scanner: &mut Scanner, delim_token_type: TokenType, delim_pattern: &str) {
    scanner.collect_until(&[delim_pattern, WINDOWS_LINE_BREAK, LINE_BREAK]);

    if scanner.current_string.len() > 0 {
        scanner.push_text_token();
    }

    if scanner.peek_match(delim_pattern) {
        scanner.push_token(delim_token_type, delim_pattern);
        scanner.advance(delim_pattern.len());
    }
}

fn inline_math(scanner: &mut Scanner) {
    handle_verbatim_inline(scanner, TokenType::InlineMathDelim, INLINE_MATH_DELIM);
}

fn inline_code(scanner: &mut Scanner) {
    handle_verbatim_inline(scanner, TokenType::InlineCodeDelim, INLINE_CODE_DELIM);
}

fn inline_literal(scanner: &mut Scanner) {
    handle_verbatim_inline(scanner, TokenType::InlineLiteralDelim, INLINE_LITERAL_DELIM);
}

fn link(scanner: &mut Scanner) {
    // At this point, we've already pushed a LinkOpen token and we need to
    // process the rest.
    scanner.collect_until(&[
        LINK_INTERMEDIATE,
        LINK_CLOSE,
        LINE_BREAK,
        WINDOWS_LINE_BREAK,
    ]);

    scanner.push_text_token();

    if scanner.peek_match(LINK_INTERMEDIATE) {
        scanner.push_token(TokenType::LinkIntermediate, LINK_INTERMEDIATE);
        scanner.advance(LINK_INTERMEDIATE.len());

        scanner.set_text_start_position();
        scanner.collect_until(&[LINK_CLOSE, LINE_BREAK, WINDOWS_LINE_BREAK]);
        scanner.push_text_token();
    }

    if scanner.peek_match(LINK_CLOSE) {
        scanner.push_token(TokenType::LinkClose, LINK_CLOSE);
        scanner.advance(LINK_CLOSE.len());
    }
}

fn no_op(_: &mut Scanner) {}

#[cfg(test)]
mod tests {
    use super::*;

    // TODO: It's good that the const patterns at the top are used for tokens,
    // but is there a good way to also programmatically set the expected tokens'
    // positions, too, based on the keyword lengths? That might be too generalized.

    // Metadata
    #[test]
    fn tokenizes_metadata() {
        let s = [METADATA_INDICATOR, " key", METADATA_SEPARATOR, " value"].concat();

        let tokens = super::tokenize(&s);

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

        let tokens = super::tokenize(&s);

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

        let tokens = super::tokenize(&s);

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

        let tokens = super::tokenize(&s);

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

        let tokens = super::tokenize(&s);

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

        let tokens = super::tokenize(&s);

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

        let tokens = super::tokenize(&s);

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

        let tokens = super::tokenize(&s);

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
    fn tokenizes_inline_bold_delimiters() {
        let s = [BOLD_DELIM, "bold", BOLD_DELIM].concat();

        let tokens = super::tokenize(&s);

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

        let tokens = super::tokenize(&s);

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

        let tokens = super::tokenize(&s);

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

        let tokens = super::tokenize(&s);

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

        let tokens = super::tokenize(&s);

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

        let tokens = super::tokenize(&s);

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

        let tokens = super::tokenize(&s);

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

        let tokens = super::tokenize(&s);

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

        let tokens = super::tokenize(&s);

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

        let tokens = super::tokenize(&s);

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

        let tokens = super::tokenize(&s);

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

        let tokens = super::tokenize(&s);

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

        let tokens = super::tokenize(&s);

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

        let tokens = super::tokenize(&s);

        let expected_tokens = vec![
            Token::new(TokenType::StructuralTab, TAB, &FilePosition::new(0, 0, 0)),
            Token::new(TokenType::StructuralTab, TAB, &FilePosition::new(0, 1, 1)),
            Token::new(TokenType::EOF, "", &FilePosition::new(0, 2, 2)),
        ];

        assert_eq!(tokens, expected_tokens);
    }
}
