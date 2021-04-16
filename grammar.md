# Grammar (Draft)

Remember, the lexer helps to clean up whitespace when the whitespace isn't a structural feature, so we can worry primarily about grammar.

```
Craindown -> Expr* + EOF

Expr -> Metadata
        | Header
        | TextBlock
        | MathBlock
        | ExportBlock
        | CodeBlock
        | LiteralBlock
        | QuoteBlock???

BlockEnd -> EOF | (LineBreak + LineBreak*)

Metadata -> MetadataIndicator + [^ \t\n:] + MetadataSeparator + [???] + BlockEnd

Header -> HeaderIndicator{1,6} + (TextExpr+) + BlockEnd

TextBlock -> (TextExpr+)

TextExpr -> PlainText
            | InlineMath
            | InlineCode
            | InlineLiteral
            | Link
            | FormattedText

InlineMath -> InlineMathDelim + PlainText + InlineMathDelim
// InlineCode and InlineLiteral have the same structure.

Link -> LinkWithDescription | LinkWithoutDescription

LinkWithDescription -> LinkOpen + PlainText + LinkIntermediate + PlainText + LinkClose

LinkWithoutDescription -> LinkOpen + PlainText + LinkClose

FormattedText -> BoldText
                 | ItalicText
                 | UnderlinedText
                 | StrikethroughText
                 | PlainText

// TODO: Decide whether it's good/necessary to recognize interspersed delims instead of only
// nested ones...
BoldText -> BoldDelim + FormattedText + BoldDelim
// Same with Italic, Underlined, and Strikethrough

// PlainText is a bit tricky. It may contain the characters that make up delimiters,
// but only in circumstances (e.g. math blocks) where the lexer knows to ignore them.
// If something is marked as plain text rather than a delimiter, trust that it should be
// marked that way.
PlainText -> <Text with no marked delimiters>

MathBlock -> MathBlockWithOptions | MathBlockWithoutOptions

MathBlockWithOptions -> MathBlockDelim + PlainText + LineBreak + PlainText + LineBreak + MathBlockDelim + BlockEnd

MathBlockWithoutOptions -> MathBlockDelim + LineBreak + PlainText + LineBreak + MathBlockDelim + BlockEnd

// The following have the same structure as MathBlock
ExportBlock -> ExportBlockWithOptions | ExportBlockWithoutOptions
CodeBlock -> CodeBlockWithOptions | CodeBlockWithoutOptions
LiteralBlock -> LiteralBlockWithOptions | LiteralBlockWithoutOptions // Does this need options?

QuoteBlock -> I really don't know what to do with you...
