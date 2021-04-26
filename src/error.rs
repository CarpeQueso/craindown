use crate::util::FilePosition;

#[macro_export]
macro_rules! parse_error {
    ($error_type:expr, $error_msg:expr, $token:ident) => {
        Err(ParseError::new(
            $error_type,
            $error_msg,
            &$token.lexeme,
            &$token.position,
        ))
    };
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub error_type: ParseErrorType,
    pub error_msg: String,
    pub lexeme: String,
    pub position: FilePosition,
}

impl ParseError {
    pub fn new(
        error_type: ParseErrorType,
        error_msg: &str,
        lexeme: &str,
        position: &FilePosition,
    ) -> Self {
        ParseError {
            error_type,
            error_msg: error_msg.to_string(),
            lexeme: lexeme.to_string(),
            position: position.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseErrorType {
    UnexpectedToken,
    UnexpectedEOF,
    UnclosedFormatSpecifier,
    UnexpectedEndOfTokenStream,
}
