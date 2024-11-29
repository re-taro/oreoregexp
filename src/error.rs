use crate::lexer::Token;

#[derive(thiserror::Error, Debug)]
pub(crate) enum Error {
    #[error("unexpected character: {0}")]
    UnexpectedChar(Token),
    #[error("expected {0}")]
    Expected(Token),
}

pub(crate) type Result<T> = std::result::Result<T, Error>;
