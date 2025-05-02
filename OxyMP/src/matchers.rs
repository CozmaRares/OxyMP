#[derive(Debug)]
pub enum TokenizeError {
    NumberParseError(std::num::ParseFloatError),
}

impl std::error::Error for TokenizeError {}
impl std::fmt::Display for TokenizeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenizeError::NumberParseError(e) => write!(f, "Number parse error: {}", e),
        }
    }
}

pub fn match_number(input: &str) -> Result<f64, TokenizeError> {
    input.parse().map_err(TokenizeError::NumberParseError)
}

pub fn match_ident(input: &str) -> Result<String, TokenizeError> {
    Ok(input.to_string())
}
