use crate::regex::at_beginning;
use regex::Regex;

type TokenCreatorFn<Token> = dyn Fn() -> Token;
type TokenTransformerFn<Token> = dyn Fn(&str, usize) -> LexResult<Token>;

// TODO: remove LexRules and instead use deterministic finite automata
pub enum LexRule<Token> {
    Exact {
        pattern: String,
        handler: Box<TokenCreatorFn<Token>>,
    },
    Regex {
        regex: Regex,
        handler: Box<TokenTransformerFn<Token>>,
    },
}

impl<Token> LexRule<Token> {
    pub fn new_exact(pattern: String, handler: Box<TokenCreatorFn<Token>>) -> Self {
        LexRule::Exact { pattern, handler }
    }

    pub fn new_regex(regex: &str, handler: Box<TokenTransformerFn<Token>>) -> Self {
        LexRule::Regex {
            regex: at_beginning(regex),
            handler,
        }
    }

    pub fn matches(&self, input: &str) -> Option<usize> {
        match &self {
            LexRule::Exact { pattern, .. } => input.starts_with(pattern).then_some(pattern.len()),
            LexRule::Regex { regex, .. } => regex
                .captures(input)
                .and_then(|captures| captures.get(0))
                .map(|matched| matched.end() - matched.start()),
        }
    }

    pub fn consume<'a>(&self, input: &'a str, matched_size: usize) -> LexResult<(Token, &'a str)> {
        match &self {
            LexRule::Exact { handler, .. } => Ok((handler(), &input[matched_size..])),
            LexRule::Regex { handler, .. } => {
                handler(input, matched_size).map(|token| (token, &input[matched_size..]))
            }
        }
    }
}

#[derive(Debug)]
pub enum LexError {
    UnknownPattern(String),
    UnparsableToken(String),
}

pub struct LexerData<T> {
    pub rules: Vec<LexRule<T>>,
}

impl<T> LexerData<T> {
    pub fn new(rules: Vec<LexRule<T>>) -> Self {
        Self { rules }
    }
}

impl LexError {
    pub fn unknown(input: &str) -> LexError {
        LexError::UnknownPattern(input.into())
    }

    pub fn unparsable(input: &str) -> LexError {
        LexError::UnparsableToken(input.into())
    }
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::UnknownPattern(input) => write!(f, "Unknown pattern: {}", input),
            LexError::UnparsableToken(input) => write!(f, "Unparsable token: {}", input),
        }
    }
}

impl std::error::Error for LexError {}

pub type LexResult<T> = Result<T, LexError>;

#[cfg(debug_assertions)]
#[derive(Debug, Clone)]
pub struct TokenDebugInfo {
    pub inpup_offset: usize,
    pub matched_rule: String,
    pub matched: String,
    pub matched_size: usize,
}
