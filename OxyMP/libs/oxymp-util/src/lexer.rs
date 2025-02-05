use regex::Regex;

pub struct LexerState<'a> {
    input: &'a str,
    cursor: usize,
}

impl<'a> LexerState<'a> {
    fn advance(&mut self, n: usize) {
        self.cursor += n;
    }

    fn remaining(&self) -> &'a str {
        &self.input[self.cursor..]
    }

    fn has_remaining(&self) -> bool {
        self.cursor < self.input.len()
    }

    pub fn current_n(&self, n: usize) -> &'a str {
        &self.input[self.cursor..self.cursor + n]
    }

    pub fn current_offset(&self) -> usize {
        self.cursor
    }
}

type TokenCreatorFn<Token> = dyn Fn() -> Token;
type TokenTransformerFn<Token> = dyn Fn(&LexerState, usize) -> LexResult<Token>;

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
        let re = format!("^{regex}");
        let re = Regex::new(&re).unwrap();
        LexRule::Regex { regex: re, handler }
    }

    fn matches(&self, state: &LexerState) -> Option<usize> {
        match &self {
            LexRule::Exact { pattern, .. } => state
                .remaining()
                .starts_with(pattern)
                .then_some(pattern.len()),
            LexRule::Regex { regex, .. } => regex
                .captures(state.remaining())
                .and_then(|captures| captures.get(0))
                .map(|matched| matched.end() - matched.start()),
        }
    }

    fn consume(&self, state: &LexerState, matched_size: usize) -> LexResult<Token> {
        match &self {
            LexRule::Exact { handler, .. } => Ok(handler()),
            LexRule::Regex { handler, .. } => handler(state, matched_size),
        }
    }
}

#[derive(Debug)]
pub enum LexError {
    UnknownPattern(String),
    UnparsableToken(String),
}

impl LexError {
    fn unknown(input: &str) -> LexError {
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

pub struct Lexer<Token> {
    pub rules: Vec<LexRule<Token>>,
}

impl<Token> Lexer<Token> {
    pub fn tokenize<'a>(&'a self, input: &'a str) -> LexResult<Vec<Token>> {
        let mut tokens = Vec::new();
        let mut state = LexerState { input, cursor: 0 };

        while state.has_remaining() {
            let mut was_consumed = false;

            for rule in &self.rules {
                let matched_size = match rule.matches(&state) {
                    None => continue,
                    Some(size) => size,
                };

                if let Ok(token) = rule.consume(&state, matched_size) {
                    tokens.push(token);
                    was_consumed = true;
                    state.advance(matched_size);
                    break;
                }
            }
            if !was_consumed {
                return Err(LexError::unknown(input));
            }
        }
        Ok(tokens)
    }
}

#[cfg(debug_assertions)]
#[derive(Debug, Clone)]
pub struct TokenDebugInfo {
    pub inpup_offset: usize,
    pub matched_rule: String,
    pub matched: String,
    pub matched_size: usize,
}
