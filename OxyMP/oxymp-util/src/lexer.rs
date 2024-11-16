use std::collections::HashMap;

use regex::Regex;

#[derive(Debug)]
pub enum TokenMatcher {
    Exact(String),
    Regex(Regex),
}

impl TokenMatcher {
    pub fn regex(re: &str) -> TokenMatcher {
        let re = format!("^{re}");
        let re = Regex::new(&re).unwrap();
        TokenMatcher::Regex(re)
    }
}

#[derive(Debug)]
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

type TokenCreatorFn<Token> = dyn Fn(&LexerState, usize) -> Token;
type TokenTransformerFn<Token> = dyn Fn(&LexerState, usize) -> Result<Token, LexError>;

pub enum TokenHandler<Token>
where
    Token: std::fmt::Debug,
{
    Pattern(Box<TokenCreatorFn<Token>>),
    Regex(Box<TokenTransformerFn<Token>>),
    Ignore,
}

impl<Token> std::fmt::Debug for TokenHandler<Token>
where
    Token: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenHandler::Pattern(_) => f.debug_tuple("Pattern").finish(),
            TokenHandler::Regex(_) => f.debug_tuple("Regex").finish(),
            TokenHandler::Ignore => f.debug_tuple("Ignore").finish(),
        }
    }
}

#[derive(Debug)]
pub struct LexRule<Token>
where
    Token: std::fmt::Debug,
{
    matcher: TokenMatcher,
    handler: TokenHandler<Token>,

    #[cfg(debug_assertions)]
    #[allow(dead_code)]
    id: usize,
}

impl<Token> LexRule<Token>
where
    Token: std::fmt::Debug,
{
    pub fn new(
        matcher: TokenMatcher,
        handler: TokenHandler<Token>,
        #[cfg(debug_assertions)] id: usize,
    ) -> Self {
        LexRule {
            matcher,
            handler,
            #[cfg(debug_assertions)]
            id,
        }
    }

    fn matches(&self, state: &LexerState) -> Option<usize> {
        match &self.matcher {
            TokenMatcher::Exact(exact_match) => state
                .remaining()
                .starts_with(exact_match)
                .then_some(exact_match.len()),
            TokenMatcher::Regex(re) => re
                .captures(state.remaining())
                .and_then(|captures| captures.get(0))
                .map(|matched| matched.end() - matched.start()),
        }
    }

    fn consume(&self, state: &LexerState, matched_size: usize) -> Result<Option<Token>, LexError> {
        match &self.handler {
            TokenHandler::Ignore => Ok(None),
            TokenHandler::Pattern(f) => Ok(Some(f(state, matched_size))),
            TokenHandler::Regex(f) => f(state, matched_size).map(|t| Some(t)),
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

pub type LexResult<T> = Result<T, LexError>;

#[derive(Debug)]
pub struct Lexer<Token>
where
    Token: std::fmt::Debug,
{
    pub rules: Vec<LexRule<Token>>,
}

impl<Token> Lexer<Token>
where
    Token: std::fmt::Debug,
{
    pub fn tokenize<'a>(&'a self, input: &'a str) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        let mut state = LexerState { input, cursor: 0 };

        while state.has_remaining() {
            let mut was_consumed = false;

            for rule in &self.rules {
                let matched_size = match rule.matches(&state) {
                    None => continue,
                    Some(size) => size,
                };

                if let Ok(result) = rule.consume(&state, matched_size) {
                    if let Some(token) = result {
                        tokens.push(token);
                    }
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

#[derive(Ord, Eq, PartialEq, PartialOrd, Default, Hash)]
pub enum DefaultTokenTier {
    High,
    Medium,
    #[default]
    Low,
}

pub struct LexerBuilder<Token, Tier = DefaultTokenTier>
where
    Token: std::fmt::Debug,
    Tier: std::hash::Hash + Ord + Default,
{
    rules: HashMap<Tier, Vec<LexRule<Token>>>,
}

impl<Token, Tier> LexerBuilder<Token, Tier>
where
    Token: std::fmt::Debug,
    Tier: std::hash::Hash + Ord + Default,
{
    pub fn new() -> Self {
        Self {
            rules: HashMap::new(),
        }
    }

    pub fn add_rule(&mut self, rule: LexRule<Token>) {
        let tier = Tier::default();
        self.rules.entry(tier).or_default().extend(vec![rule]);
    }

    pub fn add_tiered_rule(&mut self, tier: Tier, rule: LexRule<Token>) {
        self.rules.entry(tier).or_default().extend(vec![rule]);
    }

    pub fn build(self) -> Lexer<Token> {
        let mut rules: Vec<_> = self.rules.into_iter().collect();
        rules.sort_by(|(key1, _), (key2, _)| key1.cmp(key2));

        let rules = rules.into_iter().flat_map(|(_, rules)| rules).collect();

        Lexer { rules }
    }
}

impl<Token, Tier> Default for LexerBuilder<Token, Tier>
where
    Token: std::fmt::Debug,
    Tier: std::hash::Hash + Ord + Default,
{
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(debug_assertions)]
#[derive(Debug, Clone)]
pub struct TokenDebugInfo {
    pub lex_rule: String,
    pub offset: usize,
    pub matched_size: usize,
    pub matched_string: String,
}
