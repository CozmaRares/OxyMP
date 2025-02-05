use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ParserInput<Token> {
    pub tokens: Rc<[Token]>,
    pub current: usize,
}

impl<Token> From<Vec<Token>> for ParserInput<Token> {
    fn from(value: Vec<Token>) -> Self {
        Self {
            tokens: value.into(),
            current: 0,
        }
    }
}

impl<Token> ParserInput<Token> {
    pub fn get_current(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    pub fn increment(&self) -> ParserInput<Token> {
        ParserInput {
            tokens: self.tokens.clone(),
            current: self.current + 1,
        }
    }
}
pub type ParserState<Token, Value> = Result<(ParserInput<Token>, Value), ParseError<Token>>;

#[derive(Debug)]
pub enum ParseErrorReason<Token> {
    UnexpectedEOI { expected: Vec<String> },
    UnexpectedToken { expected: Vec<String>, token: Token },
    AllChoicesFailed,
}

#[derive(Debug)]
pub struct ParseIssue<Token> {
    pub rule: String,
    pub input_location: usize,
    pub reason: ParseErrorReason<Token>,
}

#[derive(Debug)]
pub enum ParseError<Token> {
    Single(ParseIssue<Token>),
    Multi(Vec<ParseIssue<Token>>),
}

impl<Token> ParseError<Token> {
    pub fn new(
        rule: String,
        input_location: usize,
        reason: ParseErrorReason<Token>,
    ) -> ParseError<Token> {
        ParseError::Single(ParseIssue {
            rule,
            input_location,
            reason,
        })
    }

    pub fn add_issue(
        self,
        rule: String,
        input_location: usize,
        reason: ParseErrorReason<Token>,
    ) -> ParseError<Token> {
        let new_issue = ParseIssue {
            rule,
            input_location,
            reason,
        };

        match self {
            ParseError::Single(issue) => ParseError::Multi(vec![issue, new_issue]),
            ParseError::Multi(mut issues) => {
                issues.push(new_issue);
                ParseError::Multi(issues)
            }
        }
    }
}
