use oxymp::Tokens;

// === REGEX ===

// missing transformer function
#[derive(Tokens)]
enum Tok1 {
    #[regex(r"\d+(.\d+)?")]
    Number { value: i64 },

    #[exact("a")]
    A,
}

// missing comma between regex and function
#[derive(Tokens)]
enum Tok2 {
    #[regex(r"\d+(.\d+)?" match_number)]
    Number { value: i64 },

    #[exact("a")]
    A,
}

// extra tokens
#[derive(Tokens)]
enum Tok3 {
    #[regex(r"\d+(.\d+)?", match_number, aaa)]
    Number { value: i64 },

    #[exact("a")]
    A,
}

// regex is not a string
#[derive(Tokens)]
enum Tok4 {
    #[regex(123, match_number)]
    Number { value: i64 },

    #[exact("a")]
    A,
}

// transform function is not a path
#[derive(Tokens)]
enum Tok5 {
    #[regex(r"\d+(.\d+)?", "match_number")]
    Number { value: i64 },
}

// missing arguments
#[derive(Tokens)]
enum Tok6 {
    #[regex]
    Number { value: i64 },
}

// no arguments
#[derive(Tokens)]
enum Tok7 {
    #[regex()]
    Number { value: i64 },
}

// === EXACT ===

// missing arguments
#[derive(Tokens)]
enum Tok8 {
    #[exact]
    Number,
}

// no arguments
#[derive(Tokens)]
enum Tok9 {
    #[exact()]
    Number,
}

// pattern is not a string
#[derive(Tokens)]
enum Tok10 {
    #[exact(123)]
    Number,
}

// extra tokens
#[derive(Tokens)]
enum Tok11 {
    #[exact("a" "b")]
    A,
}

// === GENERAL ERRORS ===

// invalid attribute

#[derive(Tokens)]
enum Tok12 {
    #[invalid]
    A,
}

// multiple attributes
#[derive(Tokens)]
enum Tok13 {
    #[exact("a")]
    #[exact("b")]
    A,
}

#[derive(Tokens)]
enum Tok14 {
    #[exact("a")]
    #[regex("abc")]
    A,
}

#[derive(Tokens)]
enum Tok15 {
    #[regex("abc")]
    #[regex("def")]
    A,
}

// no attribute
#[derive(Tokens)]
enum Tok16 {
    A,
}

fn main() {}
