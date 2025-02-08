use oxymp::Tokens;
use oxymp_util::lexer::LexResult;

// exact variant's fields must be unit
#[derive(Tokens)]
enum Tok {
    #[exact("while")]
    While(i64),
}

fn match_number(_: &str) -> LexResult<Tok2> {
    Ok(Tok2::Number)
}

// regex variant's fields must not be unit
#[derive(Tokens)]
enum Tok2 {
    #[regex(r"\d+(.\d+)?", match_number)]
    Number,
}

// empty fields
#[derive(Tokens)]
enum Tok3 {
    #[regex(r"\d+(.\d+)?", match_number)]
    Number(),
}

#[derive(Tokens)]
enum Tok4 {
    #[regex(r"\d+(.\d+)?", match_number)]
    Number {},
}

fn main() {}
