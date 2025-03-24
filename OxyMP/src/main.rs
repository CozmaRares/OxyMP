#[oxymp::oxymp]
mod language {
    #[derive(Debug)]
    #[oxymp::Tokens]
    pub enum Tok {
        #[regex(r"\d+(.\d+)?", match_number)]
        Number { value: i64 },

        #[exact("(")]
        ParenLeft,

        #[exact(")")]
        ParenRight,

        #[exact("+")]
        Plus,

        #[exact("-")]
        Minus,

        #[regex(r"[a-z]+", match_ident)]
        Ident(String),
    }

    #[oxymp::Lexer]
    #[skip(r"\s+")]
    pub struct Lexer;

    #[oxymp::RDParser]
    #[grammar(E = T T1?)]
    #[grammar(T1 = ("+" | "-") E)]
    #[grammar(T = Number | "(" E ")")]
    pub struct RDParser;
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    Ok(())
}
