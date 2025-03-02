use oxymp::oxymp;

#[oxymp::oxymp]
mod parser {
    use oxymp_util::lexer::{LexError, LexResult};

    pub fn match_number(matched: &str) -> LexResult<Tok> {
        matched
            .parse()
            .map_err(|_| LexError::unparsable(matched))
            .map(|v| Tok::Number { value: v })
    }

    pub fn match_ident(matched: &str) -> LexResult<Tok> {
        if matched.len() == 1 {
            Ok(Tok::Identifier(matched.to_string()))
        } else {
            Err(LexError::unparsable(matched))
        }
    }

    #[derive(Debug)]
    #[oxymp::Tokens]
    pub enum Tok {
        #[regex(r"\d+(.\d+)?", match_number)]
        Number { value: i64 },

        #[exact("while")]
        While,

        #[exact("(")]
        ParenLeft,

        #[exact(")")]
        ParenRight,

        #[exact("if")]
        If,

        #[exact("else")]
        Else,

        #[regex("[a-z]+", match_ident)]
        Identifier(String),

        #[exact("=")]
        Equal,

        #[exact("+")]
        Plus,
    }

    //#[oxymp::Lexer]
    //#[skip(r"\s+")]
    //pub struct Lexer;
    //// TODO: lexer is broken for some reason
    //// 1+2 -> unknown
    //
    //#[oxymp::RDParser]
    //#[grammar("EWh ::= While '(' E ')' E")]
    //#[grammar("EIf ::= If '(' E ')' E Else E")]
    //#[grammar("EEq ::= Identifier '=' E")]
    //#[grammar("EPl1 ::= '+' T EPl1?")]
    //#[grammar("EPl ::= T EPl1?")]
    //#[grammar("E ::= EWh | EIf | EEq | EPl")]
    //#[grammar("T ::= Number | Identifier | '(' E ')'")]
    //pub struct Parser;
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    //let input = "1+2";
    //let lexer = Lexer::new();
    //let tokens = lexer.tokenize(input)?;
    //
    //println!("{:?}", tokens);

    Ok(())
}
