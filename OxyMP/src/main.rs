#![allow(unused)]

#[oxymp::oxymp]
mod language {
    #[derive(Debug)]
    enum TokenizeError {
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

    fn match_number(input: &str) -> Result<Tok, TokenizeError> {
        input
            .parse()
            .map(|t| Tok::Number { value: t })
            .map_err(TokenizeError::NumberParseError)
    }

    fn match_ident(input: &str) -> Result<Tok, TokenizeError> {
        Ok(Tok::Ident(input.to_string()))
    }

    #[derive(Debug)]
    #[oxymp::Tokens]
    pub enum Tok {
        #[regex(r"[1-9][0-9]*(\.[0-9]+)?", match_number)]
        Number { value: f64 },

        #[exact(r"\(")]
        ParenLeft,

        #[exact(r"\)")]
        ParenRight,

        #[exact(r"\+")]
        Plus,

        #[exact("-")]
        Minus,
        #[exact("if")]
        If,

        #[exact("i[a-z]")]
        If2,

        #[regex(r"[a-z]+", match_ident)]
        Ident(String),
    }

    #[oxymp::Lexer]
    #[skip(r"[ \t]+")]
    pub struct Lexer;

    #[oxymp::RDParser]
    #[grammar(E = T T1?)]
    #[grammar(T1 = (r"\+" | "-") E)]
    #[grammar(T = Number | r"\(" E r"\)")]
    pub struct RDParser;
}
use language::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = "1    +2 if ia ifff";
    let tokens = Lexer::tokenize(input)?;
    println!("{:#?}", tokens);
    // let r = RDParser::E(tokens.into()).unwrap().1;
    // eprintln!("{:#?}", r.visit());

    Ok(())
}

// impl _RDParser::E {
//     pub fn visit(self) -> f64 {
//         let (t, t1) = self.value();
//         let t = t.visit();
//         match t1 {
//             Some(t1) => t + t1.visit(),
//             _ => t,
//         }
//     }
// }
//
// impl _RDParser::T1 {
//     pub fn visit(self) -> f64 {
//         let (sign, e) = self.value();
//         match sign {
//             _RDParser::T1Choice1::_1(_) => e.visit(),
//             _RDParser::T1Choice1::_2(_) => e.visit(),
//         }
//     }
// }
//
// impl _RDParser::T {
//     fn visit(self) -> f64 {
//         match self.value() {
//             _RDParser::TChoice1::_1(t) => t.value,
//             _RDParser::TChoice1::_2((_, e, _)) => e.visit(),
//         }
//     }
// }
