#![allow(unused)]

mod matchers;

#[oxymp::oxymp]
mod language {
    use crate::matchers::*;

    #[derive(Debug, Clone)]
    #[oxymp::Tokens]
    pub enum Tok {
        #[regex(r"[1-9][0-9]*(\.[0-9]+)?", match_number)]
        Number(f64),

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

    impl std::fmt::Display for Tok {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Tok::Number(n) => write!(f, "{}", n),
                Tok::ParenLeft => write!(f, "("),
                Tok::ParenRight => write!(f, ")"),
                Tok::Plus => write!(f, "+"),
                Tok::Minus => write!(f, "-"),
                Tok::If => write!(f, "if"),
                Tok::If2 => write!(f, "if2"),
                Tok::Ident(s) => write!(f, "{}", s),
            }
        }
    }

    #[oxymp::Lexer]
    #[skip(r"[ \t]+")]
    #[error(TokenizeError)]
    pub mod lexer {}

    #[oxymp::RDParser]
    #[grammar(E = T T1?)]
    #[grammar(T1 = (r"\+" | "-") E)]
    #[grammar(T = Number | r"\(" E r"\)")]
    pub mod rd_parser {}
}
use language::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = "1 + 2";
    let tokens = lexer::tokenize(input)?;
    let (_inp, ast) = rd_parser::E(tokens.clone().into())?;

    println!("{:#?}", input);
    println!("{:#?}", tokens);
    println!("{:#?}", ast);
    println!("{}", ast.visit());

    Ok(())
}

impl rd_parser::E {
    fn visit(self) -> f64 {
        let (t, t1) = self.value;
        let t = t.visit();
        match t1 {
            Some(t1) => t + t1.visit(),
            _ => t,
        }
    }
}

impl rd_parser::T1 {
    fn visit(self) -> f64 {
        let (sign, e) = self.value;
        match sign {
            rd_parser::T1Choice1::_1(_) => e.visit(),
            rd_parser::T1Choice1::_2(_) => e.visit(),
        }
    }
}

impl rd_parser::T {
    fn visit(self) -> f64 {
        match self.value {
            rd_parser::TChoice1::_1(TokNumber(n)) => n,
            rd_parser::TChoice1::_2((_, e, _)) => e.visit(),
        }
    }
}
