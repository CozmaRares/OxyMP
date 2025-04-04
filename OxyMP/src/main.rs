#[oxymp::oxymp]
mod language {
    #[derive(Debug)]
    #[oxymp::Tokens]
    pub enum Tok {
        #[regex(r"\d+(.\d+)?", match_number)]
        Number { value: f64 },

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

use language::*;

impl _RDParser::E {
    pub fn visit(self) -> f64 {
        let (t, t1) = self.value();
        let t = t.visit();
        match t1 {
            Some(t1) => t + t1.visit(),
            _ => t,
        }
    }
}

impl _RDParser::T1 {
    pub fn visit(self) -> f64 {
        let (sign, e) = self.value();
        match sign {
            _RDParser::T1Choice1::_1(_) => e.visit(),
            _RDParser::T1Choice1::_2(_) => e.visit(),
        }
    }
}

impl _RDParser::T {
    fn visit(self) -> f64 {
        match self.value() {
            _RDParser::TChoice1::_1(t) => t.value,
            _RDParser::TChoice1::_2((_, e, _)) => e.visit(),
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let r = RDParser::E(
        vec![
            Tok::Number { value: 1.0 },
            Tok::Plus,
            Tok::Number { value: 2.0 },
        ]
        .into(),
    )
    .unwrap()
    .1;
    eprintln!("{:#?}", r.visit());

    Ok(())
}
