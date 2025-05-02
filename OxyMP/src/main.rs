#![allow(unused)]

mod matchers;

#[oxymp::oxymp]
mod language {
    use crate::matchers::*;

    #[derive(Debug)]
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

    #[oxymp::Lexer]
    #[skip(r"[ \t]+")]
    pub mod lexer {}

    #[oxymp::RDParser]
    // #[grammar(E = T T1?)]
    // #[grammar(T1 = (r"\+" | "-") E)]
    // #[grammar(T = Number | r"\(" E r"\)")]
    mod rd_parser {}
}
use language::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = "1 1    +2 if ia ifff";
    let r = lexer::tokenize(input);
    println!("{:#?}", input);
    println!("{:#?}", r);
    //
    // match r {
    //     Err(e) => eprintln!("{e}"),
    //     Ok(t) => println!("{:#?}", t),
    // }

    // println!("{:#?}", tokens);
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
