#![allow(unused)]

#[oxymp::oxymp]
mod language {
    #[derive(Debug)]
    #[oxymp::Tokens]
    pub enum Tok {
        #[exact(r"[1-9][0-9]*(\.[0-9]+)?")]
        Number,

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

        #[exact(r"[a-z]+")]
        Ident,
    }

    #[oxymp::Lexer]
    #[skip(r"[ \t]+")]
    pub struct Lexer;

    //#[oxymp::RDParser]
    //#[grammar(E = T T1?)]
    //#[grammar(T1 = (r"\+" | "-") E)]
    //#[grammar(T = Number | r"\(" E r"\)")]
    //pub struct RDParser;
}
use language::*;

//impl _RDParser::E {
//    pub fn visit(self) -> f64 {
//        let (t, t1) = self.value();
//        let t = t.visit();
//        match t1 {
//            Some(t1) => t + t1.visit(),
//            _ => t,
//        }
//    }
//}
//
//impl _RDParser::T1 {
//    pub fn visit(self) -> f64 {
//        let (sign, e) = self.value();
//        match sign {
//            _RDParser::T1Choice1::_1(_) => e.visit(),
//            _RDParser::T1Choice1::_2(_) => e.visit(),
//        }
//    }
//}
//
//impl _RDParser::T {
//    fn visit(self) -> f64 {
//        match self.value() {
//            _RDParser::TChoice1::_1(t) => t.value,
//            _RDParser::TChoice1::_2((_, e, _)) => e.visit(),
//        }
//    }
//}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = "1    +2";
    let tokens = Lexer::tokenize(input);
    println!("{:#?}", tokens);
    //let r = RDParser::E(
    //    vec![
    //        Tok::Number { value: 1.0 },
    //        Tok::Plus,
    //        Tok::Number { value: 2.0 },
    //    ]
    //    .into(),
    //)
    //.unwrap()
    //.1;
    //eprintln!("{:#?}", r.visit());

    Ok(())
}
