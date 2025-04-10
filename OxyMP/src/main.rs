#![allow(unused)]

#[oxymp::oxymp]
mod language {
    #[derive(Debug)]
    #[oxymp::Tokens]
    pub enum Tok {
        #[exact("a{2,3}")]
        A,

        #[exact("b")]
        B,

        #[exact("(a)|(b)")]
        Alt,

        #[exact("((a)|(b))*")]
        Many0,

        #[exact("(a)(b)(b)")]
        Chain,

        #[exact("((a)|(b))*(a)(b)(b)")]
        All,

        #[exact("(a|b)*abb")]
        All2,
        // #[exact(r"ab|C")]
        // A0,
        //
        // #[exact(r"a")]
        // A,
        //
        // #[exact(r"ab")]
        // AB,
        //
        // #[exact("a|b")]
        // AB2,
        //
        // #[exact(r"[a-c]")]
        // AZ,
        //
        // #[exact(r"ab")]
        // AP,
        //
        // #[exact(r"a*")]
        // AS,
        //
        // #[exact(r"a?")]
        // AQ,
        //
        // #[exact(r"a{2}")]
        // AA,
        //
        // #[exact(r"a{2,}")]
        // AAA,
        //
        // #[exact(r"a{2,3}")]
        // AAAB,
        //
        // #[exact(r"a{2,3}?")]
        // AAABQ,
        //
        // #[exact(r"a{2,3}+")]
        // AAABP,
        //
        // #[exact(r"a{2,3}*")]
        // AAABS,
        //
        //#[regex(r"[1-9][0-9]*(\.[0-9]+)?", match_number)]
        //Number { value: f64 },
        //
        //#[exact(r"\(")]
        //ParenLeft,
        //
        //#[exact(r"\)")]
        //ParenRight,
        //
        //#[exact(r"\+")]
        //Plus,
        //
        //#[exact("-")]
        //Minus,
        //
        //#[regex(r"[a-z]+", match_ident)]
        //Ident(String),
    }

    //#[oxymp::Lexer]
    //#[skip(r"\s+")]
    //pub struct Lexer;
    //
    //#[oxymp::RDParser]
    //#[grammar(E = T T1?)]
    //#[grammar(T1 = (r"\+" | "-") E)]
    //#[grammar(T = Number | r"\(" E r"\)")]
    //pub struct RDParser;
}

//use language::*;
//
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
