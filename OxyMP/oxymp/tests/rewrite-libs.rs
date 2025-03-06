//#![allow(non_camel_case_types)]
//#![allow(non_snake_case)]
//#![allow(unused)]
//
//use ::oxymp_util::lexer::{LexError, LexResult, LexerData};
//use oxymp::{Lexer, RDParser, Tokens};
//
//// TODO: debug clone into iter, other traits and derives
//
//// handle all used symbols
//fn _a(symbol: oxymp_macro_utils::symbols::Symbol) {
//    match symbol {
//        oxymp_macro_utils::symbols::Symbol::Str => todo!(),
//        oxymp_macro_utils::symbols::Symbol::Result => todo!(),
//        oxymp_macro_utils::symbols::Symbol::Ok => todo!(),
//        oxymp_macro_utils::symbols::Symbol::Err => todo!(),
//        oxymp_macro_utils::symbols::Symbol::Option => todo!(),
//        oxymp_macro_utils::symbols::Symbol::None => todo!(),
//        oxymp_macro_utils::symbols::Symbol::Some => todo!(),
//        oxymp_macro_utils::symbols::Symbol::Box => todo!(),
//        oxymp_macro_utils::symbols::Symbol::Vec => todo!(),
//        oxymp_macro_utils::symbols::Symbol::VecMacro => todo!(),
//        oxymp_macro_utils::symbols::Symbol::UtilLexRule => todo!(),
//        oxymp_macro_utils::symbols::Symbol::UtilLexerData => todo!(),
//        oxymp_macro_utils::symbols::Symbol::UtilLexResult => todo!(),
//        oxymp_macro_utils::symbols::Symbol::UtilLexError => todo!(),
//    };
//}
//
//use rewrites::*;
//mod rewrites {
//    use oxymp_util::*;
//    pub mod oxymp_util {
//        use lexer::*;
//        pub mod lexer {
//            pub struct LexRule;
//            pub struct LexError;
//            pub struct LexerData;
//            pub struct LexResult;
//        }
//    }
//
//    use core::*;
//    pub mod core {
//        use result::*;
//        pub mod result {
//            pub struct Result;
//            pub struct Ok;
//            pub struct Err;
//        }
//
//        use option::*;
//        pub mod option {
//            pub struct Option;
//            pub struct None;
//            pub struct Some;
//        }
//
//        use primitive::*;
//        pub mod primitive {
//            pub struct str;
//        }
//    }
//
//    use std::*;
//    pub mod std {
//        use vec::*;
//        pub mod vec {
//            pub struct Vec;
//        }
//        use boxed::*;
//        pub mod boxed {
//            pub struct Box;
//        }
//
//        #[macro_export]
//        macro_rules! vec {
//            () => {};
//        }
//    }
//}
//
//pub fn match_number(matched: &str) -> LexResult<Tok> {
//    matched
//        .parse()
//        .map_err(|_| LexError::unparsable(matched))
//        .map(|v| Tok::Number { value: v })
//}
//
//pub fn match_ident(matched: &str) -> LexResult<Tok> {
//    if matched.len() == 1 {
//        ::std::result::Result::Ok(Tok::Identifier(matched.to_string()))
//    } else {
//        ::std::result::Result::Err(LexError::unparsable(matched))
//    }
//}
//
//#[derive(Debug, Tokens)]
//pub enum Tok {
//    #[regex(r"\d+(.\d+)?", match_number)]
//    Number { value: i64 },
//
//    #[exact("while")]
//    While,
//
//    #[regex("[a-z]+", match_ident)]
//    Identifier(String),
//}
//
//#[derive(Lexer)]
//#[tokens(Tok)]
//#[skip(r"\s+")]
//pub struct Lexer(LexerData<Tok>);
//
//#[derive(RDParser)]
//#[tokens(Tok)]
//#[grammar_tokens(
//    While, ParenLeft, ParenRight, If, Else, Identifier, Equal, Plus, Number
//)]
//#[grammar("EWh ::= While '(' E ')' E")]
//#[grammar("EIf ::= If '(' E ')' E Else E")]
//#[grammar("EEq ::= Identifier '=' E")]
//#[grammar("EPl1 ::= '+' T EPl1?")]
//#[grammar("EPl ::= T EPl1?")]
//#[grammar("E ::= EWh | EIf | EEq | EPl")]
//#[grammar("T ::= Number | Identifier | '(' E ')'")]
//pub struct Parser;
//
//fn main() {}
