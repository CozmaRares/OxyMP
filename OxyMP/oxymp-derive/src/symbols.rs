use std::{cell::RefCell, collections::HashMap, rc::Rc};

use lazy_static::lazy_static;
use quote::quote;

use crate::idents::base_ident;

#[derive(PartialEq, Eq, Hash)]
pub enum Symbol {
    DeriveDebug,
    DeriveClone,

    Ok,
    Err,

    Option,
    None,
    Some,

    Box,

    VecMacro,

    UtilTokenMatcher,
    UtilTokenHandler,
    UtilTokenDebugInfo,
    UtilLexRule,
    UtilLexer,
    UtilLexerBuilder,

    UtilParserInput,
    UtilParserState,
    UtilParseError,
    UtilParseErrorReason,
}

type Definition<'a> = (&'a [&'a str], &'a str);

#[rustfmt::skip]
lazy_static! {
    static ref STD_LIB: HashMap<Symbol, Definition<'static>> = {
        let mut m = HashMap::new();

        m.insert(Symbol::DeriveDebug, (&["std", "fmt"  ][..], "Debug"));
        m.insert(Symbol::DeriveClone, (&["std", "clone"][..], "Clone"));

        m.insert(Symbol::Ok,     (&["std", "result", "Result"][..], "Ok"    ));
        m.insert(Symbol::Err,    (&["std", "result", "Result"][..], "Err"   ));

        m.insert(Symbol::Option, (&["std", "option"          ][..], "Option"));
        m.insert(Symbol::None,   (&["std", "option", "Option"][..], "None"  ));
        m.insert(Symbol::Some,   (&["std", "option", "Option"][..], "Some"  ));

        m.insert(Symbol::Box,    (&["std", "boxed" ][..], "Box"   ));

        m.insert(Symbol::VecMacro,    (&["std"][..], "vec")   );

        m.insert(Symbol::UtilTokenMatcher,   (&["oxymp_util", "lexer"][..], "TokenMatcher"));
        m.insert(Symbol::UtilTokenHandler,   (&["oxymp_util", "lexer"][..], "TokenHandler"));
        m.insert(Symbol::UtilTokenDebugInfo, (&["oxymp_util", "lexer"][..], "TokenDebugInfo"));
        m.insert(Symbol::UtilLexRule,        (&["oxymp_util", "lexer"][..], "LexRule"     ));
        m.insert(Symbol::UtilLexer,          (&["oxymp_util", "lexer"][..], "Lexer"       ));
        m.insert(Symbol::UtilLexerBuilder,   (&["oxymp_util", "lexer"][..], "LexerBuilder"));

        m.insert(Symbol::UtilParserInput,      (&["oxymp_util", "parser"][..], "ParserInput"));
        m.insert(Symbol::UtilParserState,      (&["oxymp_util", "parser"][..], "ParserState"));
        m.insert(Symbol::UtilParseError,       (&["oxymp_util", "parser"][..], "ParseError"));
        m.insert(Symbol::UtilParseErrorReason, (&["oxymp_util", "parser"][..], "ParseErrorReason"));

        m
    };
}

thread_local! {
    static SYMBOL_CACHE: RefCell<HashMap<Symbol, Rc<proc_macro2::TokenStream>>> = RefCell::new(HashMap::new());
}

pub fn get_def(symbol: Symbol, simple: bool) -> Rc<proc_macro2::TokenStream> {
    if let Some(cached) = SYMBOL_CACHE.with(|c| c.borrow().get(&symbol).cloned()) {
        return cached;
    }

    let def = STD_LIB.get(&symbol).unwrap();
    let base = base_ident(def.1);

    let toks = Rc::new(if simple {
        quote! { #base }
    } else {
        let path = def.0.iter().map(|s| base_ident(s));
        quote! { ::#(#path)::*::#base }
    });

    SYMBOL_CACHE.with(|c| c.borrow_mut().insert(symbol, toks.clone()));
    toks
}
