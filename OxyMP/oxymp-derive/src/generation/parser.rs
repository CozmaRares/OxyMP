#![allow(non_snake_case)]

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use proc_macro2::{Ident, TokenStream};
use quote::quote;

use crate::{
    data::MacroData,
    grammar::{GrammarNode, GrammarNodeContent},
    idents::{parser, tokens},
    symbols::{get_def, Symbol},
};

pub fn generate_impl(data: &MacroData, rules: &HashMap<Rc<str>, GrammarNode>) -> TokenStream {
    let parser_ident = &data.parser_ident;

    let methods = rules
        .iter()
        .map(|(rule, node)| generate_rule(rule, node, rules, data));

    quote! {
        impl #parser_ident {
            #(#methods)*
        }
    }
}

fn generate_rule(
    rule: &str,
    node: &GrammarNode,
    rules: &HashMap<Rc<str>, GrammarNode>,
    data: &MacroData,
) -> TokenStream {
    let visibility = &data.visibility;

    let method_ident = parser::rule_method_ident(rule);
    let struct_ident = parser::rule_struct_ident(rule);
    let defs = expand_node(rule, node, rules, data, false);
    let toks = defs.0;
    let ident = defs.1;

    let _Ok = get_def(Symbol::Ok, data.simple_types);
    let _ParserInput = get_def(Symbol::UtilParserInput, data.simple_types);
    let _ParserState = get_def(Symbol::UtilParserState, data.simple_types);

    quote! {
        #visibility fn #method_ident(inp: #_ParserInput<Token>) -> #_ParserState<Token, #struct_ident> {
            #toks
            #_Ok((
                inp,
                #struct_ident(#ident)
            ))
        }
    }
}

fn expand_node(
    rule: &str,
    node: &GrammarNode,
    rules: &HashMap<Rc<str>, GrammarNode>,
    data: &MacroData,
    needs_check: bool,
) -> (TokenStream, Ident) {
    let node_ident = parser::idx_ident(node.index);
    let parser_ident = &data.parser_ident;

    let _Box = get_def(Symbol::Box, data.simple_types);
    let _None = get_def(Symbol::None, data.simple_types);
    let _Some = get_def(Symbol::Some, data.simple_types);
    let _Err = get_def(Symbol::Err, data.simple_types);
    let _Ok = get_def(Symbol::Ok, data.simple_types);
    let _ParseError = get_def(Symbol::UtilParseError, data.simple_types);
    let _ParserState = get_def(Symbol::UtilParserState, data.simple_types);
    let _ParseErrorReason = get_def(Symbol::UtilParseErrorReason, data.simple_types);
    let _vec = get_def(Symbol::VecMacro, data.simple_types);

    let toks = match &node.content {
        GrammarNodeContent::Rule(nested_rule) => {
            let rule_ident = parser::rule_method_ident(nested_rule);
            let dir_set = compute_dir_set(nested_rule, node, data.depth_limit, rules);
            let check = generate_token_check(rule, &dir_set, data.simple_types, needs_check);

            quote! {
                #check
                let (inp, #node_ident) =
                    #parser_ident::#rule_ident(inp)
                        .map(|(remaining, ast)| (remaining, #_Box::new(ast)))?;
            }
        }
        GrammarNodeContent::Token(token) => {
            let token_enum_entry = tokens::enum_ident(token);
            let token_enum_entry_string = token_enum_entry.to_string();

            quote! {
                let (inp, #node_ident) = match inp.get_current() {
                    #_None => #_Err(#_ParseError::new(
                        #rule.into(),
                        inp.current,
                        #_ParseErrorReason::UnexpectedEOI {
                            expected: #_vec![#token_enum_entry_string.into()],
                        }
                    )),
                    #_Some(Token::#token_enum_entry(tok, ..)) =>
                        #_Ok((inp.increment(), tok.clone())),
                    #_Some(tok) => #_Err(#_ParseError::new (
                        #rule.into(),
                        inp.current,
                        #_ParseErrorReason::UnexpectedToken {
                            expected: #_vec![#token_enum_entry_string.into()],
                            token: tok.clone(),
                        }
                    ))
                }?;
            }
        }
        GrammarNodeContent::List(exprs) => {
            let defs = exprs
                .iter()
                .enumerate()
                .map(|(idx, expr)| expand_node(rule, expr, rules, data, idx != 0));
            let toks = defs.clone().map(|d| d.0);
            let idents = defs.map(|d| d.1);

            let dir_set = compute_dir_set(rule, node, data.depth_limit, rules);
            let check = generate_token_check(rule, &dir_set, data.simple_types, needs_check);

            quote! {
                #check
                let (inp, #node_ident) = (|| {
                     #(#toks)*

                    #_Ok((
                        inp,
                        ( #(#idents),* )
                    ))
                })()?;
            }
        }
        GrammarNodeContent::Choice(choices, choice_idx) => {
            let defs = choices
                .iter()
                .map(|expr| expand_node(rule, expr, rules, data, false))
                .enumerate()
                .map(|(idx, (toks, ident))| {
                    let idx_ident = parser::idx_ident(idx + 1);
                    let choice_ident = parser::choice_ident(rule, *choice_idx);

                    quote! {
                        let r: #_ParserState<_, _> = (|| {
                            let inp = inp.clone();
                            #toks
                            #_Ok((inp, #ident))
                        })();
                        if let #_Ok((inp, ast)) = r {
                            return #_Ok((inp, #choice_ident::#idx_ident(ast)));
                        };
                    }
                });

            let dir_set = compute_dir_set(rule, node, data.depth_limit, rules);
            let check = generate_token_check(rule, &dir_set, data.simple_types, needs_check);

            quote! {
                #check
                let (inp, #node_ident) =  (|| {
                    #(#defs)*

                    #_Err(#_ParseError::new(
                        #rule.into(),
                        inp.current,
                        #_ParseErrorReason::AllChoicesFailed,
                    ))
                })()?;
            }
        }
        GrammarNodeContent::Optional(opt) => {
            let (toks, ident) = expand_node(rule, opt, rules, data, false);

            let dir_set = compute_dir_set(rule, node, data.depth_limit, rules);
            let check = generate_token_check(rule, &dir_set, data.simple_types, needs_check);

            quote! {
                let res: #_ParserState<_, _> = (|| {
                    #check
                    let inp = inp.clone();
                    #toks
                    #_Ok((inp, #ident))
                })();
                let (inp, #node_ident) =  match res {
                    #_Ok((new_inp, ast)) => (new_inp, #_Some(ast)),
                    #_Err(_) => (inp, #_None),
                };
            }
        }
    };

    (toks, node_ident)
}

#[derive(Clone)]
struct DirectionSet {
    tokens: Rc<HashSet<Rc<str>>>,
    nullable: bool,
}

impl DirectionSet {
    fn nullable(self) -> DirectionSet {
        DirectionSet {
            tokens: self.tokens,
            nullable: true,
        }
    }
}

thread_local! {
    static DIRECTION_SET_CACHE: RefCell<HashMap<*const GrammarNode, DirectionSet>> = RefCell::new(HashMap::new());
}

fn compute_dir_set(
    current_rule: &str,
    node: &GrammarNode,
    depth: usize,
    rules: &HashMap<Rc<str>, GrammarNode>,
) -> DirectionSet {
    if depth == 0 {
        panic!("Possible left recursion deteted! Reached depth limit when computing the direction set for the rule: {}", current_rule);
    }

    if let Some(cached) =
        DIRECTION_SET_CACHE.with(|c| c.borrow().get(&(node as *const GrammarNode)).cloned())
    {
        return cached;
    }

    let computed = match &node.content {
        GrammarNodeContent::Rule(rule) => {
            let rule_node = rules.get(rule).unwrap();
            compute_dir_set(rule, rule_node, depth - 1, rules)
        }
        GrammarNodeContent::Token(token) => DirectionSet {
            tokens: pipe!([token.clone()] => HashSet::from => Rc::new),
            nullable: false,
        },
        GrammarNodeContent::List(list) => {
            let mut dir_set = HashSet::new();
            let mut nullable = true;

            for list_node in list {
                let node_set = compute_dir_set(current_rule, list_node, depth, rules);

                dir_set.extend(<HashSet<Rc<str>> as Clone>::clone(&node_set.tokens).into_iter());

                if !node_set.nullable {
                    nullable = false;
                    break;
                }
            }

            DirectionSet {
                tokens: pipe!(dir_set => Rc::new),
                nullable,
            }
        }
        GrammarNodeContent::Optional(opt) => {
            compute_dir_set(current_rule, opt, depth, rules).nullable()
        }
        GrammarNodeContent::Choice(choices, _) => {
            let mut choices = choices
                .iter()
                .map(|choice| compute_dir_set(current_rule, choice, depth, rules));

            let set: HashSet<_> = choices
                .clone()
                .flat_map(|choice| choice.tokens.iter().cloned().collect::<Vec<_>>())
                .collect();

            let nullable = choices.any(|choice| choice.nullable);

            DirectionSet {
                tokens: pipe!(set => Rc::new),
                nullable,
            }
        }
    };

    DIRECTION_SET_CACHE.with(|c| {
        c.borrow_mut()
            .insert(node as *const GrammarNode, computed.clone())
    });
    computed
}

fn generate_token_check(
    rule: &str,
    check: &DirectionSet,
    simple_types: bool,
    needs_check: bool,
) -> TokenStream {
    if !needs_check || check.nullable {
        return quote! {};
    }

    let _None = get_def(Symbol::None, simple_types);
    let _Some = get_def(Symbol::Some, simple_types);
    let _Err = get_def(Symbol::Err, simple_types);
    let _ParseError = get_def(Symbol::UtilParseError, simple_types);
    let _ParseErrorReason = get_def(Symbol::UtilParseErrorReason, simple_types);
    let _vec = get_def(Symbol::VecMacro, simple_types);

    let token_names: Vec<_> = check
        .tokens
        .iter()
        .map(|token| tokens::enum_ident(token).to_string())
        .map(|token| quote! { #token.into() })
        .collect();

    let branches = check
        .tokens
        .iter()
        .map(|token| tokens::enum_ident(token))
        .map(|ident| quote! { #_Some(Token::#ident(..)) => {} });

    quote! {
        match inp.get_current() {
            #_None => return #_Err(#_ParseError::new(
                #rule.into(),
                inp.current,
                #_ParseErrorReason::UnexpectedEOI {
                    expected: #_vec![#(#token_names),*],
                }
            )),
            #(#branches)*
            #_Some(tok) => return #_Err(#_ParseError::new(
                #rule.into(),
                inp.current,
                #_ParseErrorReason::UnexpectedToken {
                    expected: #_vec![#(#token_names),*],
                    token: tok.clone(),
                }
            ))
        };
    }
}
