use std::collections::HashSet;

use proc_macro2::TokenStream;

use crate::{
    data::{rd_parser::RDParserData, tokens::TokensData},
    grammar::{parse_grammar, GrammarNode, GrammarRule},
    idents,
    utils::combine_errors,
};

pub fn generate(
    tokens_data: &TokensData,
    rd_parsers: Vec<(syn::ItemMod, RDParserData)>,
) -> syn::Result<Vec<syn::Item>> {
    let mut items = Vec::new();
    let mut errors = Vec::new();

    for (item_mod, rd_data) in rd_parsers {
        let result = generate_one(tokens_data, item_mod, rd_data);
        match result {
            Ok(item) => items.push(item),
            Err(err) => errors.push(err),
        }
    }

    if !errors.is_empty() {
        Err(combine_errors(errors))
    } else {
        Ok(items)
    }
}

fn generate_one(
    tokens_data: &TokensData,
    mut item_mod: syn::ItemMod,
    rd_data: RDParserData,
) -> syn::Result<syn::Item> {
    let vis = &rd_data.visibility;

    let rules = parse_grammar(tokens_data, rd_data.grammar_rules)?;

    let ast = generate_ast(&tokens_data.ident, &rules);
    let parser = generate_parser(&rules, &tokens_data.ident);

    let tokens_ident = &tokens_data.ident;

    let parser_items = items! {
        type Token = super::#tokens_ident;

        #[derive(Debug, Clone)]
        pub struct ParserInput {
            inp: ::std::rc::Rc<[Token]>,
            cursor: ::core::primitive::usize,
        }
        impl From<Vec<Token>> for ParserInput {
            fn from(value: Vec<Token>) -> Self {
                Self {
                    inp: value.into(),
                    cursor: 0,
                }
            }
        }
        impl ParserInput {
            fn peek(&self) -> Option<&Token> {
                self.inp.get(self.cursor)
            }

            fn advance(&self) -> Self {
                Self {
                    inp: self.inp.clone(),
                    cursor: self.cursor + 1,
                }
            }
        }

        // TODO:
        pub type ParserError = ();
        pub type ParserState<T> = Result<(ParserInput, T), ParserError>;

        #ast
        #parser
    };

    let (brace, _) = item_mod.content.expect("module should have content");
    item_mod.content = Some((brace, parser_items));
    Ok(syn::Item::Mod(item_mod))
}

fn generate_ast(tokens_ident: &syn::Ident, rules: &Vec<GrammarRule>) -> TokenStream {
    let structs = rules.iter().map(|GrammarRule { name, node }| {
        let ASTNode {
            main_struct,
            external_choices,
        } = generate_ast_node(&name, node, tokens_ident);

        let external_choices = match external_choices {
            None => q! {},
            Some(structs) => q! { #(#structs)* },
        };

        let rule_ident = idents::parser_rule(name);

        q! {
            #external_choices
            #[derive(Debug)]
            pub struct #rule_ident {
                value: #main_struct
            }

            // TO be used by the user
            impl #rule_ident {
                pub fn value_ref(&self) -> &#main_struct {
                    &self.value
                }

                pub fn value(self) -> #main_struct {
                    self.value
                }

                // TODO:
                //pub fn debug(&self) -> () { }
                //pub fn trace(&self) -> () { }
            }
        }
    });

    q! {
        #(#structs)*
    }
}

struct ASTNode {
    main_struct: TokenStream,
    external_choices: Option<Vec<TokenStream>>,
}

fn generate_ast_node(name: &String, node: &GrammarNode, tokens_ident: &syn::Ident) -> ASTNode {
    match &node {
        GrammarNode::Rule(rule) => {
            let ident = idents::parser_rule(rule);
            ASTNode {
                main_struct: q! {::std::boxed::Box<#ident>},
                external_choices: None,
            }
        }
        GrammarNode::Token(token) => {
            let ident = idents::token_struct(tokens_ident, token);
            ASTNode {
                main_struct: q! { super::#ident },
                external_choices: None,
            }
        }
        GrammarNode::List(exprs) => {
            let defs = exprs
                .iter()
                .map(|expr| generate_ast_node(name, expr, tokens_ident));
            let main_struct = defs.clone().map(|d| d.main_struct);
            let external_choices = defs.filter_map(|d| d.external_choices).flatten();

            ASTNode {
                main_struct: q! { ( #(#main_struct),* ) },
                external_choices: Some(external_choices.collect()),
            }
        }
        GrammarNode::Choice(choices, choice_idx) => {
            let defs = choices
                .iter()
                .map(|choice| generate_ast_node(name, choice, tokens_ident));
            let enum_entries = defs
                .clone()
                .map(|d| d.main_struct)
                .enumerate()
                .map(|(idx, s)| {
                    let idx_ident = idents::numeric(idx + 1);
                    q! {
                        #idx_ident(#s)
                    }
                });

            let mut external_choices: Vec<_> =
                defs.filter_map(|d| d.external_choices).flatten().collect();
            let enum_ident = idents::choice_enum(name, *choice_idx);
            external_choices.push(q! {
                #[derive(Debug)]
                pub enum #enum_ident {
                    #(#enum_entries),*
                }
            });

            ASTNode {
                main_struct: q! { #enum_ident },
                external_choices: Some(external_choices),
            }
        }
        GrammarNode::Optional(opt) => {
            let generated = generate_ast_node(name, opt, tokens_ident);
            let main_struct = generated.main_struct;
            ASTNode {
                main_struct: q! { ::std::option::Option<#main_struct> },
                external_choices: generated.external_choices,
            }
        }
    }
}

fn generate_parser(rules: &Vec<GrammarRule>, tokens_ident: &syn::Ident) -> TokenStream {
    let methods = rules
        .iter()
        .map(|GrammarRule { name, node }| generate_method(name, node, rules, tokens_ident));

    q! {
        #(#methods)*
    }
}

fn generate_method(
    name: &String,
    node: &GrammarNode,
    rules: &Vec<GrammarRule>,
    tokens_ident: &syn::Ident,
) -> TokenStream {
    let defs = expand_node(name, node, false, 1, rules, tokens_ident);
    let toks = defs.0;
    let ident = defs.1;

    let rule_ident = idents::parser_rule(name);

    q! {
        pub fn #rule_ident<'a>(inp: ParserInput) -> ParserState<#rule_ident> {
            #toks
            Ok((
                inp,
                #rule_ident { value: #ident }
            ))
        }
    }
}

fn expand_node(
    name: &str,
    node: &GrammarNode,
    needs_check: bool,
    node_idx: usize,
    rules: &Vec<GrammarRule>,
    tokens_ident: &syn::Ident,
) -> (TokenStream, proc_macro2::Ident) {
    let node_ident = idents::numeric(node_idx);

    let toks = match &node {
        GrammarNode::Rule(nested_rule) => {
            let rule_ident = idents::parser_rule(nested_rule);
            // TODO: implement depth limit
            //let dir_set = compute_dir_set(nested_rule, node, 10, &data.0);
            //let check = generate_token_check(rule, &dir_set, data.simple_types, needs_check);

            q! {
                //#check
                let (inp, #node_ident) =
                    #rule_ident(inp)
                        .map(|(remaining, ast)| (remaining, Box::new(ast)))?;
            }
        }
        GrammarNode::Token(token) => {
            let token_struct_entry = idents::token_struct(tokens_ident, token);

            q! {
                let Some(_current) = inp.peek() else {
                return Err(()); // TODO: EOI
                };
                let (inp, #node_ident) = match super::#token_struct_entry::try_from_ref(_current) {
                    None => return Err(()), // TODO: Unexpected
                    Some(t) => (inp.advance(), t),
                };
            }
        }
        GrammarNode::List(exprs) => {
            let defs = exprs
                .iter()
                .enumerate()
                .map(|(idx, expr)| expand_node(name, expr, idx != 0, idx + 1, rules, tokens_ident));
            let toks = defs.clone().map(|d| d.0);
            let idents = defs.map(|d| d.1);

            //let dir_set = compute_dir_set(rule, node, data.depth_limit, rules);
            //let check = generate_token_check(rule, &dir_set, data.simple_types, needs_check);

            q! {
                //#check
                let (inp, #node_ident) = (|| {
                     #(#toks)*

                    Ok((
                        inp,
                        ( #(#idents),* )
                    ))
                })()?;
            }
        }
        GrammarNode::Choice(choices, choice_idx) => {
            let defs = choices
                .iter()
                .enumerate()
                .map(|(idx, expr)| {
                    (
                        idx + 1,
                        expand_node(name, expr, false, idx + 1, rules, tokens_ident),
                    )
                })
                .map(|(idx, (toks, ident))| {
                    let idx_ident = idents::numeric(idx);
                    let choice_ident = idents::choice_enum(name, *choice_idx);

                    q! {
                        let r: ParserState<_> = (|| {
                            let inp = inp.clone();
                            #toks
                            Ok((inp, #ident))
                        })();
                        if let Ok((inp, ast)) = r {
                            return Ok((inp, #choice_ident::#idx_ident(ast)));
                        };
                    }
                });

            //let dir_set = compute_dir_set(rule, node, data.depth_limit, rules);
            //let check = generate_token_check(rule, &dir_set, data.simple_types, needs_check);

            q! {
                //#check
                let (inp, #node_ident) =  (|| {
                    #(#defs)*

                    Err(()) // All choices failed
                })()?;
            }
        }
        GrammarNode::Optional(opt) => {
            let (toks, ident) = expand_node(name, opt, false, 1, rules, tokens_ident);

            //let dir_set = compute_dir_set(rule, node, data.depth_limit, rules);
            //let check = generate_token_check(rule, &dir_set, data.simple_types, needs_check);

            q! {
                let res: ParserState<_> = (|| {
                    //#check
                    let inp = inp.clone();
                    #toks
                    Ok((inp, #ident))
                })();
                let (inp, #node_ident) =  match res {
                    Ok((new_inp, ast)) => (new_inp, Some(ast)),
                    Err(_) => (inp, None),
                };
            }
        }
    };

    (toks, node_ident)
}

#[derive(Clone)]
struct DirectionSet<'a> {
    tokens: HashSet<&'a String>,
    nullable: bool,
}

enum NullPropagation {
    IfAny,
    IfAll,
}

impl<'a> DirectionSet<'a> {
    fn nullable(self) -> DirectionSet<'a> {
        Self {
            tokens: self.tokens,
            nullable: true,
        }
    }

    fn extend(&mut self, other: DirectionSet<'a>, nullability: NullPropagation) {
        self.tokens.extend(other.tokens);

        match nullability {
            NullPropagation::IfAny => self.nullable = self.nullable || other.nullable,
            NullPropagation::IfAll => self.nullable = self.nullable && other.nullable,
        }
    }
}

impl<'a> Default for DirectionSet<'a> {
    fn default() -> Self {
        Self {
            tokens: HashSet::new(),
            nullable: false,
        }
    }
}

impl<'a, I> From<I> for DirectionSet<'a>
where
    I: IntoIterator<Item = &'a String>,
{
    fn from(value: I) -> Self {
        Self {
            tokens: value.into_iter().collect(),
            nullable: false,
        }
    }
}

// TODO:
fn compute_dir_set<'a>(
    current_rule: &String,
    node: &'a GrammarNode,
    depth: usize,
    rules: &'a Vec<GrammarRule>,
) -> DirectionSet<'a> {
    if depth == 0 {
        panic!("Possible left recursion deteted! Reached depth limit when computing the direction set for the rule: {}", current_rule);
    }

    match &node {
        GrammarNode::Rule(rule) => rules
            .iter()
            .find(|GrammarRule { name, .. }| name == rule)
            .map(|GrammarRule { name, node }| compute_dir_set(name, node, depth - 1, rules))
            .unwrap(),
        GrammarNode::Token(token) => [token].into(),
        GrammarNode::List(list) => {
            let mut dir_set = DirectionSet::default().nullable();

            for list_node in list {
                let node_set = compute_dir_set(current_rule, list_node, depth, rules);

                dir_set.extend(node_set, NullPropagation::IfAll);

                // stop at the first non-nullable node
                if !dir_set.nullable {
                    break;
                }
            }

            dir_set
        }
        GrammarNode::Optional(opt) => compute_dir_set(current_rule, opt, depth, rules).nullable(),
        GrammarNode::Choice(choices, _) => choices
            .into_iter()
            .map(|choice| compute_dir_set(current_rule, choice, depth, rules))
            .reduce(|mut a, b| {
                a.extend(b, NullPropagation::IfAny);
                a
            })
            .unwrap_or_default(),
    }
}
// TODO:
fn generate_token_check(
    rule: &str,
    check: &DirectionSet,
    simple_types: bool,
    needs_check: bool,
) -> TokenStream {
    if !needs_check || check.nullable {
        return q! {};
    }

    return q! {};
    //let token_names: Vec<_> = check
    //    .tokens
    //    .iter()
    //    .map(|token| tokens::enum_ident(token).to_string())
    //    .map(|token| quote! { #token.into() })
    //    .collect();
    //
    //let branches = check
    //    .tokens
    //    .iter()
    //    .map(|token| tokens::enum_ident(token))
    //    .map(|ident| quote! { #_Some(Token::#ident(..)) => {} });
    //
    //quote! {
    //    match inp.get_current() {
    //        #_None => return #_Err(#_ParseError::new(
    //            #rule.into(),
    //            inp.current,
    //            #_ParseErrorReason::UnexpectedEOI {
    //                expected: #_vec![#(#token_names),*],
    //            }
    //        )),
    //        #(#branches)*
    //        #_Some(tok) => return #_Err(#_ParseError::new(
    //            #rule.into(),
    //            inp.current,
    //            #_ParseErrorReason::UnexpectedToken {
    //                expected: #_vec![#(#token_names),*],
    //                token: tok.clone(),
    //            }
    //        ))
    //    };
    //}
}
