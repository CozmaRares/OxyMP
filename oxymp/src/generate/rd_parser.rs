#![allow(non_snake_case)]

use std::collections::HashSet;

use proc_macro2::TokenStream;

use crate::{
    data::{rd_parser::RDParserData, tokens::TokensData},
    grammar::{parse_grammar, GrammarNode, GrammarRule},
    idents,
    symbols::*,
    utils::FoldErrors,
};

pub fn generate(
    tokens_data: &TokensData,
    rd_parsers: Vec<(syn::ItemMod, RDParserData)>,
) -> syn::Result<Vec<syn::Item>> {
    let items = rd_parsers
        .into_iter()
        .map(|(item_mod, rd_data)| generate_one(tokens_data, item_mod, rd_data))
        .collect_errors()?;
    Ok(items)
}

fn generate_one(
    tokens_data: &TokensData,
    mut item_mod: syn::ItemMod,
    rd_data: RDParserData,
) -> syn::Result<syn::Item> {
    let rules = parse_grammar(tokens_data, rd_data.grammar_rules)?;

    let parse_tree = generate_parse_tree(&tokens_data.ident, &rules);
    let parser = generate_parser(&rules, &tokens_data.ident);

    let tokens_ident = &tokens_data.ident;

    let _Rc = Std::Rc.path();
    let _usize = Core::Usize.path();
    let _From = Trait::From.path();
    let _Vec = Std::Vec.path();
    let _Option = Std::Option.path();
    let _Result = Std::Result.path();
    let _Debug = Derive::Debug.path();
    let _Clone = Derive::Clone.path();
    let _String = Std::String.path();
    let _Display = Trait::Display.path();
    let _Formatter = Std::Formatter.path();
    let _FmtResult = Std::FmtResult.path();
    let _write = Macro::Write.path();
    let _writeln = Macro::Writeln.path();
    let _Ok = Std::Ok.path();
    let _Error = Trait::Error.path();
    let _Clone = Derive::Clone.path();
    let _format = Macro::Format.path();

    let parser_items = items! {
        type Token = super::#tokens_ident;

        #[derive(#_Debug, #_Clone)]
        pub struct Input {
            inp: #_Rc<[Token]>,
            cursor: #_usize,
        }
        impl #_From<#_Vec<Token>> for Input {
            fn from(value: #_Vec<Token>) -> Self {
                Self {
                    inp: value.into(),
                    cursor: 0,
                }
            }
        }
        impl Input {
            fn peek(&self) -> #_Option<&Token> {
                self.inp.get(self.cursor)
            }

            fn advance(&self) -> Self {
                Self {
                    inp: self.inp.clone(),
                    cursor: self.cursor + 1,
                }
            }

            fn input(&self) -> #_Rc<[Token]> {
                self.inp.clone()
            }
        }

        #[derive(#_Debug)]
        pub enum ErrorKind {
            UnexpectedEof,
            UnexpectedToken,
            ChoiceFailed {
                rule_name: #_String,
                choice_idx: #_usize,
                causes: #_Vec<Error>,
            },
        }

        impl #_Display for ErrorKind {
            fn fmt(&self, f: &mut #_Formatter<'_>) -> #_FmtResult {
                match self {
                    ErrorKind::UnexpectedEof => #_write!(f, "Unexpected end of input"),
                    ErrorKind::UnexpectedToken => #_write!(f, "Unexpected token"),
                    ErrorKind::ChoiceFailed { rule_name, choice_idx, causes } => {
                        #_write!(f, "Choice '{}', from rule '{}', failed", choice_idx, rule_name)?;
                        if !causes.is_empty() {
                            #_writeln!(f, " due to:")?;
                            for (i, cause) in causes.iter().enumerate() {
                                #_writeln!(f, "  {}. {}", i + 1, cause)?;
                            }
                        }
                        #_Ok(())
                    }
                }
            }
        }

        #[derive(#_Debug)]
        pub struct Error {
            kind: ErrorKind,
            input: #_Rc<[Token]>,
            cursor: #_usize,
            expected: #_Vec<#_String>,
        }

        impl #_Error for Error {}
        impl #_Display for Error {
            fn fmt(&self, f: &mut #_Formatter<'_>) -> #_FmtResult {
                #_writeln!(f, "Parse error at index {}", self.cursor)?;
                #_writeln!(f, "{}", self.kind)?;

                if !self.expected.is_empty() {
                    #_writeln!(f, "  Expected : {}", self.expected.join(", "))?;
                };

                #_writeln!(f, "Local context:")?;
                let len = self.input.len();
                let start = self.cursor.saturating_sub(5);
                let end = ::std::cmp::min(self.cursor + 5, len + 1);

                for i in start..end {
                    let display = if i < len {
                        #_format!("{}", self.input[i])
                    } else {
                        "<EOF>".to_string()
                    };
                    #_writeln!(f, " {:>4}: {}", i, display)?;
                }

                Ok(())
            }
        }

        pub type State<T> = #_Result<(Input, T), Error>;

        #parse_tree
        #parser
    };

    let (brace, _) = item_mod.content.expect("module should have content");
    item_mod.content = Some((brace, parser_items));
    Ok(syn::Item::Mod(item_mod))
}

fn generate_parse_tree(tokens_ident: &syn::Ident, rules: &[GrammarRule]) -> TokenStream {
    let _Debug = Derive::Debug.path();

    let structs = rules.iter().map(|GrammarRule { name, node }| {
        let ParseTreeNode {
            main_struct,
            external_choices,
        } = generate_parse_tree_node(name, node, tokens_ident);

        let external_choices = match external_choices {
            None => q! {},
            Some(structs) => q! { #(#structs)* },
        };

        let rule_ident = idents::parser_rule(name);

        q! {
            #external_choices
            #[derive(#_Debug)]
            pub struct #rule_ident {
                pub value: #main_struct,
            }
        }
    });

    q! {
        #(#structs)*
    }
}

struct ParseTreeNode {
    main_struct: TokenStream,
    external_choices: Option<Vec<TokenStream>>,
}

fn generate_parse_tree_node(
    rule_name: &String,
    node: &GrammarNode,
    tokens_ident: &syn::Ident,
) -> ParseTreeNode {
    let _Box = Std::Box.path();
    let _Option = Std::Option.path();
    let _Debug = Derive::Debug.path();

    match &node {
        GrammarNode::Rule(rule) => {
            let ident = idents::parser_rule(rule);
            ParseTreeNode {
                main_struct: q! { #_Box<#ident> },
                external_choices: None,
            }
        }
        GrammarNode::Token(token) => {
            let ident = idents::token_struct(tokens_ident, token);
            ParseTreeNode {
                main_struct: q! { super::#ident },
                external_choices: None,
            }
        }
        GrammarNode::List(exprs) => {
            let defs = exprs
                .iter()
                .map(|expr| generate_parse_tree_node(rule_name, expr, tokens_ident));
            let main_struct = defs.clone().map(|d| d.main_struct);
            let external_choices = defs.filter_map(|d| d.external_choices).flatten();

            ParseTreeNode {
                main_struct: q! { ( #(#main_struct),* ) },
                external_choices: Some(external_choices.collect()),
            }
        }
        GrammarNode::Choice(choices, choice_idx) => {
            let defs = choices
                .iter()
                .map(|choice| generate_parse_tree_node(rule_name, choice, tokens_ident));
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
            let enum_ident = idents::choice_enum(rule_name, *choice_idx);
            external_choices.push(q! {
                #[derive(#_Debug)]
                pub enum #enum_ident {
                    #(#enum_entries),*
                }
            });

            ParseTreeNode {
                main_struct: q! { #enum_ident },
                external_choices: Some(external_choices),
            }
        }
        GrammarNode::Optional(opt) => {
            let generated = generate_parse_tree_node(rule_name, opt, tokens_ident);
            let main_struct = generated.main_struct;
            ParseTreeNode {
                main_struct: q! { #_Option<#main_struct> },
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
    rule_name: &String,
    node: &GrammarNode,
    rules: &Vec<GrammarRule>,
    tokens_ident: &syn::Ident,
) -> TokenStream {
    let defs = expand_node(rule_name, node, false, 1, rules, tokens_ident);
    let toks = defs.0;
    let ident = defs.1;

    let rule_ident = idents::parser_rule(rule_name);

    let _Ok = Std::Ok.path();

    q! {
        pub fn #rule_ident<'a>(inp: Input) -> State<#rule_ident> {
            #toks
            #_Ok((
                inp,
                #rule_ident { value: #ident }
            ))
        }
    }
}

fn expand_node(
    rule_name: &str,
    node: &GrammarNode,
    needs_check: bool,
    node_idx: usize,
    rules: &Vec<GrammarRule>,
    tokens_ident: &syn::Ident,
) -> (TokenStream, proc_macro2::Ident) {
    let node_ident = idents::numeric(node_idx);

    let _Box = Std::Box.path();
    let _Some = Std::Some.path();
    let _None = Std::None.path();
    let _Err = Std::Err.path();
    let _Ok = Std::Ok.path();

    let toks = match &node {
        GrammarNode::Rule(nested_rule) => {
            let rule_ident = idents::parser_rule(nested_rule);
            // TODO: implement depth limit
            // let dir_set = compute_dir_set(nested_rule, node, 10, rules);
            // let check = generate_token_check(rule, &dir_set, data.simple_types, needs_check);

            q! {
                //#check
                let (inp, #node_ident) =
                    #rule_ident(inp)
                        .map(|(remaining, ast)| (remaining, #_Box::new(ast)))?;
            }
        }
        GrammarNode::Token(token) => {
            let token_struct_entry = idents::token_struct(tokens_ident, token);

            q! {
                let #_Some(_current) = inp.peek() else {
                    return #_Err(Error {
                        kind: ErrorKind::UnexpectedEof,
                        input: inp.input(),
                        cursor: inp.cursor,
                        expected: vec![], // TODO:
                    });
                };
                let (inp, #node_ident) = match super::#token_struct_entry::try_from_ref(_current) {
                    #_None => return #_Err(Error {
                        kind: ErrorKind::UnexpectedToken,
                        input: inp.input(),
                        cursor: inp.cursor,
                        expected: vec![], // TODO:
                    }),
                    #_Some(t) => (inp.advance(), t),
                };
            }
        }
        GrammarNode::List(exprs) => {
            let defs = exprs.iter().enumerate().map(|(idx, expr)| {
                expand_node(rule_name, expr, idx != 0, idx + 1, rules, tokens_ident)
            });
            let toks = defs.clone().map(|d| d.0);
            let idents = defs.map(|d| d.1);

            //let dir_set = compute_dir_set(rule, node, data.depth_limit, rules);
            //let check = generate_token_check(rule, &dir_set, data.simple_types, needs_check);

            q! {
                //#check
                let (inp, #node_ident) = (|| {
                     #(#toks)*

                    #_Ok((
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
                        expand_node(rule_name, expr, false, idx + 1, rules, tokens_ident),
                    )
                })
                .map(|(idx, (toks, ident))| {
                    let idx_ident = idents::numeric(idx);
                    let choice_ident = idents::choice_enum(rule_name, *choice_idx);

                    q! {
                        let r: State<_> = (|| {
                            let inp = inp.clone();
                            #toks
                            #_Ok((inp, #ident))
                        })();
                        if let #_Ok((inp, ast)) = r {
                            return #_Ok((inp, #choice_ident::#idx_ident(ast)));
                        };
                    }
                });

            //let dir_set = compute_dir_set(rule, node, data.depth_limit, rules);
            //let check = generate_token_check(rule, &dir_set, data.simple_types, needs_check);

            q! {
                //#check
                let (inp, #node_ident) =  (|| {
                    #(#defs)*

                    #_Err(Error {
                        kind: ErrorKind::ChoiceFailed {
                            rule_name: #rule_name.to_string(),
                            choice_idx: #choice_idx,
                            causes: vec![], // TODO:
                        },
                        input: inp.input(),
                        cursor: inp.cursor,
                        expected: vec![], // TODO:
                    })
                })()?;
            }
        }
        GrammarNode::Optional(opt) => {
            let (toks, ident) = expand_node(rule_name, opt, false, 1, rules, tokens_ident);

            //let dir_set = compute_dir_set(rule, node, data.depth_limit, rules);
            //let check = generate_token_check(rule, &dir_set, data.simple_types, needs_check);

            q! {
                let res: State<_> = (|| {
                    //#check
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

// TEST: this
#[derive(Clone, Default)]
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
    rule_name: &String,
    node: &'a GrammarNode,
    depth: usize,
    rules: &'a Vec<GrammarRule>,
) -> DirectionSet<'a> {
    if depth == 0 {
        panic!("Possible left recursion deteted! Reached the depth limit when computing the direction set for the rule: {}", rule_name);
    }

    match &node {
        GrammarNode::Rule(rule) => rules
            .iter()
            .find(|GrammarRule { name, .. }| name == rule)
            .map(|GrammarRule { name, node }| compute_dir_set(name, node, depth - 1, rules))
            .expect("rule should exist"),
        GrammarNode::Token(token) => [token].into(),
        GrammarNode::List(list) => {
            let mut dir_set = DirectionSet::default().nullable();

            for list_node in list {
                let node_set = compute_dir_set(rule_name, list_node, depth, rules);

                dir_set.extend(node_set, NullPropagation::IfAll);

                // stop at the first non-nullable node
                if !dir_set.nullable {
                    break;
                }
            }

            dir_set
        }
        GrammarNode::Optional(opt) => compute_dir_set(rule_name, opt, depth, rules).nullable(),
        GrammarNode::Choice(choices, _) => choices
            .iter()
            .map(|choice| compute_dir_set(rule_name, choice, depth, rules))
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
