use std::{collections::HashMap, fmt::Display, iter::Peekable};

use proc_macro2::{token_stream::IntoIter, Span, TokenTree};
use quote::ToTokens;
use syn::{parse::Parse, spanned::Spanned};

use crate::data::{
    grammar::GrammarData,
    tokens::{TokenPattern, TokensData},
};

type TokenStreamIter = Peekable<IntoIter>;
type CombiResult<T> = syn::Result<(TokenStreamIter, T)>;

fn unexpected<T: Display>(span: Span, expected: T) -> syn::Error {
    syn::Error::new(span, format!("Expected {}.", expected))
}

fn eoi(span: &Span) -> syn::Error {
    syn::Error::new(*span, "Unexpected end of input.")
}

pub fn parse_grammar(
    tokens_data: &TokensData,
    grammar_rules: Vec<GrammarData>,
) -> syn::Result<Vec<GrammarRule>> {
    let raw_rules = grammar_rules
        .into_iter()
        .map(|GrammarData { span, rule }| parse_rule(&span, rule.into_iter().peekable()))
        .collect::<Result<Vec<_>, _>>()?;

    process_rules(tokens_data, raw_rules)
}

#[derive(Debug)]
enum RawGrammarNode {
    Name(String, Span),
    Pattern(String, Span),
    List(Vec<RawGrammarNode>),
    Optional(Box<RawGrammarNode>),
    Choice(Vec<RawGrammarNode>),
}

#[derive(Debug)]
struct RawGrammarRule {
    name: String,
    name_span: Span,
    node: RawGrammarNode,
}

#[derive(Debug)]
pub enum GrammarNode {
    Rule(String),
    Token(String),
    List(Vec<GrammarNode>),
    Optional(Box<GrammarNode>),
    Choice(Vec<GrammarNode>, usize),
}

#[derive(Debug)]
pub struct GrammarRule {
    pub name: String,
    pub node: GrammarNode,
}

fn parse_rule(attr_span: &Span, mut input: TokenStreamIter) -> syn::Result<RawGrammarRule> {
    let Some(name) = input.next() else {
        return Err(eoi(attr_span));
    };
    let name = get_ident(name)?;
    let (input, _) = punct(attr_span, input, '=')?;
    let (mut input, node) = choice(attr_span, input)?;

    if let Some(token) = input.next() {
        return Err(syn::Error::new(token.span(), "unexpected token"));
    }

    Ok(RawGrammarRule {
        name: name.to_string(),
        name_span: name.span(),
        node,
    })
}

fn choice(last_group_span: &Span, mut input: TokenStreamIter) -> CombiResult<RawGrammarNode> {
    let mut nodes = Vec::new();
    let mut list_node;

    (input, list_node) = list(last_group_span, input)?;
    nodes.push(list_node);

    while matches!(input.peek(), Some(TokenTree::Punct(punct)) if punct.as_char() == '|') {
        input.next();
        (input, list_node) = list(last_group_span, input)?;
        nodes.push(list_node);
    }

    let node = if nodes.len() == 1 {
        nodes.pop().unwrap()
    } else {
        RawGrammarNode::Choice(nodes)
    };
    Ok((input, node))
}

fn list(last_group_span: &Span, mut input: TokenStreamIter) -> CombiResult<RawGrammarNode> {
    let mut items = Vec::new();
    let item;

    (input, item) = match list_item(last_group_span, input) {
        ListItem::Ok(input, item) => (input, item),
        ListItem::Err(error) => return Err(error),
        ListItem::NotStrLit(_, error) => return Err(error),
        ListItem::Eoi(_) => return Err(eoi(last_group_span)),
    };
    items.push(item);

    loop {
        let item;
        (input, item) = match list_item(last_group_span, input) {
            ListItem::Ok(input, item) => (input, Some(item)),
            ListItem::NotStrLit(inner_input, _) => (inner_input, None),
            ListItem::Eoi(inner_input) => (inner_input, None),
            ListItem::Err(error) => return Err(error),
        };
        match item {
            Some(item) => items.push(item),
            None => break,
        }
    }
    let node = if items.len() == 1 {
        items.pop().unwrap()
    } else {
        RawGrammarNode::List(items)
    };
    Ok((input, node))
}

#[derive(Debug)]
enum ListItem {
    Ok(TokenStreamIter, RawGrammarNode),
    NotStrLit(TokenStreamIter, syn::Error),
    Eoi(TokenStreamIter),
    Err(syn::Error),
}

fn list_item(last_group_span: &Span, mut input: TokenStreamIter) -> ListItem {
    fn group(_: &Span, mut input: TokenStreamIter) -> ListItem {
        match input.next() {
            Some(TokenTree::Group(group)) => {
                let stream = group.stream().into_iter().peekable();
                match choice(&group.span(), stream) {
                    Ok((mut group_input, node)) => {
                        if let Some(token) = group_input.next() {
                            return ListItem::Err(syn::Error::new(
                                token.span(),
                                "unexpected token",
                            ));
                        }
                        ListItem::Ok(input, node)
                    }
                    Err(e) => ListItem::Err(e),
                }
            }
            _ => unreachable!("unreachable in group"),
        }
    }

    fn ident(_: &Span, mut input: TokenStreamIter) -> ListItem {
        match input.next() {
            Some(ident) => match get_ident(ident) {
                Ok(ident) => {
                    ListItem::Ok(input, RawGrammarNode::Name(ident.to_string(), ident.span()))
                }
                Err(e) => ListItem::Err(e),
            },
            _ => unreachable!("unreachable in ident"),
        }
    }

    fn lit(last_group_span: &Span, mut input: TokenStreamIter) -> ListItem {
        const ERR_MSG: &str = "a group, identifier or token pattern";

        let token = match input.peek() {
            Some(TokenTree::Literal(lit)) => lit,
            Some(token) => {
                let span = token.span();
                return ListItem::NotStrLit(input, unexpected(span, ERR_MSG));
            }
            None => return ListItem::Eoi(input),
        };

        let lit = literal::<syn::LitStr>(
            last_group_span,
            token.to_token_stream().into_iter().peekable(),
            "a group, identifier or token pattern",
        );

        match lit {
            Ok((_, lit)) => {
                let value = lit.value();
                if value.is_empty() {
                    return ListItem::Err(syn::Error::new(
                        lit.span(),
                        "A pattern cannot be empty. Consider using something inside the quotes.",
                    ));
                }

                input.next();
                ListItem::Ok(input, RawGrammarNode::Pattern(lit.value(), lit.span()))
            }
            Err(e) => ListItem::NotStrLit(input, e),
        }
    }

    let handler = match input.peek() {
        Some(TokenTree::Group(_)) => group,
        Some(TokenTree::Ident(_)) => ident,
        Some(_) => lit,
        None => return ListItem::Eoi(input),
    };

    let result = handler(last_group_span, input);
    let ListItem::Ok(mut input, mut item) = result else {
        return result;
    };

    if matches!(input.peek(), Some(TokenTree::Punct(punct)) if punct.as_char() == '?') {
        let question = input.next();

        if matches!(item, RawGrammarNode::Optional(_)) {
            return ListItem::Err(syn::Error::new(
                question.span(),
                "Optional nodes cannot be nested",
            ));
        }

        item = RawGrammarNode::Optional(Box::new(item));
    }
    ListItem::Ok(input, item)
}

fn get_ident(token: TokenTree) -> syn::Result<syn::Ident> {
    let stream = token.to_token_stream();
    syn::parse2(stream)
}

fn literal<T: Parse>(
    last_group_span: &Span,
    mut input: TokenStreamIter,
    expected: &str,
) -> CombiResult<T> {
    let stream = match input.next() {
        Some(TokenTree::Literal(literal)) => literal.to_token_stream(),
        Some(token) => return Err(unexpected(token.span(), expected)),
        None => return Err(eoi(last_group_span)),
    };

    syn::parse2::<T>(stream).map(|lit| (input, lit))
}

fn punct(attr_span: &Span, mut input: TokenStreamIter, chr: char) -> CombiResult<()> {
    match input.next() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == chr => Ok((input, ())),
        Some(token) => Err(unexpected(token.span(), chr)),
        None => Err(eoi(attr_span)),
    }
}

enum NameType {
    Token,
    Rule,
}

fn process_rules(
    tokens_data: &TokensData,
    raw_rules: Vec<RawGrammarRule>,
) -> syn::Result<Vec<GrammarRule>> {
    let mut known_names: HashMap<String, NameType> = HashMap::new();
    let mut token_patterns: HashMap<String, String> = HashMap::new();

    for variant in &tokens_data.variants {
        known_names.insert(variant.ident.clone(), NameType::Token);

        let pattern = match &variant.pattern {
            TokenPattern::Exact { pattern } => pattern,
            TokenPattern::Regex { pattern, .. } => pattern,
        }
        .clone();
        token_patterns.insert(pattern, variant.ident.clone());
    }

    for rule in &raw_rules {
        if known_names
            .insert(rule.name.clone(), NameType::Rule)
            .is_some()
        {
            return Err(syn::Error::new(
                rule.name_span,
                "There is a name conflict with a token of the same name.",
            ));
        }
    }

    raw_rules
        .into_iter()
        .map(|rule| cook_rule(rule, &known_names, &token_patterns))
        .collect::<syn::Result<Vec<_>>>()
}

// TODO: find a better name
fn cook_rule(
    rule: RawGrammarRule,
    known_names: &HashMap<String, NameType>,
    token_patterns: &HashMap<String, String>,
) -> syn::Result<GrammarRule> {
    let node = cook_node(rule.node, known_names, token_patterns, 1)?;
    Ok(GrammarRule {
        name: rule.name,
        node,
    })
}

fn cook_node(
    node: RawGrammarNode,
    known_names: &HashMap<String, NameType>,
    token_patterns: &HashMap<String, String>,
    choice_number: usize,
) -> syn::Result<GrammarNode> {
    match node {
        RawGrammarNode::Name(name, span) => match known_names.get(&name) {
            Some(NameType::Rule) => Ok(GrammarNode::Rule(name)),
            Some(NameType::Token) => Ok(GrammarNode::Token(name)),
            None => Err(syn::Error::new(span, "There is no known such name.")),
        },
        RawGrammarNode::Pattern(pattern, span) => match token_patterns.get(&pattern) {
            Some(token_name) => Ok(GrammarNode::Token(token_name.clone())),
            None => Err(syn::Error::new(
                span,
                "There is no known such token pattern.",
            )),
        },
        RawGrammarNode::List(items) => {
            let items = items
                .into_iter()
                .map(|item| cook_node(item, known_names, token_patterns, choice_number))
                .collect::<syn::Result<Vec<_>>>()?;

            Ok(GrammarNode::List(items))
        }
        RawGrammarNode::Optional(node) => {
            let node = cook_node(*node, known_names, token_patterns, choice_number)?;
            Ok(GrammarNode::Optional(Box::new(node)))
        }
        RawGrammarNode::Choice(choices) => {
            let choices = choices
                .into_iter()
                .map(|choice| cook_node(choice, known_names, token_patterns, choice_number + 1))
                .collect::<syn::Result<Vec<_>>>()?;
            Ok(GrammarNode::Choice(choices, choice_number))
        }
    }
}

// TODO: more tests
#[cfg(test)]
mod tests {
    impl std::cmp::PartialEq for RawGrammarNode {
        fn eq(&self, other: &Self) -> bool {
            match (self, other) {
                (RawGrammarNode::Name(a, _), RawGrammarNode::Name(b, _)) => a == b,
                (RawGrammarNode::Pattern(a, _), RawGrammarNode::Pattern(b, _)) => a == b,
                (RawGrammarNode::List(a), RawGrammarNode::List(b)) => a == b,
                (RawGrammarNode::Optional(a), RawGrammarNode::Optional(b)) => a == b,
                (RawGrammarNode::Choice(a), RawGrammarNode::Choice(b)) => a == b,
                _ => false,
            }
        }
    }

    use super::*;
    use proc_macro2::Span;
    use quote::quote;

    macro_rules! toks {
        ($($tokens:tt)*) => {
            quote! { $($tokens)* }.into_iter().peekable()
        };
    }

    mod parse_rule {
        use super::*;
    }

    mod choice {
        use super::*;
    }

    mod list {
        use super::*;
    }

    mod list_item {
        use super::*;

        #[test]
        fn string() {
            let strings = [
                "hello",
                "hello world",
                "hello \"world\"",
                "hello\nworld",
                "hello\tworld",
                "你好世界",
                r"C:\path\to\file",
            ];

            for s in strings {
                let input = toks!(#s);
                let item = list_item(&Span::call_site(), input);
                assert!(matches!(item, ListItem::Ok(_, _)));
                let ListItem::Ok(_, item) = item else {
                    unreachable!()
                };
                let s = s.to_string();
                assert!(matches!(item, RawGrammarNode::Pattern(s, _)));
            }
        }

        #[test]
        fn no_empty_string() {
            let input = toks!("");
            let item = list_item(&Span::call_site(), input);
            assert!(matches!(item, ListItem::Err(_)));
        }

        #[test]
        fn ident() {
            let idents = [
                "my_variable",
                "variable123",
                "my_long_variable_name",
                "_private_variable",
                "MyVariable",
                "r#let",
            ];

            for ident in idents {
                let input = quote::format_ident!("{}", ident);
                let input = toks!(#input);

                let item = list_item(&Span::call_site(), input);
                assert!(matches!(item, ListItem::Ok(_, _)));

                let ListItem::Ok(_, item) = item else {
                    unreachable!()
                };
                assert_eq!(
                    item,
                    RawGrammarNode::Name(ident.to_string(), Span::call_site())
                );
            }
        }

        #[test]
        fn keyword_as_ident() {
            let input = toks! { let };
            eprintln!("{:#?}", input);
            let item = list_item(&Span::call_site(), input);
            eprintln!("{:#?}", item);
            assert!(matches!(item, ListItem::Err(_)));
        }

        #[test]
        fn group() {
            let groups = [
                (
                    toks! { (a) },
                    RawGrammarNode::Name("a".to_string(), Span::call_site()),
                ),
                (
                    toks! { (a b) },
                    RawGrammarNode::List(vec![
                        RawGrammarNode::Name("a".to_string(), Span::call_site()),
                        RawGrammarNode::Name("b".to_string(), Span::call_site()),
                    ]),
                ),
                (
                    toks! { (a | b) },
                    RawGrammarNode::Choice(vec![
                        RawGrammarNode::Name("a".to_string(), Span::call_site()),
                        RawGrammarNode::Name("b".to_string(), Span::call_site()),
                    ]),
                ),
                (
                    toks! { ((a | b) c) },
                    RawGrammarNode::List(vec![
                        RawGrammarNode::Choice(vec![
                            RawGrammarNode::Name("a".to_string(), Span::call_site()),
                            RawGrammarNode::Name("b".to_string(), Span::call_site()),
                        ]),
                        RawGrammarNode::Name("c".to_string(), Span::call_site()),
                    ]),
                ),
                (
                    toks! { (a?) },
                    RawGrammarNode::Optional(Box::new(RawGrammarNode::Name(
                        "a".to_string(),
                        Span::call_site(),
                    ))),
                ),
                (
                    toks! { ("abc") },
                    RawGrammarNode::Pattern("abc".to_string(), Span::call_site()),
                ),
            ];

            for (input, output) in groups {
                let item = list_item(&Span::call_site(), input);
                assert!(matches!(item, ListItem::Ok(_, _)));
                let ListItem::Ok(_, item) = item else {
                    unreachable!()
                };
                assert_eq!(item, output);
            }
        }

        #[test]
        fn no_empty_group() {
            let group = toks! { () };
            let item = list_item(&Span::call_site(), group);
            assert!(matches!(item, ListItem::Err(_)));
        }

        #[test]
        fn optional() {
            let tests = [
                (
                    toks! { "abc"? },
                    RawGrammarNode::Optional(Box::new(RawGrammarNode::Pattern(
                        "abc".to_string(),
                        Span::call_site(),
                    ))),
                ),
                (
                    toks! { a? },
                    RawGrammarNode::Optional(Box::new(RawGrammarNode::Name(
                        "a".to_string(),
                        Span::call_site(),
                    ))),
                ),
                (
                    toks! { (a)? },
                    RawGrammarNode::Optional(Box::new(RawGrammarNode::Name(
                        "a".to_string(),
                        Span::call_site(),
                    ))),
                ),
                (
                    toks! { a?? }, // should only parse one question mark
                    RawGrammarNode::Optional(Box::new(RawGrammarNode::Name(
                        "a".to_string(),
                        Span::call_site(),
                    ))),
                ),
            ];

            for (input, output) in tests {
                let item = list_item(&Span::call_site(), input);
                assert!(matches!(item, ListItem::Ok(_, _)));
                let ListItem::Ok(_, item) = item else {
                    unreachable!()
                };
                assert_eq!(item, output);
            }
        }

        #[test]
        fn invalid_optional() {
            let tests: Vec<(Peekable<IntoIter>, Box<dyn Fn(ListItem) -> bool>)> = vec![
                (
                    toks!((a?)?),
                    Box::new(|item| matches!(item, ListItem::Err(_))),
                ),
                (
                    toks!(?),
                    Box::new(|item| matches!(item, ListItem::NotStrLit(_, _))),
                ),
            ];

            for (input, check) in tests {
                let item = list_item(&Span::call_site(), input);
                eprintln!("{:#?}", item);
                assert!(check(item));
            }
        }
    }
}
