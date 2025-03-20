use std::{fmt::Display, iter::Peekable};

use proc_macro2::{token_stream::IntoIter, Span, TokenTree};
use quote::ToTokens;
use syn::parse::Parse;

use crate::data::{grammar::GrammarData, tokens::TokensData};

type TokenStreamIter = Peekable<IntoIter>;
type CombiResult<T> = syn::Result<(TokenStreamIter, T)>;

fn unexpected<T: Display>(span: Span, expected: T) -> syn::Error {
    syn::Error::new(span, format!("Expected {}.", expected))
}

fn eoi(attr_span: &Span) -> syn::Error {
    syn::Error::new(*attr_span, "Unexpected end of input.")
}

pub fn parse_grammar(
    _tokens_data: &TokensData,
    grammar_rules: Vec<GrammarData>,
) -> syn::Result<()> {
    let raw_rules = grammar_rules
        .into_iter()
        .map(|GrammarData { span, rule }| parse_rule(&span, rule.into_iter().peekable()))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(())
}

#[derive(Debug)]
enum RawGrammarNode {
    Name(String),
    Pattern(String),
    List(Vec<RawGrammarNode>),
    Optional(Box<RawGrammarNode>),
    Choice(Vec<RawGrammarNode>),
}

#[derive(Debug)]
struct RawGrammarRule {
    name: String,
    node: RawGrammarNode,
}

#[derive(Debug)]
pub enum GrammarNode {
    Rule(String),
    AssumedToken(String),
    List(Vec<GrammarNode>),
    Optional(Box<GrammarNode>),
    Choice(Vec<GrammarNode>, usize),
}

#[derive(Debug)]
pub struct GrammarRule {
    pub name: String,
    pub index: usize,
    pub node: GrammarNode,
}

fn parse_rule(attr_span: &Span, input: TokenStreamIter) -> syn::Result<RawGrammarRule> {
    let (input, name) = ident(attr_span, input)?;
    let (input, _) = punct(attr_span, input, '=')?;
    let (mut input, node) = choice(attr_span, input)?;

    if let Some(token) = input.next() {
        return Err(syn::Error::new(token.span(), "unexpected token"));
    }

    Ok(RawGrammarRule {
        name: name.to_string(),
        node,
    })
}

fn choice(attr_span: &Span, mut input: TokenStreamIter) -> CombiResult<RawGrammarNode> {
    let mut nodes = Vec::new();
    let mut list_node;

    (input, list_node) = list(attr_span, input)?;
    nodes.push(list_node);

    while matches!(input.peek(), Some(TokenTree::Punct(punct)) if punct.as_char() == '|') {
        input.next();
        (input, list_node) = list(attr_span, input)?;
        nodes.push(list_node);
    }

    let node = if nodes.len() == 1 {
        nodes.pop().unwrap()
    } else {
        RawGrammarNode::Choice(nodes)
    };
    Ok((input, node))
}

fn list(attr_span: &Span, mut input: TokenStreamIter) -> CombiResult<RawGrammarNode> {
    let mut items = Vec::new();
    let item;

    (input, item) = match list_item(attr_span, input) {
        ListItem::Ok(input, item) => (input, item),
        ListItem::Err(error) => return Err(error),
        ListItem::NotStrLit(_, error) => return Err(error),
        ListItem::Eoi(_) => return Err(eoi(attr_span)),
    };
    items.push(item);

    loop {
        let item;
        (input, item) = match list_item(attr_span, input) {
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

fn list_item(attr_span: &Span, mut input: TokenStreamIter) -> ListItem {
    fn group(attr_span: &Span, mut input: TokenStreamIter) -> ListItem {
        match input.next() {
            Some(TokenTree::Group(group)) => {
                let stream = group.stream().into_iter().peekable();
                match choice(attr_span, stream) {
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

    fn ident(_attr_span: &Span, mut input: TokenStreamIter) -> ListItem {
        match input.next() {
            Some(TokenTree::Ident(ident)) => {
                ListItem::Ok(input, RawGrammarNode::Name(ident.to_string()))
            }
            _ => unreachable!("unreachable in ident"),
        }
    }

    fn lit(attr_span: &Span, mut input: TokenStreamIter) -> ListItem {
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
            attr_span,
            token.to_token_stream().into_iter().peekable(),
            "a group, identifier or token pattern",
        );

        match lit {
            Ok((_, lit)) => {
                input.next();
                ListItem::Ok(input, RawGrammarNode::Pattern(lit.value()))
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

    let result = handler(attr_span, input);
    let ListItem::Ok(mut input, mut item) = result else {
        return result;
    };

    if matches!(input.peek(), Some(TokenTree::Punct(punct)) if punct.as_char() == '?') {
        input.next();
        item = RawGrammarNode::Optional(Box::new(item));
    }
    ListItem::Ok(input, item)
}

fn ident(attr_span: &Span, mut input: TokenStreamIter) -> CombiResult<String> {
    match input.next() {
        Some(TokenTree::Ident(ident)) => Ok((input, ident.to_string())),
        Some(token) => Err(unexpected(token.span(), "identifier")),
        None => Err(eoi(attr_span)),
    }
}

fn literal<T: Parse>(
    attr_span: &Span,
    mut input: TokenStreamIter,
    expected: &str,
) -> CombiResult<T> {
    let stream = match input.next() {
        Some(TokenTree::Literal(literal)) => literal.to_token_stream(),
        Some(token) => return Err(unexpected(token.span(), expected)),
        None => return Err(eoi(attr_span)),
    };

    match syn::parse2::<T>(stream) {
        Ok(lit) => Ok((input, lit)),
        Err(e) => Err(e),
    }
}

fn punct(attr_span: &Span, mut input: TokenStreamIter, chr: char) -> CombiResult<()> {
    match input.next() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == chr => Ok((input, ())),
        Some(token) => Err(unexpected(token.span(), chr)),
        None => Err(eoi(attr_span)),
    }
}
