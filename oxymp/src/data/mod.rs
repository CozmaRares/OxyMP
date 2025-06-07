mod helpers;

pub mod grammar;
pub mod lexer;
pub mod rd_parser;
pub mod tokens;

use syn::spanned::Spanned;

use helpers::{MarkerAttrError, OxyMPAttr};

use crate::utils::{multiple_attribute_error, FoldErrors};

#[derive(Debug)]
pub struct Data {
    pub tokens: tokens::TokensData,
    pub lexers: Vec<(syn::ItemMod, lexer::LexerData)>,
    pub initial_module: syn::ItemMod,
    pub rd_parsers: Vec<(syn::ItemMod, rd_parser::RDParserData)>,
}

pub fn process_module(mut module: syn::ItemMod) -> syn::Result<Data> {
    let Some((brace, items)) = module.content else {
        return Err(syn::Error::new(
            module.span(),
            "Module has no explicit content block (missing `{}`). \
            This macro requires a module with explicit contents. \
            Ensure the module contains at least one struct, enum, function, or other items inside the content block."
        ));
    };

    let mut builder = DataBuilder::new();
    let iter = items.into_iter().map(|mut item| {
        let Some((oxymp_attr, span)) = helpers::remove_first_oxymp_attr(&mut item)? else {
            return Ok::<_, syn::Error>(Some(item));
        };

        let attrs = helpers::get_item_attrs(&mut item).expect("at this point item must have attrs");
        if helpers::find_oxymp_attr(attrs).is_some() {
            let mut attr_spans = vec![span];

            while let Some((_, span)) = helpers::remove_first_oxymp_attr(&mut item)? {
                attr_spans.push(span);
            }

            let err = MarkerAttrError::MoreAttrs {
                item_span: helpers::get_ident_span(&item),
                attr_spans,
            };

            return Err(err.into());
        }

        match oxymp_attr {
            OxyMPAttr::Tokens => {
                let data = tokens::process_tokens(&mut item)?;
                builder.add_tokens(helpers::get_item_ds_span(&item), data);
                Ok(Some(item))
            }
            OxyMPAttr::Lexer => {
                let (item, data) = lexer::process_lexer(item)?;
                builder.add_lexer(item, data);
                Ok(None)
            }
            OxyMPAttr::RDParser => {
                let (item, data) = rd_parser::process_rd_parser(item)?;
                builder.add_rd_parser(item, data);
                Ok(None)
            }
        }
    });

    let items = iter.collect_errors()?.into_iter().flatten().collect();

    module.content = Some((brace, items));
    builder.build(module)
}
struct DataBuilder {
    tokens: Vec<(proc_macro2::Span, tokens::TokensData)>,
    lexers: Vec<(syn::ItemMod, lexer::LexerData)>,
    rd_parsers: Vec<(syn::ItemMod, rd_parser::RDParserData)>,
}

impl DataBuilder {
    fn new() -> DataBuilder {
        DataBuilder {
            tokens: Vec::new(),
            lexers: Vec::new(),
            rd_parsers: Vec::new(),
        }
    }

    fn add_tokens(&mut self, span: proc_macro2::Span, data: tokens::TokensData) {
        self.tokens.push((span, data));
    }

    fn add_lexer(&mut self, item: syn::ItemMod, data: lexer::LexerData) {
        self.lexers.push((item, data));
    }

    fn add_rd_parser(&mut self, item: syn::ItemMod, data: rd_parser::RDParserData) {
        self.rd_parsers.push((item, data));
    }

    fn build(mut self, module: syn::ItemMod) -> syn::Result<Data> {
        match self.tokens.len() {
            0 => {
                let msg =
                "Module marked with #[oxymp] must contain one enum marked with #[oxymp::Tokens].";
                return Err(syn::Error::new(module.ident.span(), msg));
            }

            1 => {}

            _ => {
                let primary = "Module marked with #[oxymp] must contain only one enum marked with #[oxymp::Tokens].";
                let additional = "Token target defined here.";

                return Err(multiple_attribute_error(
                    self.tokens.iter().map(|(span, _)| *span).collect(),
                    primary,
                    additional,
                ));
            }
        }

        Ok(Data {
            tokens: self.tokens.pop().expect("has one token").1,
            lexers: self.lexers,
            initial_module: module,
            rd_parsers: self.rd_parsers,
        })
    }
}
