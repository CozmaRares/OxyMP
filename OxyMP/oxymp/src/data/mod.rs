use syn::spanned::Spanned;

mod helpers;
pub mod tokens;

use helpers::{MarkerAttrError, OxyMPAttr};

#[derive(Debug)]
pub struct Data {
    pub tokens: tokens::TokensData,
    pub lexers: Vec<()>,

    #[cfg(feature = "rd")]
    pub rd_parsers: Vec<()>,

    #[cfg(feature = "lr")]
    pub lr_parsers: Vec<()>,

    pub initial_module: syn::ItemMod,
}

// TODO: use syn::Error::combine to emit multiple definition errors at once

pub fn process_module(mut module: syn::ItemMod) -> syn::Result<Data> {
    let Some((brace, mut items)) = module.content else {
        return Err(syn::Error::new(
            module.span(),
            "Module has no explicit content block (missing `{}`). \
            This macro requires a module with explicit contents. \
            Ensure the module contains at least one struct, enum, function, or other items inside the content block."
        ));
    };

    let mut tokens = Vec::new();

    for item in &mut items {
        let Some((oxymp_attr, span)) = helpers::remove_first_oxymp_attr(item)? else {
            continue;
        };

        let attrs = helpers::get_item_attrs(item).expect("item must have attrs");
        if helpers::find_oxymp_attr(attrs).is_some() {
            let mut attr_spans = vec![span];

            while let Some((_, span)) = helpers::remove_first_oxymp_attr(item)? {
                attr_spans.push(span);
            }

            let err = MarkerAttrError::MoreAttrs {
                item_span: helpers::get_ident_span(item),
                attr_spans,
            };

            return Err(err.into());
        }

        match oxymp_attr {
            OxyMPAttr::Tokens => {
                let data = tokens::process_tokens(item)?;
                tokens.push((data, helpers::get_item_ds_span(item)));
            }
            OxyMPAttr::Lexer => {}

            #[cfg(feature = "rd")]
            OxyMPAttr::RDParser => {}

            #[cfg(feature = "lr")]
            OxyMPAttr::LRParser => {}
        }
    }

    match tokens.len() {
        0 => {
            let msg =
                "Module marked with #[oxymp] must contain one enum marked with #[oxymp::Tokens].";
            return Err(syn::Error::new(module.ident.span(), msg));
        }

        1 => {}

        _ => {
            let msg = "Module marked with #[oxymp] must contain only one enum marked with #[oxymp::Tokens].";
            let mut iter = tokens.into_iter();
            let first = syn::Error::new(iter.next().expect("has at least 2 tokens").1, msg);

            let err = iter.fold(first, |mut acc, (_, span)| {
                acc.combine(syn::Error::new(span, "new token target found here"));
                acc
            });

            return Err(err);
        }
    }

    module.content = Some((brace, items));

    Ok(Data {
        tokens: tokens.pop().expect("has one token").0,
        lexers: Vec::new(),

        #[cfg(feature = "rd")]
        rd_parsers: Vec::new(),

        #[cfg(feature = "lr")]
        lr_parsers: Vec::new(),

        initial_module: module,
    })
}
