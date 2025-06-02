use std::collections::{HashMap, HashSet};

use proc_macro2::Span;
use syn::spanned::Spanned;

use crate::utils::FoldErrors;

pub enum AttrError {
    PathArg(Span),
    SegmentError { span: Span, msg: &'static str },
}

impl From<AttrError> for MarkerAttrError {
    fn from(value: AttrError) -> MarkerAttrError {
        MarkerAttrError::AttrSyntax(value)
    }
}

impl From<Vec<AttrError>> for MarkerAttrError {
    fn from(value: Vec<AttrError>) -> MarkerAttrError {
        MarkerAttrError::Multi(value.into_iter().map(Into::into).collect())
    }
}

impl From<AttrError> for syn::Error {
    fn from(value: AttrError) -> Self {
        match value {
            AttrError::PathArg(span) => {
                let msg = "Generics in path arguments are not allowed. Please remove them.";
                Self::new(span, msg)
            }
            AttrError::SegmentError { span, msg } => Self::new(span, msg),
        }
    }
}

pub enum MarkerAttrError {
    UnknowAttr(Span),
    MoreAttrs {
        item_span: Span,
        attr_spans: Vec<Span>,
    },
    AttrSyntax(AttrError),
    Multi(Vec<MarkerAttrError>),
}

impl From<MarkerAttrError> for syn::Error {
    fn from(value: MarkerAttrError) -> Self {
        match value {
            MarkerAttrError::UnknowAttr(span) => {
                let msg = "Attribute containing 'oxymp', is not supported. Make sure you spelled it correctly, or have enabled the corresponding feature.";

                Self::new(span, msg)
            }

            MarkerAttrError::MoreAttrs {
                item_span,
                attr_spans,
            } => {
                let mut error = Self::new(
                    item_span,
                    "Item is marked with multiple #[oxymp::...] attributes. ",
                );

                attr_spans
                    .into_iter()
                    .map(|span| Self::new(span, "attribute used here"))
                    .for_each(|attr_err| error.combine(attr_err));

                error
            }

            MarkerAttrError::AttrSyntax(e) => e.into(),

            MarkerAttrError::Multi(errs) => errs.collect_errors(),
        }
    }
}

pub enum OxyMPAttr {
    Tokens,
    Lexer,

    #[cfg(feature = "rd")]
    RDParser,

    #[cfg(feature = "lr")]
    LRParser,
}

impl OxyMPAttr {
    fn from_attr(attr: syn::Attribute) -> Result<(OxyMPAttr, Span), MarkerAttrError> {
        let segments: Vec<_> = attr.path().segments.iter().collect();
        let span = attr.span();

        let mut errors = Vec::new();

        oxymp_assert!(
            segments
                .first()
                .map(|segment| segment.ident.to_string())
                .unwrap_or_default()
                == "oxymp",
            "The first segment of the attribute must be 'oxymp'"
        );

        if segments.len() != 2 {
            errors.push(AttrError::SegmentError {
                span,
                msg: "Attribute containing 'oxymp' must have exactly two path segments. Make sure it is in the form of #[oxymp::feature].",
            });
        }

        for segment in &segments {
            if segment.arguments != syn::PathArguments::None {
                errors.push(AttrError::PathArg(segment.span()));
            }
        }

        if !errors.is_empty() {
            return Err(errors.into());
        }

        let feature = segments[1].ident.to_string();

        let oxymp_attr = match feature.as_str() {
            "Tokens" => OxyMPAttr::Tokens,

            "Lexer" => OxyMPAttr::Lexer,

            #[cfg(feature = "rd")]
            "RDParser" => OxyMPAttr::RDParser,

            #[cfg(feature = "lr")]
            "LRParser" => Self::LRParser,

            _ => return Err(MarkerAttrError::UnknowAttr(segments[1].span())),
        };

        Ok((oxymp_attr, attr.span()))
    }
}

// credit to polkadot-sdk
pub fn remove_first_oxymp_attr(
    item: &mut syn::Item,
) -> Result<Option<(OxyMPAttr, Span)>, MarkerAttrError> {
    let Some(attrs) = get_item_attrs(item) else {
        return Ok(None);
    };

    let Some(idx) = find_oxymp_attr(attrs) else {
        return Ok(None);
    };

    let attr = attrs.remove(idx);
    let attr = OxyMPAttr::from_attr(attr)?;
    Ok(Some(attr))
}

pub fn find_oxymp_attr(attrs: &[syn::Attribute]) -> Option<usize> {
    attrs.iter().position(|attr| {
        attr.path()
            .segments
            .first()
            .map_or(false, |segment| segment.ident == "oxymp")
    })
}

pub fn get_item_attrs(item: &mut syn::Item) -> Option<&mut Vec<syn::Attribute>> {
    match item {
        syn::Item::Enum(item) => Some(item.attrs.as_mut()),
        syn::Item::ExternCrate(item) => Some(item.attrs.as_mut()),
        syn::Item::Fn(item) => Some(item.attrs.as_mut()),
        syn::Item::ForeignMod(item) => Some(item.attrs.as_mut()),
        syn::Item::Impl(item) => Some(item.attrs.as_mut()),
        syn::Item::Macro(item) => Some(item.attrs.as_mut()),
        syn::Item::Mod(item) => Some(item.attrs.as_mut()),
        syn::Item::Static(item) => Some(item.attrs.as_mut()),
        syn::Item::Struct(item) => Some(item.attrs.as_mut()),
        syn::Item::Trait(item) => Some(item.attrs.as_mut()),
        syn::Item::TraitAlias(item) => Some(item.attrs.as_mut()),
        syn::Item::Type(item) => Some(item.attrs.as_mut()),
        syn::Item::Union(item) => Some(item.attrs.as_mut()),
        syn::Item::Use(item) => Some(item.attrs.as_mut()),
        _ => None,
    }
}

pub fn get_ident_span(item: &syn::Item) -> Span {
    match item {
        syn::Item::Const(item) => item.ident.span(),
        syn::Item::Enum(item) => item.ident.span(),
        syn::Item::ExternCrate(item) => item.ident.span(),
        syn::Item::Fn(item) => item.sig.ident.span(),
        syn::Item::Impl(item) => item.self_ty.span(),
        syn::Item::Macro(item) => item.ident.span(),
        syn::Item::Mod(item) => item.ident.span(),
        syn::Item::Static(item) => item.ident.span(),
        syn::Item::Struct(item) => item.ident.span(),
        syn::Item::Trait(item) => item.ident.span(),
        syn::Item::TraitAlias(item) => item.ident.span(),
        syn::Item::Type(item) => item.ident.span(),
        syn::Item::Union(item) => item.ident.span(),
        _ => item.span(),
    }
}

pub fn get_item_ds_span(item: &syn::Item) -> proc_macro2::Span {
    match item {
        syn::Item::Enum(item) => item.enum_token.span,
        syn::Item::ExternCrate(item) => item.extern_token.span,
        syn::Item::Fn(item) => item.sig.fn_token.span,
        syn::Item::ForeignMod(item) => item.abi.extern_token.span,
        syn::Item::Impl(item) => item.impl_token.span,
        syn::Item::Macro(item) => item.mac.path.span(),
        syn::Item::Mod(item) => item.mod_token.span,
        syn::Item::Static(item) => item.static_token.span,
        syn::Item::Struct(item) => item.struct_token.span,
        syn::Item::Trait(item) => item.trait_token.span,
        syn::Item::TraitAlias(item) => item.trait_token.span,
        syn::Item::Type(item) => item.type_token.span,
        syn::Item::Union(item) => item.union_token.span,
        syn::Item::Use(item) => item.use_token.span,
        item => item.span(),
    }
}

pub const TRAILING_TOKENS_ERR: &str =
    "Unexpected remaining tokens after parsing the attribute. Remove the trailing tokens";

type Map = HashMap<String, Vec<(proc_macro2::Span, proc_macro2::TokenStream)>>;

pub fn process_module_helper(
    item: syn::Item,
    known_attrs: &[&str],
) -> syn::Result<(syn::ItemMod, Map)> {
    let syn::Item::Mod(mut item) = item else {
        return Err(syn::Error::new(
            get_item_ds_span(&item),
            "Item must be a module.",
        ));
    };

    match &item.content {
        Some((_, items)) if items.is_empty() => Ok(()),
        Some((_, items)) => {
            let span = items.first().expect("has 1 item").span();
            Err((span, "No items allowed in module."))
        }
        None => Err((item.ident.span(), "Missing `{}`.")),
    }
    .map_err(|(span, msg)| {
        let msg = format!("Module must have an empty content block. {}", msg);
        syn::Error::new(span, msg)
    })?;

    let mut attributes = Vec::new();

    let known_attrs: HashSet<_> = known_attrs.iter().copied().collect();
    let mut found_attrs = Map::new();

    for attr in item.attrs {
        let path = attr.path();

        if path.segments.len() != 1 {
            attributes.push(attr);
            continue;
        }

        let ident = path.segments.first().unwrap().ident.clone();
        let ident_lit = ident.to_string();

        if !known_attrs.contains(ident_lit.as_str()) {
            attributes.push(attr);
            continue;
        }

        let tokens = if let syn::Meta::List(meta_list) = attr.meta {
            meta_list.tokens
        } else {
            return Err(syn::Error::new(
                attr.span(),
                format!("Invalid attribute. Expected `#[{}(...)]`", ident_lit),
            ));
        };

        found_attrs
            .entry(ident_lit)
            .or_default()
            .push((ident.span(), tokens));
    }

    item.attrs = attributes;
    Ok((item, found_attrs))
}
