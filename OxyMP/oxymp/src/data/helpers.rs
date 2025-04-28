use proc_macro2::Span;
use syn::spanned::Spanned;

pub enum AttrError {
    UnknowAttr(Span),
    MoreAttrs {
        item_span: Span,
        attr_spans: Vec<Span>,
    },
    Not2Segments(Span),
    PathArg(Span),
    Multi(Vec<AttrError>),
}

impl AttrError {
    fn add(self, other: Self) -> Self {
        let errs = match self {
            Self::Multi(mut errs) => {
                errs.push(other);
                errs
            }
            this => vec![this, other],
        };
        Self::Multi(errs)
    }
}

impl From<AttrError> for syn::Error {
    fn from(value: AttrError) -> Self {
        match value {
            AttrError::UnknowAttr(span) => {
                let msg = "Attribute containing 'oxymp', is not supported. Make sure you spelled it correctly, or have enabled the corresponding feature.";

                Self::new(span, msg)
            }

            AttrError::MoreAttrs {
                item_span,
                attr_spans,
            } => {
                let mut error = Self::new(
                    item_span,
                    "Item is marked with multiple #[oxymp::...] attributes. ",
                );

                attr_spans
                    .into_iter()
                    .map(|span| Self::new(span, "'oxymp' attribute used here"))
                    .for_each(|attr_err| error.combine(attr_err));

                error
            }

            AttrError::Not2Segments(span) => {
                let msg = "Attribute containing 'oxymp' must have exactly two path segments. Make sure it is in the form of #[oxymp::feature].";
                Self::new(span, msg)
            }

            AttrError::PathArg(span) => {
                let msg = "Generics in path arguments are not allowed. Please remove them.";
                Self::new(span, msg)
            }

            AttrError::Multi(errs) => {
                let mut iter = errs.into_iter().map(|err| err.into());
                let first = iter.next().expect("must have at least one error");

                iter.fold(first, |mut acc, err| {
                    acc.combine(err);
                    acc
                })
            }
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
    fn from_attr(attr: syn::Attribute) -> Result<(OxyMPAttr, Span), AttrError> {
        let segments: Vec<_> = attr.path().segments.iter().collect();
        let attr_span = attr.span();

        let mut errors = Vec::new();

        if segments.len() != 2 {
            errors.push(AttrError::Not2Segments(attr_span));
        }

        for segment in &segments {
            if !matches!(segment.arguments, syn::PathArguments::None) {
                errors.push(AttrError::PathArg(segment.span()));
            }
        }

        if !errors.is_empty() {
            return Err(AttrError::Multi(errors));
        }

        let feature = segments[1].ident.to_string();

        let oxymp_attr = match feature.as_str() {
            "Tokens" => OxyMPAttr::Tokens,

            "Lexer" => OxyMPAttr::Lexer,

            #[cfg(feature = "rd")]
            "RDParser" => OxyMPAttr::RDParser,

            #[cfg(feature = "lr")]
            "LRParser" => Self::LRParser,

            _ => return Err(AttrError::UnknowAttr(segments[1].span())),
        };

        Ok((oxymp_attr, attr_span))
    }
}

// credit to polkadot-sdk
pub fn remove_first_oxymp_attr(
    item: &mut syn::Item,
) -> Result<Option<(OxyMPAttr, Span)>, AttrError> {
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
