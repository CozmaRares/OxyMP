use syn::{parse_quote, spanned::Spanned};

#[derive(Debug)]
pub enum AttributeTarget {
    Tokens(syn::ItemEnum),
    Lexer(syn::ItemStruct),

    #[cfg(feature = "rd")]
    RDParser(syn::ItemStruct),

    #[cfg(feature = "lr")]
    LRParser(syn::ItemStruct),
}

#[derive(Debug)]
pub enum ItemParseResult {
    Ok(AttributeTarget),
    Err(syn::Error),
    Ignore(syn::Item),
}

trait Marker {
    fn get_attr(&self, attribute: syn::Attribute) -> Option<usize>;
    fn attr_starts_with(&self, path_segment: &str) -> Option<&syn::Attribute>;
}

impl Marker for Vec<syn::Attribute> {
    fn get_attr(&self, attribute: syn::Attribute) -> Option<usize> {
        self.iter().position(|attr| attr == &attribute)
    }

    fn attr_starts_with(&self, path_segment: &str) -> Option<&syn::Attribute> {
        self.iter().find(|attr| {
            attr.path()
                .segments
                .first()
                .map(|segment| segment.ident.to_string() == path_segment)
                .is_some()
        })
    }
}

trait ItemHelper {
    fn get_ds_span(&self) -> proc_macro2::Span;
    fn get_variant_name(&self) -> &'static str;
}

impl ItemHelper for syn::Item {
    // all variants that could have attributes
    fn get_variant_name(&self) -> &'static str {
        match self {
            syn::Item::ExternCrate(item) => "extern crate",
            syn::Item::Fn(item) => "function",
            syn::Item::ForeignMod(item) => "foreign module",
            syn::Item::Impl(item) => "impl block",
            syn::Item::Macro(item) => "macro",
            syn::Item::Mod(item) => "module",
            syn::Item::Static(item) => "static variable",
            syn::Item::Trait(item) => "trait",
            syn::Item::TraitAlias(item) => "trait alias",
            syn::Item::Type(item) => "type definition",
            syn::Item::Use(item) => "use",
            _ => "",
        }
    }

    fn get_ds_span(&self) -> proc_macro2::Span {
        match self {
            syn::Item::ExternCrate(item) => item.extern_token.span,
            syn::Item::Fn(item) => item.sig.fn_token.span,
            syn::Item::ForeignMod(item) => item.abi.extern_token.span,
            syn::Item::Impl(item) => item.impl_token.span,
            syn::Item::Macro(item) => item.mac.span(),
            syn::Item::Mod(item) => item.mod_token.span,
            syn::Item::Static(item) => item.static_token.span,
            syn::Item::Trait(item) => item.trait_token.span,
            syn::Item::TraitAlias(item) => item.trait_token.span,
            syn::Item::Type(item) => item.type_token.span,
            syn::Item::Use(item) => item.use_token.span,
            item => item.span(),
        }
    }
}

impl AttributeTarget {
    pub fn new(item: syn::Item) -> ItemParseResult {
        let attrs = match &item {
            syn::Item::Enum(item) => Some(&item.attrs),
            syn::Item::Struct(item) => Some(&item.attrs),
            syn::Item::Union(item) => Some(&item.attrs),
            syn::Item::ExternCrate(item) => Some(&item.attrs),
            syn::Item::Fn(item) => Some(&item.attrs),
            syn::Item::ForeignMod(item) => Some(&item.attrs),
            syn::Item::Impl(item) => Some(&item.attrs),
            syn::Item::Macro(item) => Some(&item.attrs),
            syn::Item::Mod(item) => Some(&item.attrs),
            syn::Item::Static(item) => Some(&item.attrs),
            syn::Item::Trait(item) => Some(&item.attrs),
            syn::Item::TraitAlias(item) => Some(&item.attrs),
            syn::Item::Type(item) => Some(&item.attrs),
            syn::Item::Use(item) => Some(&item.attrs),
            _ => None,
        };

        let Some(attrs) = attrs else {
            return ItemParseResult::Ignore(item);
        };

        if let Some(attr_idx) = attrs.get_attr(parse_quote! { #[oxymp::Tokens] }) {
            return match item {
                syn::Item::Enum(mut item) => {
                    item.attrs.remove(attr_idx);
                    pipe!(item => AttributeTarget::Tokens => ItemParseResult::Ok)
                }
                item => ItemParseResult::Err(syn::Error::new(
                    item.get_ds_span(),
                    format!(
                        "'{}' cannot be marked with #[oxymp::Tokens]. Please use an enum.",
                        item.get_variant_name()
                    ),
                )),
            };
        }

        if let Some(attr_idx) = attrs.get_attr(parse_quote! { #[oxymp::Lexer] }) {
            return match item {
                syn::Item::Struct(mut item) => {
                    item.attrs.remove(attr_idx);
                    pipe!(item => AttributeTarget::Lexer => ItemParseResult::Ok)
                }
                item => ItemParseResult::Err(syn::Error::new(
                    item.get_ds_span(),
                    format!(
                        "'{}' cannot be marked with #[oxymp::Lexer]. Please use a struct.",
                        item.get_variant_name()
                    ),
                )),
            };
        }

        #[cfg(feature = "rd")]
        if let Some(attr_idx) = attrs.get_attr(parse_quote! { #[oxymp::RDParser] }) {
            return match item {
                syn::Item::Struct(mut item) => {
                    item.attrs.remove(attr_idx);
                    pipe!(item => AttributeTarget::RDParser => ItemParseResult::Ok)
                }
                item => ItemParseResult::Err(syn::Error::new(
                    item.get_ds_span(),
                    format!(
                        "'{}' cannot be marked with #[oxymp::RDParser]. Please use a struct.",
                        item.get_variant_name()
                    ),
                )),
            };
        }

        #[cfg(feature = "lr")]
        if let Some(attr_idx) = attrs.get_attr(parse_quote! { #[oxymp::LRParser] }) {
            return match item {
                syn::Item::Struct(mut item) => {
                    item.attrs.remove(attr_idx);
                    pipe!(item => AttributeTarget::LRParser => ItemParseResult::Ok)
                }
                item => ItemParseResult::Err(syn::Error::new(
                    item.get_ds_span(),
                    format!(
                        "'{}' cannot be marked with #[oxymp::LRParser]. Please use a struct.",
                        item.get_variant_name()
                    ),
                )),
            };
        }

        if let Some(attr) = attrs.attr_starts_with("oxymp") {
            return ItemParseResult::Err(syn::Error::new(
                attr.span(),
                "Attribute starting with 'oxymp' is not supported. Make sure you spelled it correctly, or have enabled the corresponding feature.",
            ));
        }

        ItemParseResult::Ignore(item)
    }
}
