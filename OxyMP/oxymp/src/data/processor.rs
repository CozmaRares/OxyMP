use quote::{format_ident, ToTokens};
use syn::{parse_quote, spanned::Spanned};

use crate::utils::capitalize;

pub trait ItemProcessor<TData, TItem> {
    fn get_target() -> &'static str;
    fn get_expected_variant() -> &'static str;
    fn get_variant(item: &syn::Item) -> Option<TItem>;
    fn extract_data(item: TItem, marker_attr_idx: usize) -> syn::Result<(TData, syn::Item)>;
}

pub enum ItemProcessResult<TData, TItem> {
    Ok(TData, TItem),
    Err(syn::Error),
    Ignore,
}

pub fn get_processor_attribute<TProcessor, TData, TItem>() -> syn::Attribute
where
    TProcessor: ItemProcessor<TData, TItem>,
{
    let segment = TProcessor::get_target();
    let segment = format_ident!("{}", segment);
    parse_quote! { #[oxymp::#segment] }
}

pub fn process_item<TProcessor, TData, TItem>(
    _processor: TProcessor,
    item: &syn::Item,
) -> ItemProcessResult<TData, syn::Item>
where
    TProcessor: ItemProcessor<TData, TItem>,
{
    let Some(attrs) = item.get_attributes() else {
        return ItemProcessResult::Ignore;
    };
    if attrs.is_empty() {
        return ItemProcessResult::Ignore;
    }

    let target_attribute = get_processor_attribute::<TProcessor, TData, TItem>();

    eprintln!("{:#?}", attrs.iter().nth(1));
    eprintln!("{:#?}", target_attribute);

    let res = if let Some(attr_idx) = attrs.get_attr(target_attribute) {
        let Some(variant) = TProcessor::get_variant(&item) else {
            return ItemProcessResult::Err(syn::Error::new(
                item.get_ds_span(),
                format!(
                    "{} cannot be marked with #[oxymp::{}]. Please use {}.",
                    capitalize(item.get_variant_name()),
                    TProcessor::get_target(),
                    TProcessor::get_expected_variant()
                ),
            ));
        };

        match TProcessor::extract_data(variant, attr_idx) {
            Ok(ok) => Some(ok),
            Err(err) => return ItemProcessResult::Err(err),
        }
    } else {
        None
    };

    let new_attrs = match &res {
        Some((_, item)) => item.get_attributes().unwrap(),
        None => attrs,
    };

    if let Some(attr) = new_attrs.attr_starts_with("oxymp") {
        let msg = if res.is_none() {
            format!(
            "Attribute {}, containing 'oxymp', is not supported. Make sure you spelled it correctly, or have enabled the corresponding feature.",
                attr.to_token_stream().to_string()
            )
        } else {
            format!(
                "Item is first marked with #[oxymp::{}], but later with {}. Please use only one attribute.",
                TProcessor::get_target(),
                attr.to_token_stream().to_string()
            )
        };

        return ItemProcessResult::Err(syn::Error::new(attr.span(), msg));
    }

    match res {
        Some((data, item)) => ItemProcessResult::Ok(data, item),
        None => ItemProcessResult::Ignore,
    }
}

pub trait HasAttribute {
    fn get_attr(&self, attribute: syn::Attribute) -> Option<usize>;
    fn attr_starts_with(&self, path_segment: &str) -> Option<&syn::Attribute>;
}

impl HasAttribute for Vec<syn::Attribute> {
    fn get_attr(&self, attribute: syn::Attribute) -> Option<usize> {
        self.iter().position(|attr| attr == &attribute)
    }

    fn attr_starts_with(&self, path_segment: &str) -> Option<&syn::Attribute> {
        self.iter().find(|attr| {
            attr.path()
                .segments
                .first()
                .map(|segment| segment.ident.to_string() == path_segment)
                .unwrap_or(false)
        })
    }
}

pub trait ItemHelper {
    fn get_attributes(&self) -> Option<&Vec<syn::Attribute>>;
    fn get_ds_span(&self) -> proc_macro2::Span;
    fn get_variant_name(&self) -> &'static str;
}

// implement the trait for all variants that could have attributes
impl ItemHelper for syn::Item {
    fn get_variant_name(&self) -> &'static str {
        match self {
            syn::Item::Enum(_) => "an enum",
            syn::Item::ExternCrate(_) => "an extern crate",
            syn::Item::Fn(_) => "a function",
            syn::Item::ForeignMod(_) => "a foreign module",
            syn::Item::Impl(_) => "an impl block",
            syn::Item::Macro(_) => "a macro",
            syn::Item::Mod(_) => "a module",
            syn::Item::Static(_) => "a static variable",
            syn::Item::Struct(_) => "a struct",
            syn::Item::Trait(_) => "a trait",
            syn::Item::TraitAlias(_) => "a trait alias",
            syn::Item::Type(_) => "a type",
            syn::Item::Union(_) => "a union",
            syn::Item::Use(_) => "an import",
            _ => "an unknown item",
        }
    }

    fn get_ds_span(&self) -> proc_macro2::Span {
        match self {
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

    fn get_attributes(&self) -> Option<&Vec<syn::Attribute>> {
        match self {
            syn::Item::Enum(item) => Some(&item.attrs),
            syn::Item::ExternCrate(item) => Some(&item.attrs),
            syn::Item::Fn(item) => Some(&item.attrs),
            syn::Item::ForeignMod(item) => Some(&item.attrs),
            syn::Item::Impl(item) => Some(&item.attrs),
            syn::Item::Macro(item) => Some(&item.attrs),
            syn::Item::Mod(item) => Some(&item.attrs),
            syn::Item::Static(item) => Some(&item.attrs),
            syn::Item::Struct(item) => Some(&item.attrs),
            syn::Item::Trait(item) => Some(&item.attrs),
            syn::Item::TraitAlias(item) => Some(&item.attrs),
            syn::Item::Type(item) => Some(&item.attrs),
            syn::Item::Union(item) => Some(&item.attrs),
            syn::Item::Use(item) => Some(&item.attrs),
            _ => None,
        }
    }
}
