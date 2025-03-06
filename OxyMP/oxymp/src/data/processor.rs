use quote::format_ident;
use syn::{parse_quote, spanned::Spanned};

use crate::utils::{
    capitalize, find_attr, get_item_attrs, get_item_ds_span, get_item_variant,
    has_attr_starting_with, pretty_print_attr_path,
};

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
    let Some(attrs) = get_item_attrs(item) else {
        return ItemProcessResult::Ignore;
    };
    if attrs.is_empty() {
        return ItemProcessResult::Ignore;
    }

    let target_attribute = get_processor_attribute::<TProcessor, TData, TItem>();

    let Some(attr_idx) = find_attr(attrs, target_attribute) else {
        return ItemProcessResult::Ignore;
    };

    let Some(variant) = TProcessor::get_variant(&item) else {
        return ItemProcessResult::Err(syn::Error::new(
            get_item_ds_span(item),
            format!(
                "{} cannot be marked with #[oxymp::{}]. Please use {}.",
                capitalize(get_item_variant(item)),
                TProcessor::get_target(),
                TProcessor::get_expected_variant()
            ),
        ));
    };

    let (data, modified_item) = match TProcessor::extract_data(variant, attr_idx) {
        Ok(ok) => ok,
        Err(err) => return ItemProcessResult::Err(err),
    };

    let empry_vec = Vec::new();
    let attrs = get_item_attrs(&modified_item).unwrap_or(&empry_vec);
    let Some(attr) = has_attr_starting_with(attrs, "oxymp") else {
        return ItemProcessResult::Ok(data, modified_item);
    };

    ItemProcessResult::Err(syn::Error::new(
        attr.span(),
        format!(
            "Item is already assumed to be marked with #[oxymp::{}], but later found #[{}]. Please use only one attribute.",
            TProcessor::get_target(),
            pretty_print_attr_path(attr.path())
        )
    ))
}
