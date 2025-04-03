use std::any::Any;

use syn::spanned::Spanned;

pub fn capitalize<T: AsRef<str>>(string: T) -> String {
    let string = string.as_ref();
    let mut chars = string.chars();
    if let Some(first) = chars.next() {
        format!("{}{}", first.to_uppercase(), chars.as_str())
    } else {
        String::new()
    }
}

pub fn pretty_print_attr_path(path: &syn::Path) -> String {
    let mut res = String::new();
    path.segments.iter().for_each(|segment| {
        res.push_str(&segment.ident.to_string());
        res.push_str("::");
    });
    res.pop();
    res.pop();
    res
}

pub fn get_item_variant(item: &syn::Item) -> &'static str {
    match item {
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

pub fn get_item_attrs(item: &syn::Item) -> Option<&Vec<syn::Attribute>> {
    match item {
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

pub fn find_attr(attrs: &[syn::Attribute], attribute: syn::Attribute) -> Option<usize> {
    attrs.iter().position(|attr| attr == &attribute)
}

pub fn has_attr_starting_with<'a>(
    attrs: &'a [syn::Attribute],
    path_segment: &str,
) -> Option<&'a syn::Attribute> {
    attrs.iter().find(|attr| {
        attr.path()
            .segments
            .first()
            .map(|segment| segment.ident == path_segment)
            .unwrap_or(false)
    })
}

pub fn split_iter<I, A, B>(iter: I) -> (Vec<A>, Vec<B>)
where
    I: Iterator<Item = (A, B)>,
{
    let mut a = Vec::new();
    let mut b = Vec::new();
    for (ia, ib) in iter {
        a.push(ia);
        b.push(ib);
    }
    (a, b)
}

pub fn extract_panic_message(e: &Box<dyn Any + Send + 'static>) -> Option<String> {
    if let Some(s) = e.downcast_ref::<String>() {
        Some(s.clone())
    } else if let Some(s) = e.downcast_ref::<&str>() {
        Some(s.to_string())
    } else {
        None
    }
}

macro_rules! pq {
    () => { ::syn::parse_quote!() };
    ($($tt:tt)*) => {
        {
            let result: Result<_, _> = ::std::panic::catch_unwind(|| {
                ::syn::parse_quote!($($tt)*)
            });

            match result {
                Ok(val) => val,
                Err(e) => {
                    eprintln!("syn::parse_quote! on {}:{} paniced",
                        file!(),
                        line!(),
                    );
                    let msg = crate::utils::extract_panic_message(&e).unwrap_or_else(|| "Panic occurred, but message could not be retrieved.".to_string());
                    panic!("{}", msg);
                }
            }
        }
    };
}

macro_rules! q {
    () => { ::quote::quote!() };
    ($($tt:tt)*) => { ::quote::quote!($($tt)*) };
    ($($tt:tt)*) => {
        {
            let result: Result<_, _> = ::std::panic::catch_unwind(|| {
                ::quote::quote!($($tt)*)
            });

            match result {
                Ok(val) => val,
                Err(_) => {
                    eprintln!("qoute::quote! on {}:{} paniced",
                        file!(),
                        line!(),
                    );
                    let msg = crate::utils::extract_panic_message(&e).unwrap_or_else(|| "Panic occurred, but message could not be retrieved.".to_string());
                    panic!("{}", msg);
                }
            }
        }
    };

    ($($tt:tt)*) => {
        ::quote::quote!($($tt)*)
    };
}
