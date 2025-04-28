// TODO: rethink the place for some of these functions

// pub fn capitalize<T: AsRef<str>>(string: T) -> String {
//     let string = string.as_ref();
//     let mut chars = string.chars();
//     if let Some(first) = chars.next() {
//         format!("{}{}", first.to_uppercase(), chars.as_str())
//     } else {
//         String::new()
//     }
// }
//
// pub fn get_item_variant(item: &syn::Item) -> &'static str {
//     match item {
//         syn::Item::Enum(_) => "an enum",
//         syn::Item::ExternCrate(_) => "an extern crate",
//         syn::Item::Fn(_) => "a function",
//         syn::Item::ForeignMod(_) => "a foreign module",
//         syn::Item::Impl(_) => "an impl block",
//         syn::Item::Macro(_) => "a macro",
//         syn::Item::Mod(_) => "a module",
//         syn::Item::Static(_) => "a static variable",
//         syn::Item::Struct(_) => "a struct",
//         syn::Item::Trait(_) => "a trait",
//         syn::Item::TraitAlias(_) => "a trait alias",
//         syn::Item::Type(_) => "a type",
//         syn::Item::Union(_) => "a union",
//         syn::Item::Use(_) => "an import",
//         _ => "an unknown item",
//     }
// }
//
// pub fn get_item_ds_span(item: &syn::Item) -> proc_macro2::Span {
//     match item {
//         syn::Item::Enum(item) => item.enum_token.span,
//         syn::Item::ExternCrate(item) => item.extern_token.span,
//         syn::Item::Fn(item) => item.sig.fn_token.span,
//         syn::Item::ForeignMod(item) => item.abi.extern_token.span,
//         syn::Item::Impl(item) => item.impl_token.span,
//         syn::Item::Macro(item) => item.mac.path.span(),
//         syn::Item::Mod(item) => item.mod_token.span,
//         syn::Item::Static(item) => item.static_token.span,
//         syn::Item::Struct(item) => item.struct_token.span,
//         syn::Item::Trait(item) => item.trait_token.span,
//         syn::Item::TraitAlias(item) => item.trait_token.span,
//         syn::Item::Type(item) => item.type_token.span,
//         syn::Item::Union(item) => item.union_token.span,
//         syn::Item::Use(item) => item.use_token.span,
//         item => item.span(),
//     }
// }
