mod tokens;

use crate::data::MacroData;

pub fn generate(data: MacroData) -> Vec<syn::Item> {
    let mut items = Vec::new();
    items.extend(tokens::generate_structs(&data.tokens));

    items
}
