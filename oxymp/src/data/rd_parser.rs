use super::{grammar::GrammarData, helpers::process_module_helper};

#[derive(Debug)]
pub struct RDParserData {
    pub grammar_rules: Vec<GrammarData>,
}

const ATTRIBUTES: &[&str] = &["grammar"];

pub(super) fn process_rd_parser(item: syn::Item) -> syn::Result<(syn::ItemMod, RDParserData)> {
    let (item, mut found_attrs) = process_module_helper(item, ATTRIBUTES)?;

    let Some(grammar_attrs) = found_attrs.remove("grammar") else {
        return Err(syn::Error::new(
            item.ident.span(),
            "Missing grammar attributes. A parser must have at least one grammar attribute.",
        ));
    };

    let grammar_rules = grammar_attrs
        .into_iter()
        .map(|(span, rule)| GrammarData { span, rule })
        .collect();

    Ok((item, RDParserData { grammar_rules }))
}
