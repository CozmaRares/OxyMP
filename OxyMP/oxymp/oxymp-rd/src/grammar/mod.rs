mod combinators;
mod nodes;

use std::collections::HashMap;

use nodes::GrammarNode;

use crate::data::MacroData;

pub fn parse_grammar(data: &MacroData) -> HashMap<String, GrammarNode> {
    let rules = data
        .grammar_rules
        .iter()
        .map(|rule| combinators::parse_rule(&rule).map(|r| r.1))
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    eprintln!("{:#?}", rules);

    HashMap::new()
}
