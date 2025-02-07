#[derive(Debug)]
pub struct ExactToken {
    pub pattern: String,
}
pub const EXACT_TOKEN_FORMAT: &str = "#[exact(\"your exact string\")]";

impl syn::parse::Parse for ExactToken {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<ExactToken> {
        let pattern: syn::LitStr = input.parse()?;

        if !input.is_empty() {
            return Err(syn::Error::new(
                input.span(),
                "Unexpected remaining tokens after parsing the attribute. Please consider removing any trailing tokens"
            ));
        }

        Ok(ExactToken {
            pattern: pattern.value(),
        })
    }
}

#[derive(Debug)]
pub struct RegexToken {
    pub regex: String,
    pub transformer: syn::Path,
}
pub const REGEX_TOKEN_FORMAT: &str = "#[regex(\"your regex\", path::to::function)]";

impl syn::parse::Parse for RegexToken {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<RegexToken> {
        let regex: syn::LitStr = input.parse()?;
        let _comma: syn::Token![,] = input.parse()?;
        let transformer_path: syn::Path = input.parse()?;

        if !input.is_empty() {
            return Err(syn::Error::new(
                input.span(),
                "Unexpected remaining tokens after parsing the attribute. Please consider removing any trailing tokens"
            ));
        }

        Ok(RegexToken {
            regex: regex.value(),
            transformer: transformer_path,
        })
    }
}

#[derive(Debug)]
pub enum TokenInfo {
    Exact(ExactToken),
    Regex(RegexToken),
}
