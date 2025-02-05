#[derive(Debug)]
pub struct ExactToken {
    pub pattern: String,
}

impl syn::parse::Parse for ExactToken {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<ExactToken> {
        let pattern: syn::LitStr = input.parse()?;

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

const TRANSFORMER_NAME: &str = "transform";

impl syn::parse::Parse for RegexToken {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<RegexToken> {
        let regex: syn::LitStr = input.parse()?;
        let _comma: syn::Token![,] = input.parse()?;

        let transform_ident: syn::Ident = input.parse()?;
        if transform_ident != TRANSFORMER_NAME {
            return Err(syn::Error::new(
                transform_ident.span(),
                format!("Expected `{TRANSFORMER_NAME}` as the transformer identifier",),
            ));
        }
        let _eq: syn::Token![=] = input.parse()?;
        let transform_path: syn::Path = input.parse()?;

        Ok(RegexToken {
            regex: regex.value(),
            transformer: transform_path,
        })
    }
}

#[derive(Debug)]
pub enum TokenInfo {
    Exact(ExactToken),
    Regex(RegexToken),
}
