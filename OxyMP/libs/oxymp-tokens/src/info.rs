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

impl syn::parse::Parse for RegexToken {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<RegexToken> {
        let regex: syn::LitStr = input.parse()?;
        let _comma: syn::Token![,] = input.parse()?;

        let transform_ident: syn::Ident = input.parse()?;
        if transform_ident != "transform" {
            return Err(syn::Error::new(
                transform_ident.span(),
                "Expected `transform` as the transformer identifier",
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

impl syn::parse::Parse for TokenInfo {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<TokenInfo> {
        let _hash: syn::Token![#] = input.parse()?;

        let content_bracketed;
        let _bracketed = syn::bracketed!(content_bracketed in input);

        let ident: syn::Ident = content_bracketed.parse()?;

        let branch = match ident.to_string().as_str() {
            "exact" => 1,
            "regex" => 2,
            _ => return Err(syn::Error::new(ident.span(), "Unknown token type")),
        };

        let content_parenthesized;
        let _parenthesized = syn::parenthesized!(content_parenthesized in content_bracketed);

        let info = match branch {
            1 => content_parenthesized.parse().map(TokenInfo::Exact),
            2 => content_parenthesized.parse().map(TokenInfo::Regex),
            _ => unreachable!(),
        }?;

        // optional last comma
        let lookahead = content_parenthesized.lookahead1();
        if lookahead.peek(syn::Token![,]) {
            content_parenthesized.parse::<syn::Token![,]>()?;
        }

        if !content_parenthesized.is_empty() {
            return Err(syn::Error::new(
                content_parenthesized.span(),
                "Unexpected remaining tokens",
            ));
        }

        Ok(info)
    }
}
