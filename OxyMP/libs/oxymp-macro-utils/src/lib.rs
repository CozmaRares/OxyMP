fn capitalize(s: String) -> String {
    let mut c = s.chars();
    let first = c.next().unwrap();
    let rest = c.as_str();
    format!("{}{}", first.to_uppercase(), rest)
}

pub struct AttributeParseError {
    span: proc_macro2::Span,
    message: String,
    correct_format: String,
}

impl AttributeParseError {
    pub fn new(
        span: proc_macro2::Span,
        message: String,
        correct_format: &str,
    ) -> AttributeParseError {
        AttributeParseError {
            span,
            message: capitalize(message),
            correct_format: correct_format.to_string(),
        }
    }
}

pub fn from_syn_result<T>(
    res: syn::Result<T>,
    correct_format: &str,
) -> Result<T, AttributeParseError> {
    let Err(err) = res else {
        return Ok(res.unwrap());
    };

    let span = err.span();
    let reason = err.to_string();

    Err(AttributeParseError {
        span,
        message: reason,
        correct_format: correct_format.to_string(),
    })
}

impl From<AttributeParseError> for syn::Error {
    fn from(err: AttributeParseError) -> syn::Error {
        let AttributeParseError {
            span,
            message: reason,
            correct_format,
        } = err;

        syn::Error::new(
            span,
            format!(
                "{}. Correct format is {}.",
                capitalize(reason),
                correct_format
            ),
        )
    }
}
