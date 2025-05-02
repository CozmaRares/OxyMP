pub fn capitalize<T: AsRef<str>>(string: T) -> String {
    let string = string.as_ref();
    let mut chars = string.chars();
    if let Some(first) = chars.next() {
        format!("{}{}", first.to_uppercase(), chars.as_str())
    } else {
        String::new()
    }
}

pub fn combine_errors<T: Into<syn::Error>, I: IntoIterator<Item = T>>(errors: I) -> syn::Error {
    let mut iter = errors.into_iter();
    let first = iter.next().expect("must have at least one error").into();
    iter.fold(first, |mut acc, err| {
        acc.combine(err.into());
        acc
    })
}
