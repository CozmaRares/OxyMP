pub fn capitalize<S>(string: S) -> String
where
    S: AsRef<str>,
{
    let string = string.as_ref();
    let mut chars = string.chars();
    if let Some(first) = chars.next() {
        format!("{}{}", first.to_uppercase(), chars.as_str())
    } else {
        String::new()
    }
}
