use regex::Regex;

pub fn at_beginning(re: &str) -> Regex {
    let re = format!("^{re}");
    Regex::new(&re).unwrap()
}
