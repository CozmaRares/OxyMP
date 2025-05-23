pub fn capitalize<T: AsRef<str>>(string: T) -> String {
    let string = string.as_ref();
    let mut chars = string.chars();
    if let Some(first) = chars.next() {
        format!("{}{}", first.to_uppercase(), chars.as_str())
    } else {
        String::new()
    }
}

pub trait FoldErrors<T, E: Into<syn::Error>> {
    fn collect_errors(self) -> T;
}

impl<E: Into<syn::Error>, Iter: IntoIterator<Item = E>> FoldErrors<syn::Error, E> for Iter {
    fn collect_errors(self) -> syn::Error {
        let mut iter = self.into_iter();
        let first = iter.next().expect("must have at least one error").into();
        iter.fold(first, |mut acc, err| {
            acc.combine(err.into());
            acc
        })
    }
}

impl<T, E: Into<syn::Error>, Iter: IntoIterator<Item = Result<T, E>>>
    FoldErrors<syn::Result<Vec<T>>, E> for Iter
{
    fn collect_errors(self) -> syn::Result<Vec<T>> {
        let (oks, errs): (Vec<T>, Vec<syn::Error>) =
            self.into_iter()
                .fold((Vec::new(), Vec::new()), |(mut oks, mut errs), res| {
                    match res {
                        Ok(ok) => oks.push(ok),
                        Err(err) => errs.push(err.into()),
                    }
                    (oks, errs)
                });

        if !errs.is_empty() {
            Err(errs.collect_errors())
        } else {
            Ok(oks)
        }
    }
}

struct SynErrorWrapper(proc_macro2::Span, &'static str);
impl Into<syn::Error> for SynErrorWrapper {
    fn into(self) -> syn::Error {
        syn::Error::new(self.0, self.1)
    }
}

pub fn multiple_attribute_error(
    spans: Vec<proc_macro2::Span>,
    primary_message: &'static str,
    additional_message: &'static str,
) -> syn::Error {
    spans
        .into_iter()
        .enumerate()
        .map(|(idx, span)| {
            if idx == 0 {
                SynErrorWrapper(span, primary_message)
            } else {
                SynErrorWrapper(span, additional_message)
            }
        })
        .collect_errors()
}

pub trait CharHelper {
    fn next_char(&self) -> Option<char>;
    fn prev_char(&self) -> Option<char>;

    fn is_followed_by(&self, other: &Self) -> bool;
    fn is_preceded_by(&self, other: &Self) -> bool {
        other.is_followed_by(self)
    }
}

const MIN_CODEPOINT: u32 = 0;
const LOW_SURROGATE_END: u32 = 0xD7FF;
const HIGH_SURROGATE_START: u32 = 0xE000;
const MAX_CODEPOINT: u32 = 0x10FFFF;

// TEST: test this
impl CharHelper for char {
    fn next_char(&self) -> Option<char> {
        let mut code = *self as u32;
        if code == MAX_CODEPOINT {
            return None;
        }
        code += 1;

        if code > LOW_SURROGATE_END && code < HIGH_SURROGATE_START {
            code = HIGH_SURROGATE_START;
        }
        char::from_u32(code)
    }

    fn prev_char(&self) -> Option<char> {
        let mut code = *self as u32;
        if code == MIN_CODEPOINT {
            return None;
        }
        code -= 1;

        if code > LOW_SURROGATE_END && code < HIGH_SURROGATE_START {
            code = LOW_SURROGATE_END;
        }
        char::from_u32(code)
    }

    fn is_followed_by(&self, other: &Self) -> bool {
        self.next_char()
            .map(|next_char| next_char == *other)
            .unwrap_or(false)
    }
}
