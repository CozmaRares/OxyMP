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
                syn::Error::new(span, primary_message)
            } else {
                syn::Error::new(span, additional_message)
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

#[cfg(test)]
mod tests {
    mod char_helper {
        use super::super::{
            CharHelper, HIGH_SURROGATE_START, LOW_SURROGATE_END, MAX_CODEPOINT, MIN_CODEPOINT,
        };

        macro_rules! chr {
            ($codepoint:expr) => {
                char::from_u32($codepoint).unwrap()
            };
        }

        #[test]
        fn test_codepoints_exist() {
            assert!(char::from_u32(MIN_CODEPOINT).is_some());
            assert!(char::from_u32(LOW_SURROGATE_END).is_some());
            assert!(char::from_u32(HIGH_SURROGATE_START).is_some());
            assert!(char::from_u32(MAX_CODEPOINT).is_some());
        }

        #[test]
        fn test_next() {
            assert_eq!('a'.next_char(), Some('b'));
            assert_eq!('A'.next_char(), Some('B'));
            assert_eq!('0'.next_char(), Some('1'));
        }

        #[test]
        fn test_max_next() {
            assert_eq!(chr!(MAX_CODEPOINT).next_char(), None);
        }

        #[test]
        fn test_surrogate_next() {
            assert_eq!(
                chr!(LOW_SURROGATE_END).next_char(),
                Some(chr!(HIGH_SURROGATE_START))
            );
        }

        #[test]
        fn test_prev() {
            assert_eq!('b'.prev_char(), Some('a'));
            assert_eq!('B'.prev_char(), Some('A'));
            assert_eq!('1'.prev_char(), Some('0'));
        }

        #[test]
        fn test_min_prev() {
            assert_eq!(chr!(MIN_CODEPOINT).prev_char(), None);
        }

        #[test]
        fn test_surrogate_prev() {
            assert_eq!(
                chr!(HIGH_SURROGATE_START).prev_char(),
                Some(chr!(LOW_SURROGATE_END))
            );
        }

        #[test]
        fn test_is_followed_by() {
            assert_eq!('a'.is_followed_by(&'b'), true);
        }

        #[test]
        fn test_not_is_followed_by() {
            assert_eq!('a'.is_followed_by(&'c'), false);
        }

        #[test]
        fn test_max_is_followed_by() {
            assert_eq!(chr!(MAX_CODEPOINT).is_followed_by(&'a'), false);
        }

        #[test]
        fn test_is_followed_by_surrogate() {
            let c1 = chr!(LOW_SURROGATE_END);
            let c2 = chr!(HIGH_SURROGATE_START);
            assert_eq!(c1.is_followed_by(&c2), true);
        }

        #[test]
        fn test_is_followed_by_not_followed() {
            let c1 = chr!(1000);
            let c2 = chr!(2000);
            assert_eq!(c1.is_followed_by(&c2), false);
        }

        #[test]
        fn test_consecutive_chars() {
            for i in 0..100 {
                let c = chr!(i);
                let next_c = chr!(i + 1);
                assert!(c.is_followed_by(&next_c));
            }
        }

        #[test]
        fn test_prev_next_consistency() {
            for i in 1..101 {
                let c = chr!(i);
                if let Some(prev_c) = c.prev_char() {
                    assert_eq!(prev_c.next_char(), Some(c));
                }
            }
        }
    }
}
