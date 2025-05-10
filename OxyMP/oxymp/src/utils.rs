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
