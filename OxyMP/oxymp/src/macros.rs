macro_rules! pq {
    () => { ::syn::parse_quote!() };
    ($($tt:tt)*) => {
        {
            let result: Result<_, _> = ::std::panic::catch_unwind(|| {
                ::syn::parse_quote!($($tt)*)
            });

            match result {
                Ok(val) => val,
                Err(e) => {
                    eprintln!("syn::parse_quote! on {}:{} paniced",
                        file!(),
                        line!(),
                    );
                    let msg = crate::utils::extract_panic_message(&e).unwrap_or_else(|| "Panic occurred, but message could not be retrieved.".to_string());
                    panic!("{}", msg);
                }
            }
        }
    };
}

macro_rules! q {
    () => { ::quote::quote!() };
    ($($tt:tt)*) => { ::quote::quote!($($tt)*) };
    ($($tt:tt)*) => {
        {
            let result: Result<_, _> = ::std::panic::catch_unwind(|| {
                ::quote::quote!($($tt)*)
            });

            match result {
                Ok(val) => val,
                Err(_) => {
                    eprintln!("qoute::quote! at {}:{} paniced",
                        file!(),
                        line!(),
                    );
                    let msg = crate::utils::extract_panic_message(&e).unwrap_or_else(|| "Panic occurred, but message could not be retrieved.".to_string());
                    panic!("{}", msg);
                }
            }
        }
    };

    ($($tt:tt)*) => {
        ::quote::quote!($($tt)*)
    };
}

macro_rules! assert {
    ($cond:expr, $($args:tt)*) => {
        if !$cond {
            eprintln!("Assertion failed at {}:{}", file!(), line!());
            panic!($($args)*);
        }
    };
}
