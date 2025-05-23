use std::any::Any;

pub fn extract_panic_message(e: &Box<dyn Any + Send + 'static>) -> Option<String> {
    e.downcast_ref::<String>().map(|s| s.to_string())
}

macro_rules! q {
    () => { ::quote::quote!() };
    ($($tt:tt)*) => { ::quote::quote!($($tt)*) };
}

macro_rules! pq {
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
                    let msg = crate::macros::extract_panic_message(&e).unwrap_or_else(|| "Panic occurred, but message could not be retrieved.".to_string());
                    std::panic!("{}", msg);
                }
            }
        }
    };
}

macro_rules! items {
    ($($tt:tt)*) => {
        {
            let input = q!($($tt)*);
            let item_mod: syn::ItemMod = pq! {
                mod a {
                    #input
                }
            };
            item_mod.content.unwrap().1
        }
    };
}

macro_rules! assert {
    ($cond:expr, $($args:tt)*) => {
        if !$cond {
            eprintln!("Assertion failed at {}:{}", file!(), line!());
            std::panic!($($args)*);
        }
    };
}

#[allow(unused_macros)]
#[cfg(debug_assertions)]
macro_rules! time {
    ($str:expr, $($tt:tt)*) => {
        {
            let start = std::time::Instant::now();
            let r = {
                $($tt)*
            };
            let end = std::time::Instant::now();
            let duration = end.duration_since(start);
            println!("{} took {} milliseconds", $str, duration.as_millis());
            r
        }

    };
}
