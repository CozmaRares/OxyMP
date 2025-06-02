#![allow(unused_macros)]

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
