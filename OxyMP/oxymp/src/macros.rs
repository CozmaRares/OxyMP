macro_rules! pipe {
    ($val:expr => $func:expr) => {
        $func($val)
    };
    ($val:expr => $func:expr => $($rest:expr)=>+) => {
        pipe!($func($val) => $($rest)=>+)
    };
}
