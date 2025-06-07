enum All {
    CoreUsize,
    CoreStr,
    CoreChar,

    StdOption,
    StdSome,
    StdNone,
    StdResult,
    StdOk,
    StdErr,
    StdRc,
    StdVec,
    StdBox,
    StdFormatter,
    StdFmtResult,
    StdString,

    MacroVec,
    MacroWrite,
    MacroWriteln,
    MacroFormat,
    MacroUnreachable,

    TraitDisplay,
    TraitError,
    TraitFrom,

    DeriveDebug,
    DeriveClone,
}

impl All {
    fn path(self) -> proc_macro2::TokenStream {
        match self {
            All::CoreUsize => q! { ::core::primitive::usize },
            All::CoreStr => q! { ::core::primitive::str },
            All::CoreChar => q! { ::core::primitive::char },

            All::StdOption => q! { ::std::option::Option },
            All::StdSome => q! { ::std::option::Option::Some },
            All::StdNone => q! { ::std::option::Option::None },
            All::StdResult => q! { ::std::result::Result },
            All::StdOk => q! { ::std::result::Result::Ok },
            All::StdErr => q! { ::std::result::Result::Err },
            All::StdRc => q! { ::std::rc::Rc },
            All::StdVec => q! { ::std::vec::Vec },
            All::StdBox => q! { ::std::boxed::Box },
            All::StdFormatter => q! { ::std::fmt::Formatter },
            All::StdFmtResult => q! { ::std::fmt::Result },
            All::StdString => q! { ::std::string::String },

            All::MacroVec => q! { ::std::vec },
            All::MacroWrite => q! { ::std::write },
            All::MacroWriteln => q! { ::std::writeln },
            All::MacroFormat => q! { ::std::format },
            All::MacroUnreachable => q! { ::std::unreachable },

            All::TraitDisplay => q! { ::std::fmt::Display },
            All::TraitError => q! { ::std::error::Error },
            All::TraitFrom => q! { ::std::convert::From },

            All::DeriveDebug => q! { ::std::fmt::Debug },
            All::DeriveClone => q! { ::std::clone::Clone },
        }
    }
}

pub enum Core {
    Usize,
    Str,
    Char,
}

pub enum Std {
    Option,
    Some,
    None,
    Result,
    Ok,
    Err,
    Rc,
    Vec,
    Box,
    Formatter,
    FmtResult,
    String,
}

pub enum Macro {
    Vec,
    Write,
    Writeln,
    Format,
    Unreachable,
}

pub enum Trait {
    Display,
    Error,
    From,
}

pub enum Derive {
    Debug,
    Clone,
}

impl From<Core> for All {
    fn from(val: Core) -> Self {
        match val {
            Core::Usize => All::CoreUsize,
            Core::Str => All::CoreStr,
            Core::Char => All::CoreChar,
        }
    }
}

impl From<Std> for All {
    fn from(val: Std) -> Self {
        match val {
            Std::Option => All::StdOption,
            Std::Some => All::StdSome,
            Std::None => All::StdNone,
            Std::Result => All::StdResult,
            Std::Ok => All::StdOk,
            Std::Err => All::StdErr,
            Std::Rc => All::StdRc,
            Std::Vec => All::StdVec,
            Std::Box => All::StdBox,
            Std::Formatter => All::StdFormatter,
            Std::FmtResult => All::StdFmtResult,
            Std::String => All::StdString,
        }
    }
}

impl From<Macro> for All {
    fn from(val: Macro) -> Self {
        match val {
            Macro::Vec => All::MacroVec,
            Macro::Write => All::MacroWrite,
            Macro::Writeln => All::MacroWriteln,
            Macro::Format => All::MacroFormat,
            Macro::Unreachable => All::MacroUnreachable,
        }
    }
}

impl From<Trait> for All {
    fn from(val: Trait) -> Self {
        match val {
            Trait::Display => All::TraitDisplay,
            Trait::Error => All::TraitError,
            Trait::From => All::TraitFrom,
        }
    }
}

impl From<Derive> for All {
    fn from(val: Derive) -> Self {
        match val {
            Derive::Debug => All::DeriveDebug,
            Derive::Clone => All::DeriveClone,
        }
    }
}

macro_rules! impl_path {
    ($name:ident) => {
        impl $name {
            pub fn path(self) -> proc_macro2::TokenStream {
                All::from(self).path()
            }
        }
    };
}

impl_path!(Core);
impl_path!(Std);
impl_path!(Macro);
impl_path!(Trait);
impl_path!(Derive);
