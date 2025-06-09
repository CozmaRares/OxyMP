// must have at least one variant
#[oxymp::oxymp]
mod l {
    #[oxymp::Tokens]
    enum Tok {}
}

// === REGEX ===

// missing matcher function
#[oxymp::oxymp]
mod l1 {
    #[oxymp::Tokens]
    enum Tok {
        #[regex(r"\d+(.\d+)?")]
        Number { value: i64 },

        #[exact("a")]
        A,
    }
}

// missing comma between regex and function
#[oxymp::oxymp]
mod l2 {
    #[oxymp::Tokens]
    enum Tok {
        #[regex(r"\d+(.\d+)?" match_number)]
        Number { value: i64 },

        #[exact("a")]
        A,
    }
}

// extra tokens
#[oxymp::oxymp]
mod l3 {
    #[oxymp::Tokens]
    enum Tok {
        #[regex(r"\d+(.\d+)?", match_number, aaa)]
        Number { value: i64 },

        #[exact("a")]
        A,
    }
}

// regex is not a string
#[oxymp::oxymp]
mod l4 {
    #[oxymp::Tokens]
    enum Tok {
        #[regex(123, match_number)]
        Number { value: i64 },

        #[exact("a")]
        A,
    }
}

// matcher function is not a path
#[oxymp::oxymp]
mod l5 {
    #[oxymp::Tokens]
    enum Tok {
        #[regex(r"\d+(.\d+)?", "match_number")]
        Number { value: i64 },
    }
}

// missing arguments
#[oxymp::oxymp]
mod l6 {
    #[oxymp::Tokens]
    enum Tok {
        #[regex]
        Number { value: i64 },
    }
}

// no arguments
#[oxymp::oxymp]
mod l7 {
    #[oxymp::Tokens]
    enum Tok {
        #[regex()]
        Number { value: i64 },
    }
}

// === EXACT ===

// missing arguments
#[oxymp::oxymp]
mod l8 {
    #[oxymp::Tokens]
    enum Tok {
        #[exact]
        Number,
    }
}

// no arguments
#[oxymp::oxymp]
mod l9 {
    #[oxymp::Tokens]
    enum Tok {
        #[exact()]
        Number,
    }
}

// pattern is not a string
#[oxymp::oxymp]
mod l10 {
    #[oxymp::Tokens]
    enum Tok {
        #[exact(123)]
        Number,
    }
}

// extra tokens
#[oxymp::oxymp]
mod l11 {
    #[oxymp::Tokens]
    enum Tok {
        #[exact("a" "b")]
        A,
    }
}

// === GENERAL ERRORS ===

// invalid attribute
#[oxymp::oxymp]
mod l12 {
    #[oxymp::Tokens]
    enum Tok {
        #[invalid]
        A,
    }
}

// no attribute
#[oxymp::oxymp]
mod l16 {
    #[oxymp::Tokens]
    enum Tok {
        A,
    }
}

// multiple attributes
#[oxymp::oxymp]
mod l13 {
    #[oxymp::Tokens]
    enum Tok {
        #[exact("a")]
        #[exact("b")]
        A,
    }
}

#[oxymp::oxymp]
mod l14 {
    #[oxymp::Tokens]
    enum Tok {
        #[exact("a")]
        #[regex("abc", abc)]
        A,
    }
}

#[oxymp::oxymp]
mod l15 {
    #[oxymp::Tokens]
    enum Tok {
        #[regex("abc", abc)]
        #[regex("def", def)]
        A,
    }
}

fn main() {}
