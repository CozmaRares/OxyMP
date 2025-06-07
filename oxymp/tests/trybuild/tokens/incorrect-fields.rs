// exact variant's fields must be unit
#[oxymp::oxymp]
mod l1 {
    #[oxymp::Tokens]
    enum Tok {
        #[exact("while")]
        While(i64),
    }
}

// regex variant's fields must not be unit
#[oxymp::oxymp]
mod l2 {
    use oxymp_util::lexer::LexResult;
    fn match_number(_: &str) -> LexResult<Tok> {
        Ok(Tok::Number)
    }

    #[oxymp::Tokens]
    enum Tok {
        #[regex(r"\d+(.\d+)?", match_number)]
        Number,
    }
}

// regex variant with empty tuple
#[oxymp::oxymp]
mod l3 {
    #[oxymp::Tokens]
    enum Tok {
        #[regex(r"\d+(.\d+)?", match_number)]
        Number(),
    }
}

// regex variant with empty struct
#[oxymp::oxymp]
mod l4 {
    #[oxymp::Tokens]
    enum Tok {
        #[regex(r"\d+(.\d+)?", match_number)]
        Number {},
    }
}

fn main() {}
