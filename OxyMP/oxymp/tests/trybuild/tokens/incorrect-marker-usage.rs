// can't be a struct
#[oxymp::oxymp]
mod l2 {
    #[oxymp::Tokens]
    struct Tok;
}

// can't be a union
#[oxymp::oxymp]
mod l3 {
    #[oxymp::Tokens]
    union Tok {
        f1: u32,
        f2: f32,
    }
}

// no tokens
#[oxymp::oxymp]
mod l4 {}

// multiple tokens
#[oxymp::oxymp]
mod l5 {
    #[oxymp::Tokens]
    enum Tok1 {
        #[exact("a")]
        A,
    }

    #[oxymp::Tokens]
    enum Tok2 {
        #[exact("a")]
        A,
    }
}

fn main() {}
