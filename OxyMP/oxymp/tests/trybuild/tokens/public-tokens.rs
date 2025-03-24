#[oxymp::oxymp]
mod l1 {
    #[oxymp::Tokens]
    pub enum Tok {
        #[exact("a")]
        A,
    }
}

#[oxymp::oxymp]
mod l2 {
    #[oxymp::Tokens]
    pub(super) enum Tok {
        #[exact("a")]
        A,
    }
}

mod l3 {
    #[oxymp::oxymp]
    pub mod l {
        #[oxymp::Tokens]
        pub(crate) enum Tok {
            #[exact("a")]
            A,
        }
    }
}

fn main() {
    let _ = l1::TokA;
    let _ = l2::TokA;
    let _ = l3::l::TokA;
}
