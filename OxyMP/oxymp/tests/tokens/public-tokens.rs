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

#[oxymp::oxymp]
mod l3 {
    #[oxymp::Tokens]
    pub mod l {
        #[oxymp::Tokens]
        pub(crate) enum Tok {
            #[exact("a")]
            A,
        }
    }
}

fn main() {
    let _ = l1::Tok::A;
    let _ = l2::Tok::A;
    let _ = l3::l::Tok::A;
}
