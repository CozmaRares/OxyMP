#[oxymp::oxymp]
mod l1 {
    #[oxymp::Tokens]
    enum Tok {
        #[exact("a")]
        A,
    }
}

mod l2 {
    #[oxymp::oxymp]
    pub mod l {
        #[oxymp::Tokens]
        pub(super) enum Tok {
            #[exact("a")]
            A,
        }
    }
}

mod l3 {
    #[oxymp::oxymp]
    pub mod l {
        #[oxymp::Tokens]
        pub(in crate::l3) enum Tok {
            #[exact("a")]
            A,
        }
    }
}

fn main() {
    let _ = l1::tok::A;
    let _ = l2::l::tok::A;
    let _ = l3::l::tok::A;
}
