mod m {
    use oxymp::Tokens;

    #[derive(Tokens)]
    pub enum Tok {
        #[exact("a")]
        A,
    }
}

fn main() {
    let _ = m::Tok::A;
}
