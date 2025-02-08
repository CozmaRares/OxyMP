mod m {
    use oxymp::Tokens;

    #[derive(Tokens)]
    enum Tok {
        #[exact("a")]
        A,
    }
}

fn main() {
    let _ = m::Tok::A;
}
