use oxymp::Tokens;

// must have at least one variant
#[derive(Tokens)]
enum Tok {}

// can't be a struct
#[derive(Tokens)]
struct Tok2;

// can't be a union
#[derive(Tokens)]
union Tok3 {
    f1: u32,
    f2: f32,
}

fn main() {}
