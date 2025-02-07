mod new;

use new::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = "1+2";
    let lexer = Lexer::new();
    let tokens = lexer.tokenize(input)?;

    println!("{:?}", tokens);

    Ok(())
}
