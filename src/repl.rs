use crate::lexer::Lexer;
use std::io::{self, BufRead, Write};
pub fn start() -> Result<(), io::Error> {
    let mut stdout = io::stdout();
    let stdin = io::stdin();
    let mut stdin = stdin.lock();

    let mut input = String::new();
    loop {
        print!(">> ");
        stdout.flush()?;
        stdin.read_line(&mut input)?;
        match &input[..] {
            "exit\n" => break,
            _ => {
                let lex = Lexer::new(&input[..]);
                for token in lex {
                    println!("{:?}", token);
                }
            }
        }
        input.clear();
    }
    Ok(())
}
