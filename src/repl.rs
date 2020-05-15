use std::io::{self, BufRead, Write};

use crate::{lexer::Lexer, parser::Parser};

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
                let parser = Parser::new(lex);
                let (statements, errors): (Vec<_>, Vec<_>) =
                    parser.statements().partition(Result::is_ok);
                let statements = statements.into_iter().map(|x| x.unwrap());
                let errors = errors.into_iter().map(|x| x.unwrap_err());
                println!("{:?}", statements);
                println!("{:?}", errors);
            }
        }
        input.clear();
    }
    Ok(())
}
