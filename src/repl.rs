use std::io::{self, BufRead, Write};

use crate::{eval::Eval, lexer::Lexer, parser::Parser};

pub fn start() -> Result<(), io::Error> {
    let mut stdout = io::stdout();
    let stdin = io::stdin();
    let mut stdin = stdin.lock();

    let mut input = String::new();
    let mut e = Eval::new();
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
                let errors: Vec<String> = errors.into_iter().map(|x| x.unwrap_err()).collect();
                if errors.len() > 0 {
                    println!("Uh oh there were errors!\n{:?}", errors);
                    continue;
                }

                let result = e.eval(statements);
                println!("{:?}", result);
            }
        }
        input.clear();
    }
    Ok(())
}
