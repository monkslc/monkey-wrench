use std::{env, fs, path::Path};

use monkey_wrench::{
    eval::{Eval, Object},
    repl,
};

fn main() {
    match env::args().nth(1) {
        Some(filename) => match parse_file(filename) {
            Ok(Object::Error(err)) => println!("{}", err),
            Err(e) => println!("{}", e),
            Ok(_) => (),
        },
        None => match repl::start() {
            Ok(_) => println!("See you later! ;)"),
            Err(_) => println!("Oh no! It looks like an error occured!"),
        },
    }
}

fn parse_file<P: AsRef<Path>>(filename: P) -> Result<Object, String> {
    match fs::read_to_string(filename) {
        Ok(program) => Eval::run(&program),
        Err(e) => Err(format!("{}", e)),
    }
}
