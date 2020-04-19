use monkey_wrench::repl;
fn main() {
    match repl::start() {
        Ok(_) => println!("See you later! ;)"),
        Err(_) => println!("Oh no! It looks like an error occured!"),
    }
}
