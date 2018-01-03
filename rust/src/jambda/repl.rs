extern crate rustyline;
use jambda::reader;

fn eval(ast: Result<reader::parser::Form, String>) -> Result<reader::parser::Form, String> {
  ast
}

fn print(exp: Result<reader::parser::Form, String>) -> Result<String, String> {
  exp.map(|form| format!("{:?}", form))
}

pub fn rep(input: String) {
  match print(eval(reader::read(input))) {
    Ok(string) => println!("{}", string),
    Err(string) => eprintln!("{}", string),
  };
}

pub fn run() {
  let mut editor = rustyline::Editor::<()>::new();
  loop {
    match editor.readline("∎ ") {
      Ok(line) => {
        editor.add_history_entry(&line);
        rep(line);
      }
      Err(rustyline::error::ReadlineError::Eof) => {
        break
      }
      Err(err) => {
        eprintln!("Error: {:?}", err);
      }
    }
  }
}
