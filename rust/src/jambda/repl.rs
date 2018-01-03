extern crate rustyline;
use jambda::reader;

fn eval(ast: reader::parser::Type) -> reader::parser::Type {
  ast
}

fn print(exp: reader::parser::Type) -> String {
  format!("{:?}", exp)
}

pub fn run() {
  let mut editor = rustyline::Editor::<()>::new();
  loop {
    match editor.readline("∎ ") {
      Ok(line) => {
        editor.add_history_entry(&line);
        println!("{:?}", print(eval(reader::read(line))));
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
