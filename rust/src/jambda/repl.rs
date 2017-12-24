extern crate rustyline;

fn read(str: String) -> String {
  str
}

fn eval(ast: String) -> String {
  ast
}

fn print(exp: String) -> String {
  exp
}

pub fn run() {
  let mut editor = rustyline::Editor::<()>::new();
  loop {
    match editor.readline("∎ ") {
      Ok(line) => {
        println!("{:?}", print(eval(read(line))));
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
