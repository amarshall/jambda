extern crate rustyline;
use jambda::evaluator;
use jambda::printer;
use jambda::reader;

pub fn rep(input: String) {
  let result = reader::read(input)
    .and_then(|o| evaluator::eval(o))
    .and_then(|o| printer::print(o));

  match result {
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
