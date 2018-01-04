extern crate rustyline;
use jambda::core;
use jambda::evaluator;
use jambda::printer;
use jambda::reader;

pub fn rep(input: String) -> Result<String, String> {
  reader::read(input)
    .and_then(|o| evaluator::eval(o, &core::make()))
    .and_then(|o| printer::print(o))
}

pub fn repp(input: String) {
  match rep(input) {
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
        repp(line);
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
