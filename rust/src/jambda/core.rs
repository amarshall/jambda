use jambda::types::{Env, Form, RForm};
use jambda::printer::print;

fn add(forms: Vec<Form>) -> RForm {
  let rsum = forms.iter().map(|form| {
    match *form {
      Form::Integer(val) => Ok(val),
      _ => Err(format!("TypeError: expected Integer got {:?}", form)),
    }
  }).fold(Ok(0), |rs, rx| rs.and_then(|s| rx.map(|x| s + x)));
  rsum.map(|sum| Form::Integer(sum))
}

fn println(forms: Vec<Form>) -> RForm {
  for form in forms {
    println!("{}", print(form).unwrap());
  }
  Ok(Form::Nothing)
}

pub fn make() -> Env {
  let mut env = Env::new();
  env.insert("+".to_string(), Form::Function(add));
  env.insert("println".to_string(), Form::Function(println));
  env
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_add() {
    assert_eq!(
      add(vec![Form::Integer(1), Form::Integer(3)]),
      Ok(Form::Integer(4))
    );
  }
}
