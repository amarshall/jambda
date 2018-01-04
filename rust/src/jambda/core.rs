use jambda::types::{Env, Form, RForm};

fn add(forms: Vec<Form>) -> RForm {
  let rsum = forms.iter().map(|form| {
    match *form {
      Form::Integer(val) => Ok(val),
      _ => Err(format!("TypeError: expected Integer got {:?}", form)),
    }
  }).fold(Ok(0), |rs, rx| rs.and_then(|s| rx.map(|x| s + x)));
  rsum.map(|sum| Form::Integer(sum))
}

pub fn make() -> Env {
  let mut env = Env::new();
  env.insert("+".to_string(), Form::Function(add));
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
