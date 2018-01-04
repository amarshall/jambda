use jambda::types::{Env, Form, RForm};

pub fn eval(ast: Form, env: &Env) -> RForm {
  match ast {
    Form::List(forms) => eval_ast(forms, env),
    _ => Err(format!("Oops: not supported eval type")),
  }
}

fn eval_ast(ast: Vec<Form>, env: &Env) -> RForm {
  match ast.split_first().unwrap() {
    (&Form::Identifier(ref fn_name), args) => {
      match env.get(fn_name) {
        Some(&Form::Function(func)) => func(args.to_vec()),
        Some(_) => Err(format!("Oops: not supported form in identifier position")),
        None => Err(format!("NameError: “{}” is not defined", fn_name)),
      }
    },
    _ => Err(format!("Oops: not supported form in identifier position")),
  }
}
