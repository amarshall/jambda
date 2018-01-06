use jambda::types::{Env, Form, RForm};

pub fn eval(ast: Form, env: &Env) -> RForm {
  match ast {
    Form::Identifier(_) => eval_identifier(ast, env),
    Form::List(forms) => eval_ast(forms, env),
    Form::Nothing => Err(format!("Oops: tried to eval Nothing")),
    form => Ok(form),
  }
}

fn eval_ast(ast: Vec<Form>, env: &Env) -> RForm {
  let ast_evaled = ast.iter()
    .map(|form| eval(form.clone(), env).unwrap())
    .collect::<Vec<Form>>();
  let (func, args) = ast_evaled.split_first().unwrap();
  match func {
    &Form::Function(ref func) => func(args.to_vec()),
    _ => Err(format!("Oops: not supported form in identifier position")),
  }
}

fn eval_identifier(ast: Form, env: &Env) -> RForm {
  match ast {
    Form::Identifier(fn_name) => {
      env.get(&fn_name)
        .map(|func| func.to_owned())
        .ok_or(format!("NameError: “{}” is not defined", fn_name))
    },
    _ => Err(format!("Oops: tried to eval non-Identifier as Identifier")),
  }
}
