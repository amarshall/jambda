use jambda::types::Form;

pub fn print(ast: Form) -> Result<String, String> {
  match ast {
    Form::Nothing => Ok("Nothing()".to_string()),
    _ => Ok(format!("{:?}", ast)),
  }
}
