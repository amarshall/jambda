use jambda::reader::parser::Form;

pub fn print(form: Result<Form, String>) -> Result<String, String> {
  form.map(|form2| match form2 {
    Form::Nothing => "".to_string(),
    _ => format!("{:?}", form2),
  })
}
