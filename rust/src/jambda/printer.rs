use jambda::reader::parser::Form;

pub fn print(form: Result<Form, String>) -> Result<String, String> {
  form.map(|f| format!("{:?}", f))
}
