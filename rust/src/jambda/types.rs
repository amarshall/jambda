use std::collections::HashMap;

pub type Env = HashMap<String, Form>;
pub type RForm = Result<Form, String>;

#[derive(Clone, Debug, PartialEq)]
pub enum Form {
  Boolean(bool),
  Float(f64),
  Function(fn(Vec<Form>) -> RForm),
  Identifier(String),
  Integer(isize),
  Nothing,
  List(Vec<Form>),
  String(String),
}
