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

#[derive(Clone, Debug, PartialEq)]
pub struct SourcePosition {
  line: usize,
  char: usize,
}

impl SourcePosition {
  pub fn new(line: usize, char: usize) -> SourcePosition {
    SourcePosition{line, char}
  }
}

impl ToString for SourcePosition {
  fn to_string(&self) -> String {
    format!("{}:{}", self.line, self.char)
  }
}
