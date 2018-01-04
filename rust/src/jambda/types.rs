use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum Form {
  Boolean(bool),
  Float(f64),
  Identifier(String),
  Integer(isize),
  Nothing,
  List(Vec<Form>),
  String(String),
}
