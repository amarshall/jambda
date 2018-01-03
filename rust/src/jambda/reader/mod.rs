pub mod lexer;
pub mod parser;

pub fn read(str: String) -> Result<parser::Form, String> {
  parser::parse_all(lexer::tokenize(str.as_str()))
}
