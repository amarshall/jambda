pub mod lexer;
pub mod parser;

pub fn read(str: String) -> parser::Type {
  parser::parse_all(lexer::tokenize(str.as_str()))
}
