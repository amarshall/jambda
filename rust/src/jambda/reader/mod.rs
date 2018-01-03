pub mod lexer;
pub mod parser;

pub fn read(str: String) -> Result<parser::Form, String> {
  parser::parse_all(lexer::tokenize(str.as_str()))
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  #[ignore]
  fn test_read_depth() {
    let depth = 1_900;
    let mut input = "(".repeat(depth);
    input.push_str(")".repeat(depth).as_str());
    assert!(read(input).is_ok());
  }

  #[test]
  fn test_parse_error_line_number() {
    let input = "(foo\n(bar \n42a))".to_string();
    assert_eq!(read(input), Err("ParseError: [2] got invalid Word 42a".to_string()));
  }
}
