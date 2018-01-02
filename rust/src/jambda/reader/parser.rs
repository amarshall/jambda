use std;
use jambda::reader::lexer::Token;

#[derive(Debug, PartialEq)]
pub enum Type {
  String(std::string::String),
}

#[derive(Clone, Copy)]
struct Reader<'a> {
  tokens: &'a Vec<Token>,
  position: usize, // the position of next
}

impl<'a> Reader<'a> {
  fn next(&mut self) -> Option<Token> {
    if self.position < self.tokens.len() {
      self.position += 1;
      Some(self.tokens[self.position - 1].to_owned())
    } else {
      None
    }
  }

  fn peek(&self) -> Option<Token> {
    if self.position < self.tokens.len() {
      Some(self.tokens[self.position].to_owned())
    } else {
      None
    }
  }
}

pub fn parse_all(tokens: Vec<Token>) -> Vec<Type> {
  let mut nodes = vec![];
  let reader = &mut Reader{tokens: &tokens, position: 0};

  while let Some(token) = reader.peek() {
    let node = match token {
      Token::DoubleQuote => parse_string(reader),
      _ => panic!(format!("Oops: parser unimplimented token ({:?})", token)),
    };
    nodes.push(node);
  };

  nodes
}

fn parse_string(reader: &mut Reader) -> Type {
  reader.next();
  let mut accumulator = "".to_string();
  while let Some(token) = reader.next() {
    match token {
      Token::DoubleQuote => break,
      _ => accumulator.push_str(token.to_string().as_str()),
    };
  };

  Type::String(accumulator)
}

#[cfg(test)]
mod tests {
  use super::*;
  use jambda::reader::lexer::Token;

  #[test]
  fn test_parse_all_string_empty() {
    let input = vec![
      Token::DoubleQuote,
      Token::DoubleQuote,
    ];
    assert_eq!(parse_all(input), [Type::String("".to_string())]);
  }

  #[test]
  fn test_parse_all_string_basic() {
    let input = vec![
      Token::DoubleQuote,
      Token::Word("foo".to_string()),
      Token::DoubleQuote,
    ];
    assert_eq!(parse_all(input), [Type::String("foo".to_string())]);
  }

  #[test]
  fn test_parse_all_string_complex() {
    let input = vec![
      Token::DoubleQuote,
      Token::Word("foo".to_string()),
      Token::Whitespace("  \t ".to_string()),
      Token::Word("bar".to_string()),
      Token::RParen,
      Token::Newline,
      Token::DoubleQuote,
    ];
    assert_eq!(parse_all(input), [Type::String("foo  \t bar(\n".to_string())]);
  }
}
