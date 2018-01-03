use regex;
use std;
use jambda::reader::lexer::Token;

#[derive(Debug, PartialEq)]
pub enum Type {
  Integer(isize),
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
      Token::Word(_) => parse_atom(reader),
      _ => panic!(format!("Oops: parser unimplimented token ({:?})", token)),
    };
    nodes.push(node);
  };

  nodes
}

fn parse_atom(reader: &mut Reader) -> Type {
  if regex::Regex::new(r"^[+-]?\d+$").unwrap().is_match(reader.peek().unwrap().to_string().as_str()) {
    let val = reader.next().unwrap().to_string().parse::<isize>().unwrap();
    Type::Integer(val)
  } else {
    panic!(format!("Oops: parser unexpected word ({:?})", reader.next().unwrap()))
  }
}

fn parse_string(reader: &mut Reader) -> Type {
  reader.next();
  let mut accumulator = "".to_string();
  while let Some(token) = reader.next() {
    match token {
      Token::Backslash => accumulator.push_str(parse_string_escape(reader).as_str()),
      Token::DoubleQuote => break,
      _ => accumulator.push_str(token.to_string().as_str()),
    };
  };

  Type::String(accumulator)
}

fn parse_string_escape(reader: &mut Reader) -> String {
  match reader.next() {
    Some(Token::Backslash) => r"\".to_string(),
    Some(Token::DoubleQuote) => "\"".to_string(),
    Some(token) => format!("\\{}", token.to_string()),
    None => "\\".to_string(),
  }
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

  #[test]
  fn test_parse_all_string_escaped_backslash() {
    let input = vec![
      Token::DoubleQuote,
      Token::Backslash,
      Token::Backslash,
    ];
    assert_eq!(parse_all(input), [Type::String(r"\".to_string())]);
  }

  #[test]
  fn test_parse_all_string_escaped_double_quote() {
    let input = vec![
      Token::DoubleQuote,
      Token::Backslash,
      Token::DoubleQuote,
      Token::Word("foo".to_string()),
      Token::DoubleQuote,
    ];
    assert_eq!(parse_all(input), [Type::String("\"foo".to_string())]);
  }

  #[test]
  fn test_parse_all_string_escaped_non_escape() {
    let input = vec![
      Token::DoubleQuote,
      Token::Backslash,
      Token::Word("foo".to_string()),
      Token::DoubleQuote,
    ];
    assert_eq!(parse_all(input), [Type::String("\\foo".to_string())]);
  }

  #[test]
  fn test_parse_all_integer() {
    let input = vec![Token::Word("42".to_string())];
    assert_eq!(parse_all(input), [Type::Integer(42)]);
  }

  #[test]
  fn test_parse_all_integer_positive() {
    let input = vec![Token::Word("+42".to_string())];
    assert_eq!(parse_all(input), [Type::Integer(42)]);
  }

  #[test]
  fn test_parse_all_integer_negative() {
    let input = vec![Token::Word("-42".to_string())];
    assert_eq!(parse_all(input), [Type::Integer(-42)]);
  }
}
