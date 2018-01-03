use regex;
use std;
use jambda::reader::lexer::Token;

#[derive(Debug, PartialEq)]
pub enum Type {
  Identifier(std::string::String),
  Integer(isize),
  List(Vec<Type>),
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

  while let Some(_) = reader.peek() {
    let node = parse_form(reader);
    nodes.push(node);
  };

  nodes
}

fn parse_form(reader: &mut Reader) -> Type {
  match reader.peek().unwrap() {
    Token::DoubleQuote => parse_string(reader),
    Token::LParen => parse_list(reader),
    Token::Word(_) => parse_atom(reader),
    token => panic!(format!("Oops: parser unimplimented token ({:?})", token)),
  }
}

fn parse_atom(reader: &mut Reader) -> Type {
  let word = reader.peek().unwrap().to_string();
  if regex::Regex::new(r"^[+-]?\d+$").unwrap().is_match(word.as_str()) {
    let val = reader.next().unwrap().to_string().parse::<isize>().unwrap();
    Type::Integer(val)
  } else if regex::Regex::new(r"^[^\d]").unwrap().is_match(word.as_str()) {
    reader.next();
    Type::Identifier(word)
  } else {
    panic!(format!("Oops: parser unexpected word ({:?})", reader.next().unwrap()))
  }
}

fn parse_list(reader: &mut Reader) -> Type {
  reader.next();
  let mut accumulator = vec![];
  while let Some(token) = reader.peek() {
    match token {
      Token::RParen => { reader.next(); break },
      _ => accumulator.push(parse_form(reader)),
    };
  }
  Type::List(accumulator)
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
  fn test_parse_all_identifier_alpha() {
    let input = vec![Token::Word("foo".to_string())];
    assert_eq!(parse_all(input), [Type::Identifier("foo".to_string())]);
  }

  #[test]
  fn test_parse_all_identifier_alphanumeric() {
    let input = vec![Token::Word("a1b2".to_string())];
    assert_eq!(parse_all(input), [Type::Identifier("a1b2".to_string())]);
  }

  #[test]
  fn test_parse_all_identifier_unicode() {
    let input = vec![Token::Word("ƒøø".to_string())];
    assert_eq!(parse_all(input), [Type::Identifier("ƒøø".to_string())]);
  }

  #[test]
  #[should_panic]
  fn test_parse_all_identifier_cannot_start_with_number() {
    let input = vec![Token::Word("1abc".to_string())];
    parse_all(input);
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

  #[test]
  fn test_parse_all_list_empty() {
    let input = vec![Token::LParen, Token::RParen];
    assert_eq!(parse_all(input), [Type::List(vec![])]);
  }

  #[test]
  fn test_parse_all_list_one_element() {
    let input = vec![
      Token::LParen,
      Token::Word("42".to_string()),
      Token::RParen,
    ];
    assert_eq!(parse_all(input), [Type::List(vec![Type::Integer(42)])]);
  }

  #[test]
  fn test_parse_all_list_many_elements() {
    let input = vec![
      Token::LParen,
      Token::Word("42".to_string()),
      Token::DoubleQuote,
      Token::Word("42".to_string()),
      Token::DoubleQuote,
      Token::RParen,
    ];
    assert_eq!(parse_all(input), [Type::List(vec![
      Type::Integer(42),
      Type::String("42".to_string()),
    ])]);
  }

  #[test]
  fn test_parse_all_list_nested() {
    let input = vec![
      Token::LParen,
      Token::LParen,
      Token::LParen,
      Token::RParen,
      Token::RParen,
      Token::RParen,
    ];
    assert_eq!(parse_all(input), [
      Type::List(vec![
        Type::List(vec![
          Type::List(vec![]),
        ]),
      ]),
    ]);
  }

  #[test]
  fn test_parse_all_list_nested_complex() {
    let input = vec![
      Token::LParen,
      Token::Word("1".to_string()),
      Token::LParen,
      Token::Word("2".to_string()),
      Token::LParen,
      Token::Word("3".to_string()),
      Token::RParen,
      Token::Word("4".to_string()),
      Token::RParen,
      Token::Word("5".to_string()),
      Token::RParen,
    ];
    assert_eq!(parse_all(input), [
      Type::List(vec![
        Type::Integer(1),
        Type::List(vec![
          Type::Integer(2),
          Type::List(vec![
            Type::Integer(3),
          ]),
          Type::Integer(4),
        ]),
        Type::Integer(5),
      ]),
    ]);
  }
}
