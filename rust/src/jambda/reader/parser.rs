use regex;
use std;
use jambda::reader::lexer;
use jambda::reader::lexer::Token;
use jambda::reader::lexer::TokenPtr;

#[derive(Debug, PartialEq)]
pub enum Form {
  Float(f64),
  Identifier(std::string::String),
  Integer(isize),
  Nil,
  List(Vec<Form>),
  String(std::string::String),
}

#[derive(Clone, Copy)]
struct Reader<'a> {
  tokens: &'a Vec<TokenPtr>,
  cursor_pos: usize, // the position of next
}

impl<'a> Reader<'a> {
  fn from_tokens(tokens: &Vec<TokenPtr>) -> Reader {
    Reader{tokens, cursor_pos: 0}
  }

  fn parse_error(&self, message: String) -> String {
    format!("ParseError: [{}] {}", self.position().to_string(), message)
  }

  fn position(&self) -> lexer::Position {
    let cursor_pos = if self.cursor_pos < self.tokens.len() {
      self.cursor_pos
    } else {
      self.tokens.len() - 1
    };
    self.tokens[cursor_pos].position()
  }

  fn next(&mut self) -> Option<TokenPtr> {
    if self.cursor_pos < self.tokens.len() {
      self.cursor_pos += 1;
      let token = self.tokens[self.cursor_pos - 1].to_owned();
      Some(token)
    } else {
      None
    }
  }

  fn peek(&self) -> Option<TokenPtr> {
    if self.cursor_pos < self.tokens.len() {
      let token = self.tokens[self.cursor_pos].to_owned();
      Some(token)
    } else {
      None
    }
  }
}

pub fn parse_all(tokens: Vec<TokenPtr>) -> Result<Form, String> {
  let reader = &mut Reader::from_tokens(&tokens);
  let result = parse_form(reader);
  match result {
    Ok(_) => match reader.peek() {
      Some(token) => Err(reader.parse_error(format!("got {} but expected nothing", token.name()))),
      None => result,
    },
    Err(_) => result,
  }
}

fn eat_whitespace(reader: &mut Reader) {
  loop {
    match reader.peek().map(|t| t.token) {
      Some(Token::Newline) => { reader.next(); },
      Some(Token::Whitespace(_)) => { reader.next(); },
      _ => break,
    }
  }
}

fn eat_comment(reader: &mut Reader) {
  loop {
    match reader.peek().map(|t| t.token) {
      Some(Token::Newline) => break,
      None => break,
      _ => reader.next(),
    };
  }
}

fn parse_form(reader: &mut Reader) -> Result<Form, String> {
  let mut result;
  loop {
    eat_whitespace(reader);
    result = match reader.peek().map(|t| t.token) {
      Some(Token::DoubleQuote) => Some(parse_string(reader)),
      Some(Token::LParen) => Some(parse_list(reader)),
      Some(Token::Semicolon) => { eat_comment(reader); None },
      Some(Token::Word(_)) => Some(parse_atom(reader)),
      Some(token) => Some(Err(reader.parse_error(format!("got {} but expected one of DoubleQuote,LParen,SemiColon,Word", token.name())))),
      None => Some(Ok(Form::Nil)),
    };
    if result.is_some() { break };
  }
  eat_whitespace(reader);
  result.unwrap()
}

fn parse_atom(reader: &mut Reader) -> Result<Form, String> {
  let word = reader.peek().unwrap().to_string();
  if regex::Regex::new(r"^[+-]?(\d+|\d(\d|_)+\d)$").unwrap().is_match(word.as_str()) {
    reader.next();
    let val = word.replace("_", "").parse::<isize>().unwrap();
    Ok(Form::Integer(val))
  } else if regex::Regex::new(r"^[+-]?(\d+|\d(\d|_)+\d)\.(\d+|\d(\d|_)+\d)$").unwrap().is_match(word.as_str()) {
    reader.next();
    let val = word.replace("_", "").parse::<f64>().unwrap();
    Ok(Form::Float(val))
  } else if regex::Regex::new(r"^[^\d]").unwrap().is_match(word.as_str()) {
    reader.next();
    Ok(Form::Identifier(word))
  } else {
    let err = Err(reader.parse_error(format!("got invalid Word {}", word)));
    reader.next();
    err
  }
}

fn parse_list(reader: &mut Reader) -> Result<Form, String> {
  reader.next();
  let mut accumulator = vec![];
  loop {
    match reader.peek().map(|t| t.token) {
      Some(Token::RParen) => { reader.next(); break },
      Some(Token::Whitespace(_)) => { reader.next(); },
      Some(_) => match parse_form(reader) {
        Ok(form) => accumulator.push(form),
        err @ Err(_) => return err,
      },
      None => return Err(reader.parse_error(format!("got nothing but expected at least RParen (unclosed list)"))),
    };
  };

  Ok(Form::List(accumulator))
}

fn parse_string(reader: &mut Reader) -> Result<Form, String> {
  reader.next();
  let mut accumulator = "".to_string();
  loop {
    match reader.next().map(|t| t.token) {
      Some(Token::Backslash) => accumulator.push_str(parse_string_escape(reader).as_str()),
      Some(Token::DoubleQuote) => break,
      Some(token) => accumulator.push_str(token.to_string().as_str()),
      None => return Err(reader.parse_error(format!("got nothing but expected at least DoubleQuote (unclosed string literal)"))),
    };
  };

  Ok(Form::String(accumulator))
}

fn parse_string_escape(reader: &mut Reader) -> String {
  let newline_re = regex::Regex::new("n(.*)").unwrap();
  match reader.next().map(|t| t.token) {
    Some(Token::Backslash) => r"\".to_string(),
    Some(Token::DoubleQuote) => "\"".to_string(),
    Some(Token::Word(word)) => {
      if let Some(captures) = newline_re.captures(word.as_str()) {
        format!("\n{}", captures.get(1).map_or("", |m| m.as_str()))
      } else {
        format!("\\{}", word)
      }
    },
    Some(token) => format!("\\{}", token.to_string()),
    None => "\\".to_string(),
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use jambda::reader::lexer::Token;

  fn parse_all(tokens: Vec<Token>) -> Result<Form, String> {
    let token_ptrs = tokens.iter()
      .map(|token| TokenPtr::from_token(token.clone()))
      .collect::<Vec<TokenPtr>>();
    super::parse_all(token_ptrs)
  }

  #[test]
  fn test_parse_all_nothing() {
    let input = vec![];
    assert_eq!(parse_all(input).unwrap(), Form::Nil);
  }

  #[test]
  fn test_parse_all_whitespace() {
    let input = vec![Token::Whitespace(" ".to_string())];
    assert_eq!(parse_all(input).unwrap(), Form::Nil);
  }

  #[test]
  fn test_parse_all_comment() {
    let input = vec![
      Token::Semicolon,
      Token::Whitespace("  ".to_string()),
      Token::Word("foo".to_string()),
      Token::Whitespace("  ".to_string()),
      Token::RParen,
      Token::LParen,
      Token::Backslash,
      Token::DoubleQuote,
    ];
    assert_eq!(parse_all(input).unwrap(), Form::Nil);
  }

  #[test]
  fn test_parse_all_comment_newline() {
    let input = vec![
      Token::Semicolon,
      Token::Whitespace("  ".to_string()),
      Token::Word("foo".to_string()),
      Token::Whitespace("  ".to_string()),
      Token::RParen,
      Token::LParen,
      Token::Backslash,
      Token::DoubleQuote,
      Token::Newline,
      Token::Word("42".to_string()),
    ];
    assert_eq!(parse_all(input).unwrap(), Form::Integer(42));
  }

  #[test]
  fn test_parse_all_string_empty() {
    let input = vec![
      Token::DoubleQuote,
      Token::DoubleQuote,
    ];
    assert_eq!(parse_all(input).unwrap(), Form::String("".to_string()));
  }

  #[test]
  fn test_parse_all_string_basic() {
    let input = vec![
      Token::DoubleQuote,
      Token::Word("foo".to_string()),
      Token::DoubleQuote,
    ];
    assert_eq!(parse_all(input).unwrap(), Form::String("foo".to_string()));
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
    assert_eq!(parse_all(input).unwrap(), Form::String("foo  \t bar(\n".to_string()));
  }

  #[test]
  fn test_parse_all_string_escaped_backslash() {
    let input = vec![
      Token::DoubleQuote,
      Token::Backslash,
      Token::Backslash,
      Token::DoubleQuote,
    ];
    assert_eq!(parse_all(input).unwrap(), Form::String(r"\".to_string()));
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
    assert_eq!(parse_all(input).unwrap(), Form::String("\"foo".to_string()));
  }

  #[test]
  fn test_parse_all_string_escaped_newline() {
    let input = vec![
      Token::DoubleQuote,
      Token::Backslash,
      Token::Word("n".to_string()),
      Token::DoubleQuote,
    ];
    assert_eq!(parse_all(input).unwrap(), Form::String("\n".to_string()));
  }

  #[test]
  fn test_parse_all_string_with_semicolon() {
    let input = vec![
      Token::DoubleQuote,
      Token::Semicolon,
      Token::DoubleQuote,
      Token::Newline,
    ];
    assert_eq!(parse_all(input).unwrap(), Form::String(";".to_string()));
  }

  #[test]
  fn test_parse_all_string_unclosed() {
    let input = vec![
      Token::DoubleQuote,
    ];
    assert_eq!(
      parse_all(input),
      Err("ParseError: [0:0] got nothing but expected at least DoubleQuote (unclosed string literal)".to_string())
    );
  }

  #[test]
  fn test_parse_all_identifier_alpha() {
    let input = vec![Token::Word("foo".to_string())];
    assert_eq!(parse_all(input).unwrap(), Form::Identifier("foo".to_string()));
  }

  #[test]
  fn test_parse_all_identifier_alphanumeric() {
    let input = vec![Token::Word("a1b2".to_string())];
    assert_eq!(parse_all(input).unwrap(), Form::Identifier("a1b2".to_string()));
  }

  #[test]
  fn test_parse_all_identifier_unicode() {
    let input = vec![Token::Word("ƒøø".to_string())];
    assert_eq!(parse_all(input).unwrap(), Form::Identifier("ƒøø".to_string()));
  }

  #[test]
  fn test_parse_all_identifier_cannot_start_with_number() {
    let input = vec![Token::Word("1abc".to_string())];
    assert_eq!(parse_all(input), Err("ParseError: [0:0] got invalid Word 1abc".to_string()));
  }

  #[test]
  fn test_parse_all_integer() {
    let input = vec![Token::Word("42".to_string())];
    assert_eq!(parse_all(input).unwrap(), Form::Integer(42));
  }

  #[test]
  fn test_parse_all_integer_positive() {
    let input = vec![Token::Word("+42".to_string())];
    assert_eq!(parse_all(input).unwrap(), Form::Integer(42));
  }

  #[test]
  fn test_parse_all_integer_negative() {
    let input = vec![Token::Word("-42".to_string())];
    assert_eq!(parse_all(input).unwrap(), Form::Integer(-42));
  }

  #[test]
  fn test_parse_all_integer_underscore() {
    let input = vec![Token::Word("4_200".to_string())];
    assert_eq!(parse_all(input).unwrap(), Form::Integer(4200));
  }

  #[test]
  fn test_parse_all_integer_underscore_at_start_is_word() {
    let input = vec![Token::Word("_42".to_string())];
    assert_eq!(parse_all(input).unwrap(), Form::Identifier("_42".to_string()));
  }

  #[test]
  fn test_parse_all_integer_underscore_at_end_is_word() {
    let input = vec![Token::Word("42_".to_string())];
    assert_eq!(parse_all(input), Err("ParseError: [0:0] got invalid Word 42_".to_string()));
  }

  #[test]
  fn test_parse_all_float() {
    let input = vec![Token::Word("4.2".to_string())];
    assert_eq!(parse_all(input).unwrap(), Form::Float(4.2));
  }

  #[test]
  fn test_parse_all_float_positive() {
    let input = vec![Token::Word("+4.2".to_string())];
    assert_eq!(parse_all(input).unwrap(), Form::Float(4.2));
  }

  #[test]
  fn test_parse_all_float_negative() {
    let input = vec![Token::Word("-4.2".to_string())];
    assert_eq!(parse_all(input).unwrap(), Form::Float(-4.2));
  }

  #[test]
  fn test_parse_all_float_underscore() {
    let input = vec![Token::Word("4_200.002_4".to_string())];
    assert_eq!(parse_all(input).unwrap(), Form::Float(4200.0024));
  }

  #[test]
  fn test_parse_all_float_underscore_at_start_is_word() {
    let input = vec![Token::Word("_4.2".to_string())];
    assert_eq!(parse_all(input).unwrap(), Form::Identifier("_4.2".to_string()));
  }

  #[test]
  fn test_parse_all_float_underscore_at_left_end_is_word() {
    let input = vec![Token::Word("4_.2".to_string())];
    assert_eq!(parse_all(input), Err("ParseError: [0:0] got invalid Word 4_.2".to_string()));
  }

  #[test]
  fn test_parse_all_float_underscore_at_right_end_is_word() {
    let input = vec![Token::Word("4._2".to_string())];
    assert_eq!(parse_all(input), Err("ParseError: [0:0] got invalid Word 4._2".to_string()));
  }

  #[test]
  fn test_parse_all_float_underscore_at_end_is_word() {
    let input = vec![Token::Word("4.2_".to_string())];
    assert_eq!(parse_all(input), Err("ParseError: [0:0] got invalid Word 4.2_".to_string()));
  }

  #[test]
  fn test_parse_all_list_empty() {
    let input = vec![Token::LParen, Token::RParen];
    assert_eq!(parse_all(input).unwrap(), Form::List(vec![]));
  }

  #[test]
  fn test_parse_all_list_one_element() {
    let input = vec![
      Token::LParen,
      Token::Word("42".to_string()),
      Token::RParen,
    ];
    assert_eq!(parse_all(input).unwrap(), Form::List(vec![Form::Integer(42)]));
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
    assert_eq!(parse_all(input).unwrap(), Form::List(vec![
      Form::Integer(42),
      Form::String("42".to_string()),
    ]));
  }

  #[test]
  fn test_parse_all_list_many_elements_whitespace() {
    let input = vec![
      Token::LParen,
      Token::Word("42".to_string()),
      Token::Whitespace(" ".to_string()),
      Token::Word("42".to_string()),
      Token::RParen,
    ];
    assert_eq!(parse_all(input).unwrap(), Form::List(vec![
      Form::Integer(42),
      Form::Integer(42),
    ]));
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
    assert_eq!(parse_all(input).unwrap(),
      Form::List(vec![
        Form::List(vec![
          Form::List(vec![]),
        ]),
      ])
    );
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
    assert_eq!(parse_all(input).unwrap(),
      Form::List(vec![
        Form::Integer(1),
        Form::List(vec![
          Form::Integer(2),
          Form::List(vec![
            Form::Integer(3),
          ]),
          Form::Integer(4),
        ]),
        Form::Integer(5),
      ])
    );
  }

  #[test]
  fn test_parse_all_list_unclosed() {
    let input = vec![
      Token::LParen,
    ];
    assert_eq!(
      parse_all(input),
      Err("ParseError: [0:0] got nothing but expected at least RParen (unclosed list)".to_string())
    );
  }

  #[test]
  fn test_parse_all_leading_whitespace() {
    let input = vec![
      Token::Newline,
      Token::Newline,
      Token::Whitespace(" ".to_string()),
      Token::Whitespace(" ".to_string()),
      Token::Newline,
      Token::Newline,
      Token::Whitespace(" ".to_string()),
      Token::Whitespace(" ".to_string()),
      Token::Word("42".to_string()),
    ];
    assert_eq!(parse_all(input).unwrap(), Form::Integer(42));
  }

  #[test]
  fn test_parse_all_trailing_whitespace() {
    let input = vec![
      Token::Word("42".to_string()),
      Token::Newline,
      Token::Newline,
      Token::Whitespace(" ".to_string()),
      Token::Whitespace(" ".to_string()),
      Token::Newline,
      Token::Newline,
      Token::Whitespace(" ".to_string()),
      Token::Whitespace(" ".to_string()),
    ];
    assert_eq!(parse_all(input).unwrap(), Form::Integer(42));
  }

  #[test]
  fn test_parse_all_trailing_tokens_dont_supress_existing_err() {
    let input = vec![
      Token::Word("42a".to_string()),
      Token::Word("42".to_string()),
    ];
    assert_eq!(parse_all(input), Err("ParseError: [0:0] got invalid Word 42a".to_string()));
  }

  #[test]
  fn test_parse_all_trailing_tokens() {
    let input = vec![
      Token::Word("42".to_string()),
      Token::Whitespace(" ".to_string()),
      Token::Word("42".to_string()),
    ];
    assert_eq!(parse_all(input), Err("ParseError: [0:0] got Word but expected nothing".to_string()));
  }
}
