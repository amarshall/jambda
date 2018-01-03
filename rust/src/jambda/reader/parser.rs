use regex;
use std;
use jambda::reader::lexer::Token;

#[derive(Debug, PartialEq)]
pub enum Form {
  Identifier(std::string::String),
  Integer(isize),
  List(Vec<Form>),
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

pub fn parse_all(tokens: Vec<Token>) -> Result<Form, String> {
  let reader = &mut Reader{tokens: &tokens, position: 0};
  let result = parse_form(reader);
  match result {
    Ok(_) => match reader.peek() {
      Some(token) => Err(format!("ParseError: got {} but expected nothing", token.name())),
      None => result,
    },
    Err(_) => result,
  }
}

fn eat_whitespace(reader: &mut Reader) {
  loop {
    match reader.peek() {
      Some(Token::Newline) => { reader.next(); },
      Some(Token::Whitespace(_)) => { reader.next(); },
      _ => break,
    }
  }
}

fn parse_form(reader: &mut Reader) -> Result<Form, String> {
  eat_whitespace(reader);
  let result = match reader.peek().unwrap() {
    Token::DoubleQuote => parse_string(reader),
    Token::LParen => parse_list(reader),
    Token::Word(_) => parse_atom(reader),
    token => Err(format!("ParseError: got {} but expected one of DoubleQuote,LParen,Word", token.name())),
  };
  eat_whitespace(reader);
  result
}

fn parse_atom(reader: &mut Reader) -> Result<Form, String> {
  let word = reader.peek().unwrap().to_string();
  if regex::Regex::new(r"^[+-]?\d+$").unwrap().is_match(word.as_str()) {
    reader.next();
    let val = word.parse::<isize>().unwrap();
    Ok(Form::Integer(val))
  } else if regex::Regex::new(r"^[^\d]").unwrap().is_match(word.as_str()) {
    reader.next();
    Ok(Form::Identifier(word))
  } else {
    Err(format!("ParseError: got invalid Word {}", reader.next().unwrap().to_string()))
  }
}

fn parse_list(reader: &mut Reader) -> Result<Form, String> {
  reader.next();
  let mut accumulator = vec![];
  loop {
    match reader.peek() {
      Some(Token::RParen) => { reader.next(); break },
      Some(Token::Whitespace(_)) => { reader.next(); },
      Some(_) => match parse_form(reader) {
        Ok(form) => accumulator.push(form),
        err @ Err(_) => return err,
      },
      None => return Err("ParseError: got nothing but expected at least RParen (unclosed list)".to_string()),
    };
  };

  Ok(Form::List(accumulator))
}

fn parse_string(reader: &mut Reader) -> Result<Form, String> {
  reader.next();
  let mut accumulator = "".to_string();
  loop {
    match reader.next() {
      Some(Token::Backslash) => accumulator.push_str(parse_string_escape(reader).as_str()),
      Some(Token::DoubleQuote) => break,
      Some(token) => accumulator.push_str(token.to_string().as_str()),
      None => return Err("ParseError: got nothing but expected at least DoubleQuote (unclosed string literal)".to_string()),
    };
  };

  Ok(Form::String(accumulator))
}

fn parse_string_escape(reader: &mut Reader) -> String {
  let newline_re = regex::Regex::new("n(.*)").unwrap();
  match reader.next() {
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
  fn test_parse_all_string_escaped_non_escape() {
    let input = vec![
      Token::DoubleQuote,
      Token::Backslash,
      Token::Word("foo".to_string()),
      Token::DoubleQuote,
    ];
    assert_eq!(parse_all(input).unwrap(), Form::String("\\foo".to_string()));
  }

  #[test]
  fn test_parse_all_string_unclosed() {
    let input = vec![
      Token::DoubleQuote,
    ];
    assert_eq!(
      parse_all(input),
      Err("ParseError: got nothing but expected at least DoubleQuote (unclosed string literal)".to_string())
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
    assert_eq!(parse_all(input), Err("ParseError: got invalid Word 1abc".to_string()));
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
      Err("ParseError: got nothing but expected at least RParen (unclosed list)".to_string())
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
    assert_eq!(parse_all(input), Err("ParseError: got invalid Word 42a".to_string()));
  }

  #[test]
  fn test_parse_all_trailing_tokens() {
    let input = vec![
      Token::Word("42".to_string()),
      Token::Word("42".to_string()),
    ];
    assert_eq!(parse_all(input), Err("ParseError: got Word but expected nothing".to_string()));
  }
}
