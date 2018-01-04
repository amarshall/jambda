use jambda::types::{SourcePosition};
use regex;
use std;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
  Backslash,
  DoubleQuote,
  LParen,
  Newline,
  RParen,
  Semicolon,
  Whitespace(String),
  Word(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TokenPtr {
  pub token: Token,
  pos: SourcePosition,
}

impl Token {
  pub fn len(&self) -> usize {
    match *self {
      Token::Whitespace(ref val) => val.len(),
      Token::Word(ref val) => val.len(),
      _ => 1,
    }
  }

  pub fn name(&self) -> &str {
    match *self {
      Token::Backslash => "Backslash",
      Token::DoubleQuote => "DoubleQuote",
      Token::Semicolon => "Semicolon",
      Token::LParen => "LParen",
      Token::RParen => "RParen",
      Token::Newline => "Newline",
      Token::Whitespace(_) => "Whitespace",
      Token::Word(_) => "Word",
    }
  }
}

impl std::string::ToString for Token {
  fn to_string(&self) -> String {
    match *self {
      Token::Backslash => "\\".to_string(),
      Token::DoubleQuote => "\"".to_string(),
      Token::Semicolon => ";".to_string(),
      Token::LParen => "(".to_string(),
      Token::RParen => "(".to_string(),
      Token::Newline => "\n".to_string(),
      Token::Whitespace(ref val) => val.to_string(),
      Token::Word(ref val) => val.to_string(),
    }
  }
}

impl TokenPtr {
  pub fn from_token(token: Token) -> TokenPtr {
    TokenPtr{token: token, pos: SourcePosition::new(0, 0)}
  }

  pub fn position(&self) -> SourcePosition {
    self.pos.clone()
  }

  pub fn name(&self) -> &str {
    self.token.name()
  }
}

impl PartialEq<Token> for TokenPtr {
  fn eq(&self, other: &Token) -> bool {
    self.token == *other
  }
}

impl std::string::ToString for TokenPtr {
  fn to_string(&self) -> String {
    self.token.to_string()
  }
}

const REGEX: &str = r###"(?x)(
(?P<newline>\n)
|
(?P<whitespace>[\x20\t]+)
|
(?P<backslash>[\\])
|
(?P<semicolon>[;])
|
(?P<doublequote>["])
|
(?P<lparen>[(])
|
(?P<rparen>[)])
|
(?P<word>[^\s\\,;"()]+)
)"###;

fn capture_to_string(capture: Option<regex::Match>) -> String {
  capture.unwrap().as_str().to_string()
}

pub fn tokenize(str: &str) -> Vec<TokenPtr> {
  let mut line_no = 1;
  let mut char_no = 1;
  let mut tokens = vec![];
  let re = regex::Regex::new(REGEX).unwrap();
  for captures in re.captures_iter(&str) {
    let position = SourcePosition::new(line_no, char_no);
    let token = if captures.name("whitespace").is_some() {
      let capture = captures.name("whitespace");
      Token::Whitespace(capture_to_string(capture))
    } else if captures.name("newline").is_some() {
      Token::Newline
    } else if captures.name("backslash").is_some() {
      Token::Backslash
    } else if captures.name("doublequote").is_some() {
      Token::DoubleQuote
    } else if captures.name("semicolon").is_some() {
      Token::Semicolon
    } else if captures.name("lparen").is_some() {
      Token::LParen
    } else if captures.name("rparen").is_some() {
      Token::RParen
    } else if captures.name("word").is_some() {
      let capture = captures.name("word");
      Token::Word(capture_to_string(capture))
    } else {
      panic!("LexError: expected a capture but got none (bug)");
    };
    match token {
      Token::Newline => {
        char_no = 1;
        line_no += 1;
      },
      _ => char_no += token.len(),
    };
    let token_ptr = TokenPtr{token: token, pos: position};
    tokens.push(token_ptr);
  }
  tokens
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_nothing() {
    let input = "";
    assert_eq!(tokenize(input).is_empty(), true)
  }

  #[test]
  fn test_single_token() {
    let input = "abc1";
    assert_eq!(tokenize(input), [Token::Word("abc1".to_string())])
  }

  #[test]
  fn test_multiple_tokens() {
    let input = "abc1 123a";
    assert_eq!(tokenize(input), [
      Token::Word("abc1".to_string()),
      Token::Whitespace(" ".to_string()),
      Token::Word("123a".to_string()),
    ])
  }

  #[test]
  fn test_unicode_words() {
    let input = "áβc1";
    assert_eq!(tokenize(input), [Token::Word("áβc1".to_string())])
  }

  #[test]
  fn test_multiple_whitespace() {
    let input = "  abc1  123a  ";
    assert_eq!(tokenize(input), [
      Token::Whitespace("  ".to_string()),
      Token::Word("abc1".to_string()),
      Token::Whitespace("  ".to_string()),
      Token::Word("123a".to_string()),
      Token::Whitespace("  ".to_string()),
    ])
  }

  #[test]
  fn test_whitespace_and_newline() {
    let input = "  \n  \n\n  ";
    assert_eq!(tokenize(input), [
      Token::Whitespace("  ".to_string()),
      Token::Newline,
      Token::Whitespace("  ".to_string()),
      Token::Newline,
      Token::Newline,
      Token::Whitespace("  ".to_string()),
    ])
  }

  #[test]
  fn test_a_string() {
    let input = r#""foo bar""#;
    assert_eq!(tokenize(input), [
      Token::DoubleQuote,
      Token::Word("foo".to_string()),
      Token::Whitespace(" ".to_string()),
      Token::Word("bar".to_string()),
      Token::DoubleQuote,
    ])
  }

  #[test]
  fn test_escaped_double_quote() {
    let input = r#"foo\"bar"#;
    assert_eq!(tokenize(input), [
      Token::Word("foo".to_string()),
      Token::Backslash,
      Token::DoubleQuote,
      Token::Word("bar".to_string()),
    ])
  }

  #[test]
  fn test_escaped_backslash_then_double_quote() {
    let input = r#""foo\\\""#;
    assert_eq!(tokenize(input), [
      Token::DoubleQuote,
      Token::Word("foo".to_string()),
      Token::Backslash,
      Token::Backslash,
      Token::Backslash,
      Token::DoubleQuote,
    ])
  }

  #[test]
  fn test_empty_list() {
    let input = "()";
    assert_eq!(tokenize(input), [Token::LParen, Token::RParen])
  }

  #[test]
  fn test_single_list() {
    let input = "(1 2 3)";
    assert_eq!(tokenize(input), [
      Token::LParen,
      Token::Word("1".to_string()),
      Token::Whitespace(" ".to_string()),
      Token::Word("2".to_string()),
      Token::Whitespace(" ".to_string()),
      Token::Word("3".to_string()),
      Token::RParen,
    ])
  }

  #[test]
  fn test_nested_list() {
    let input = "(1 (2) 3)";
    assert_eq!(tokenize(input), [
      Token::LParen,
      Token::Word("1".to_string()),
      Token::Whitespace(" ".to_string()),
      Token::LParen,
      Token::Word("2".to_string()),
      Token::RParen,
      Token::Whitespace(" ".to_string()),
      Token::Word("3".to_string()),
      Token::RParen,
    ])
  }

  #[test]
  fn test_nested_list_empty() {
    let input = "((((()))))";
    assert_eq!(tokenize(input), [
      Token::LParen,
      Token::LParen,
      Token::LParen,
      Token::LParen,
      Token::LParen,
      Token::RParen,
      Token::RParen,
      Token::RParen,
      Token::RParen,
      Token::RParen,
    ])
  }

  #[test]
  fn test_nested_list_consecutive_parens() {
    let input = "(((1 2 3)))";
    assert_eq!(tokenize(input), [
      Token::LParen,
      Token::LParen,
      Token::LParen,
      Token::Word("1".to_string()),
      Token::Whitespace(" ".to_string()),
      Token::Word("2".to_string()),
      Token::Whitespace(" ".to_string()),
      Token::Word("3".to_string()),
      Token::RParen,
      Token::RParen,
      Token::RParen,
    ])
  }

  #[test]
  fn test_comments_no_prior_content() {
    let input = "; comment";
    assert_eq!(tokenize(input), [
      Token::Semicolon,
      Token::Whitespace(" ".to_string()),
      Token::Word("comment".to_string()),
    ])
  }

  #[test]
  fn test_comments_prior_content() {
    let input = "42; comment";
    assert_eq!(tokenize(input), [
      Token::Word("42".to_string()),
      Token::Semicolon,
      Token::Whitespace(" ".to_string()),
      Token::Word("comment".to_string()),
    ])
  }

  #[test]
  fn test_comments_content_after() {
    let input = "42 ; comment\n 43";
    assert_eq!(tokenize(input), [
      Token::Word("42".to_string()),
      Token::Whitespace(" ".to_string()),
      Token::Semicolon,
      Token::Whitespace(" ".to_string()),
      Token::Word("comment".to_string()),
      Token::Newline,
      Token::Whitespace(" ".to_string()),
      Token::Word("43".to_string()),
    ])
  }

  #[test]
  fn test_consecutive_symbol_chars() {
    let input = "=><*/";
    assert_eq!(tokenize(input), [Token::Word("=><*/".to_string())])
  }

  #[test]
  fn test_symbol_with_word_boundaries() {
    let input = "foo!-bar?*";
    assert_eq!(tokenize(input), [Token::Word("foo!-bar?*".to_string())])
  }
}
