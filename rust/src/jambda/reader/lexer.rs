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

const REGEX: &str = r###"(?x)(
(?P<newline>\n)
|
(?P<whitespace>\s+)
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

pub fn tokenize(str: &str) -> Vec<Token> {
  let mut tokens = vec![];
  let re = regex::Regex::new(REGEX).unwrap();
  for captures in re.captures_iter(&str) {
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
      panic!("Oops: lexer unexpectedly captured without known capture");
    };
    tokens.push(token);
  }
  tokens
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_nothing() {
    let input = "";
    assert_eq!(tokenize(input), [])
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
