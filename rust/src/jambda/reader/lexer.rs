use nom;
use nom::{alpha, alphanumeric, digit, rest_s};
use std;
use std::str::FromStr;

#[derive(Debug, PartialEq)]
pub enum Token {
  Comment(String),
  Identifier(String),
  Integer(isize),
  String(std::string::String),
}

named!(
  parse_all<&str, Vec<Token>>,
  ws!(many0!(alt!(
    map!(comment, Token::Comment) |
    map!(integer_literal, Token::Integer) |
    map!(identifier, Token::Identifier) |
    map!(string_literal, Token::String)
    )))
);

named!(comment<&str, String>,
  map!(
    recognize!(
      tuple!(
        tag_s!(";"),
        alt!(
          take_until_and_consume_s!("\n") |
          rest_s
        )
      )
    ),
    String::from
  )
);

named!(integer_literal<&str, isize>, map_res!(digit, isize::from_str));

named!(identifier<&str, String>,
  map!(
    recognize!(
      preceded!(alpha, alphanumeric)
    ),
    String::from
  )
);

named!(string_literal<&str, String>,
  map!(
    delimited!(
      tag_s!("\""),
      take_until_s!("\""),
      tag_s!("\"")
    ),
    String::from
  )
);

pub fn parse(str: &str) -> Vec<Token> {
  match parse_all(str) {
    nom::IResult::Done(rest, parsed) => {
      if rest.is_empty() {
        parsed
      } else {
        panic!("Parse error, remaining tokens: {:?}", rest)
      }
    }
    nom::IResult::Error(err) => {
      panic!("Parse error: {:?}", err)
    }
    nom::IResult::Incomplete(needed) => {
      panic!("Parse error, incomplete, needed: {:?}", needed)
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use nom::IResult::Done;

  #[test]
  fn test_integer_literal() {
    assert_eq!(integer_literal("42"), Done("", 42))
  }

  #[test]
  fn test_identifier() {
    let input = "abc1";
    assert_eq!(parse(input), [Token::Identifier("abc1".to_string())])
  }

  #[test]
  fn test_identifier_unicode() {
    let input = "áḃç1";
    assert_eq!(parse(input), [Token::Identifier("áḃç1".to_string())])
  }

  #[test]
  fn test_multiple() {
    let input = "abc1 123";
    assert_eq!(parse(input), [Token::Identifier("abc1".to_string()), Token::Integer(123)])
  }

  #[test]
  fn test_whitespace() {
    let input = "  abc1  123  ";
    assert_eq!(parse(input), [Token::Identifier("abc1".to_string()), Token::Integer(123)])
  }

  #[test]
  fn test_a_string() {
    let input = r#""foo bar""#;
    assert_eq!(parse(input), [Token::String("foo bar".to_string())])
  }

  #[test]
  fn test_comments_no_prior_content() {
    let input = "; comment";
    assert_eq!(parse(input), [Token::Comment("; comment".to_string())])
  }

  #[test]
  fn test_comments_prior_content() {
    let input = "42; comment";
    assert_eq!(parse(input), [Token::Integer(42), Token::Comment("; comment".to_string())])
  }

  #[test]
  fn test_comments_content_after() {
    let input = "42 ; comment\n 43";
    assert_eq!(parse(input), [Token::Integer(42), Token::Comment("; comment\n".to_string()), Token::Integer(43)])
  }
}
