use regex::Regex;

const REGEX: &str = r###"(?x)(
\s+ # Whitespace
|
[\\,;"()] # Separator
|
[^\s\\,;"()]+ # Word
)"###;

pub fn tokenize(str: &str) -> Vec<String> {
  let mut tokens = vec![];
  let re = Regex::new(REGEX).unwrap();
  for captures in re.captures_iter(&str) {
    tokens.push((&captures[0]).to_string());
  }
  tokens
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_single_token() {
    let input = "abc1";
    assert_eq!(tokenize(input), ["abc1"])
  }

  #[test]
  fn test_multiple_tokens() {
    let input = "abc1 123a";
    assert_eq!(tokenize(input), ["abc1", " ", "123a"])
  }

  #[test]
  fn test_unicode_words() {
    let input = "áβc1";
    assert_eq!(tokenize(input), ["áβc1"])
  }

  #[test]
  fn test_multiple_whitespace() {
    let input = "  abc1  123a  ";
    assert_eq!(tokenize(input), ["  ", "abc1", "  ", "123a", "  "])
  }

  #[test]
  fn test_a_string() {
    let input = r#""foo bar""#;
    assert_eq!(tokenize(input), ["\"", "foo", " ", "bar", "\""])
  }

  #[test]
  fn test_escaped_double_quote() {
    let input = r#"foo\"bar"#;
    assert_eq!(tokenize(input), ["foo", r"\", r#"""#, "bar"])
  }

  #[test]
  fn test_escaped_backslash_then_double_quote() {
    let input = r#""foo\\\""#;
    assert_eq!(tokenize(input), [r#"""#, "foo", r"\", r"\", r"\", r#"""#])
  }

  #[test]
  fn test_empty_list() {
    let input = "()";
    assert_eq!(tokenize(input), ["(", ")"])
  }

  #[test]
  fn test_single_list() {
    let input = "(1 2 3)";
    assert_eq!(tokenize(input), ["(", "1", " ", "2", " ", "3", ")"])
  }

  #[test]
  fn test_nested_list() {
    let input = "(1 (2) 3)";
    assert_eq!(tokenize(input), ["(", "1", " ", "(", "2", ")", " ", "3", ")"])
  }

  #[test]
  fn test_nested_list_empty() {
    let input = "((((()))))";
    assert_eq!(tokenize(input), ["(", "(", "(", "(", "(", ")", ")", ")", ")", ")"])
  }

  #[test]
  fn test_nested_list_consecutive_parens() {
    let input = "(((1 2 3)))";
    assert_eq!(tokenize(input), ["(", "(", "(", "1", " ", "2", " ", "3", ")", ")", ")"])
  }

  #[test]
  fn test_comments_no_prior_content() {
    let input = "; comment";
    assert_eq!(tokenize(input), [";", " ", "comment"])
  }

  #[test]
  fn test_comments_prior_content() {
    let input = "42; comment";
    assert_eq!(tokenize(input), ["42", ";", " ", "comment"])
  }

  #[test]
  fn test_comments_content_after() {
    let input = "42 ; comment\n 43";
    assert_eq!(tokenize(input), ["42", " ", ";", " ", "comment", "\n ", "43"])
  }

  #[test]
  fn test_consecutive_symbol_chars() {
    let input = "=><*/";
    assert_eq!(tokenize(input), ["=><*/"])
  }

  #[test]
  fn test_symbol_with_word_boundaries() {
    let input = "foo!-bar?*";
    assert_eq!(tokenize(input), ["foo!-bar?*"])
  }
}
