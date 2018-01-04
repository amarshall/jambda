pub mod core;
pub mod evaluator;
pub mod printer;
pub mod reader;
pub mod repl;
pub mod types;

#[cfg(test)]
mod tests {
  use jambda::repl;

  fn rep(str: &str) -> Result<String, String> {
    repl::rep(str.to_string())
  }

  #[test]
  fn test_fn_call() {
    let input = "(+ 1 2)";
    assert_eq!(rep(input), Ok("Integer(3)".to_string()));
  }
}
