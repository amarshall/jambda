extern crate atty;
extern crate regex;

use std::io::{self, Read};

mod jambda;

fn read_stdin() -> String {
  let mut buffer = String::new();
  let stdin = io::stdin();
  let mut handle = stdin.lock();
  handle.read_to_string(&mut buffer).expect("Oops: while reading stdin");
  buffer
}

fn main() {
  if atty::is(atty::Stream::Stdout) {
    jambda::repl::run();
  } else {
    jambda::repl::rep(read_stdin());
  }
}
