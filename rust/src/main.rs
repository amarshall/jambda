#[macro_use]
extern crate nom;

mod jambda;

fn main() {
  jambda::repl::run()
}
