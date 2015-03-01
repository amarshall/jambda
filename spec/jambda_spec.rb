require 'spec_helper'
require 'jambda/repl'

describe "jambda" do
  refer Jambda::REPL, :rep

  specify "calling a function" do
    input = '(+ 1 2)'
    expect(rep(input)).to eq '3'
  end

  specify "calling a nested function" do
    input = '(+ 1 (+ 2 3))'
    expect(rep(input)).to eq '6'
  end

  specify "let-ing vars" do
    input = '(let (foo 42 bar 42) (- foo bar))'
    expect(rep(input)).to eq '0'
  end
end
