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

  specify "if condition" do
    input = '(if (> 3 2) 42 0)'
    expect(rep(input)).to eq '42'
  end

  specify "getting func as value" do
    input = '>'
    expect(rep(input)).to match /lambda\(.*>.*\)/
  end

  specify "if form in func position" do
    input = '((if 1 > 2) 3 2)'
    expect(rep(input)).to eq 'true'
  end
end
