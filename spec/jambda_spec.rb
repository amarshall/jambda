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
    expect(rep(input)).to match(/^#<fn.*>$/)
  end

  specify "if form in func position" do
    input = '((if 1 > 2) 3 2)'
    expect(rep(input)).to eq 'true'
  end

  specify "func declaration" do
    input = '(fn (a b) (+ a b))'
    expect(rep(input)).to match(/^#<fn.*>$/)
  end

  specify "func with no args" do
    input = '((fn () 4))'
    expect(rep(input)).to eq '4'
  end

  specify "calling func" do
    input = '((fn (a b) (+ a b)) 3 2)'
    expect(rep(input)).to eq '5'
  end

  specify "calling func with wrong arguments is error" do
    input = '((fn (a b) (+ a b)) 3 2 4)'
    expect { rep(input) }.to raise_error, 'wrong number of arguments (3 for 2)'
  end

  specify "functions are closures" do
    input = '((let (a 10) (fn (x) (+ a x))) 32)'
    expect(rep(input)).to eq '42'
  end

  specify "functions do not have access to caller's env" do
    input = <<-JAMBDA
    (let (f (fn () x))
      (let (x 10) (f)))
    JAMBDA
    expect { rep(input) }.to raise_error, 'undefined symbol “x”'
  end
end
