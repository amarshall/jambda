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

  specify "attempting to call a non-symbol as function" do
    input = '(42)'
    expect { rep(input) }.to raise_error Jambda::Error, 'invalid symbol “42”'
  end

  specify "let-ing vars" do
    input = '(let (foo 42 bar 42) (- foo bar))'
    expect(rep(input)).to eq '0'
  end

  specify "if condition" do
    input = '(if (> 3 2) 42 0)'
    expect(rep(input)).to eq '42'
  end

  specify "if with no negative case" do
    input = '(if (= 3 2) 42)'
    expect(rep(input)).to eq 'nil'
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

  specify "func with empty args" do
    input = '((fn () 4))'
    expect(rep(input)).to eq '4'
  end

  specify "func with no body" do
    input = '((fn ()))'
    expect(rep(input)).to eq 'nil'
  end

  specify "func with no binding form" do
    input = '((fn 42))'
    expect { rep(input) }.to raise_error Jambda::Error, 'missing binding form in fn'
  end

  specify "func with no body or binding form" do
    input = '((fn))'
    expect { rep(input) }.to raise_error Jambda::Error, 'missing binding form in fn'
  end

  specify "calling func" do
    input = '((fn (a b) (+ a b)) 3 2)'
    expect(rep(input)).to eq '5'
  end

  specify "calling func with wrong arguments is error" do
    input = '((fn (a b) (+ a b)) 3 2 4)'
    expect { rep(input) }.to raise_error 'wrong number of arguments (3 for 2)'
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
    expect { rep(input) }.to raise_error 'undefined symbol “x”'
  end

  specify "do multiple exps, returning last" do
    input = '(do (println 1) (+ 1 3))'
    begin
      $stdout = stdout = StringIO.new
      result = rep(input)
    ensure
      $stdout = STDOUT
    end

    expect(result).to eq '4'
    expect(stdout.string).to eq "1\n"
  end

  specify "defining vars to literals" do
    input = '(do (def foo 10) (+ foo 1))'
    expect(rep(input)).to eq '11'
  end

  specify "defining vars to funcs" do
    input = '(do (def inc (fn (x) (+ 1 x))) (inc 10))'
    expect(rep(input)).to eq '11'
  end

  specify "defs are accessible everywhere" do
    input = '(do (let (x 42) (def f (fn (y) (+ x y)))) (f 10))'
    expect(rep(input)).to eq '52'
  end

  specify "stdlib accessible" do
    input = '(inc 41)'
    expect(rep(input)).to eq '42'
  end

  specify "evaluating function arguments before calling" do
    input = '((fn (x y) (+ x y)) (+ 3 4) (* 5 6))'
    expect(rep(input)).to eq '37'
  end

  specify "recursion" do
    input = '(do (def tozero (fn (x) (if (<= x 0) x (tozero (dec x))))) (tozero 10))'
    expect(rep(input)).to eq '0'
  end

  specify "quote" do
    input = '(quote ((+ 1 2) 4 5))'
    expect(rep(input)).to eq '((+ 1 2) 4 5)'
  end

  specify "quasiquote unquote" do
    input = '(quasi-quote (+ (unquote (+ 1 2)) 4 5))'
    expect(rep(input)).to eq '(+ 3 4 5)'
  end

  specify "strings" do
    input = '"foo"'
    expect(rep(input)).to eq '"foo"'
  end
end
