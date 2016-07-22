require 'spec_helper'
require 'jambda/repl'

describe "core/stdlib fns" do
  refer Jambda::REPL, :rep

  specify "cons" do
    input = '(cons 4 (list 1 2 3))'
    expect(rep(input)).to eq '(4 1 2 3)'
  end

  specify "conj" do
    input = '(conj (list 1 2 3) 4)'
    expect(rep(input)).to eq '(1 2 3 4)'
  end

  specify "map" do
    input = '(map inc (list 3 1 4 1 5))'
    expect(rep(input)).to eq '(4 2 5 2 6)'
  end

  specify "reduce" do
    input = '(reduce + 0 (list 1 2 3 4))'
    expect(rep(input)).to eq '10'
  end

  specify "filter" do
    input = '(filter (fn (x) (>= x 5)) (list 10 4 5 2 8))'
    expect(rep(input)).to eq '(10 5 8)'
  end
end
