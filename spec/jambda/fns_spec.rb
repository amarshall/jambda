require 'spec_helper'
require 'jambda/repl'

describe "core/stdlib fns" do
  refer Jambda::REPL, :rep

  specify "cons" do
    input = '(cons 4 (list 1 2 3))'
    expect(rep(input)).to eq '(4 1 2 3)'
  end

  specify "reduce" do
    input = '(reduce + 0 (list 1 2 3 4))'
    expect(rep(input)).to eq '10'
  end
end
