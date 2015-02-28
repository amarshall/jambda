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
end
