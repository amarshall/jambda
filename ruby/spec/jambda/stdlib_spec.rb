require 'spec_helper'
require 'jambda/repl'

describe "jambda" do
  refer Jambda::REPL, :rep

  describe "filter" do
    specify "all true" do
      input = '(filter (fn (x) true) (list 1 2 3))'
      expect(rep(input)).to eq '(1 2 3)'
    end

    specify "all false" do
      input = '(filter (fn (x) false) (list 1 2 3))'
      expect(rep(input)).to eq '()'
    end

    specify "a bit of both" do
      input = '(filter (fn (x) (= 0 (% x 2))) (list 1 2 3 4 5 6))'
      expect(rep(input)).to eq '(2 4 6)'
    end
  end
end
