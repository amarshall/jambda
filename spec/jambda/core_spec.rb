require 'spec_helper'
require 'jambda/repl'

describe 'jambda/core' do
  refer Jambda::REPL, :rep

  describe "string()" do
    specify "no args -> empty string" do
      input = '(string)'
      expect(rep(input)).to eq '""'
    end

    specify "list arg -> string joined on list" do
      input = "(string '(foo bar))"
      expect(rep(input)).to eq '"foobar"'
    end
  end
end
