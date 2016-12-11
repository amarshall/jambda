require 'spec_helper'
require 'jambda/repl'

describe 'jambda/core' do
  refer Jambda::REPL, :rep

  describe "slurp()" do
    let(:file) { Tempfile.new }
    after { file.close; file.unlink }

    specify "reads a file from disk into a string" do
      file.write("foo\nbar")
      file.close
      input = %((slurp "#{file.path}"))
      expect(rep(input)).to eq '"foo\\nbar"'
    end
  end

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
