require 'spec_helper'
require 'jambda/reader'

describe "reader" do
  describe "tokenize" do
    def tokenize(*args, &block)
      Jambda::Reader.tokenize(*args, &block)
    end

    specify "single token" do
      input = 'abc1'
      expect(tokenize(input)).to eq %w[abc1]
    end

    specify "multiple tokens" do
      input = 'abc1 123a'
      expect(tokenize(input)).to eq %w[abc1 123a]
    end

    specify "multiple whitespace" do
      input = '  abc1  123a  '
      expect(tokenize(input)).to eq %w[abc1 123a]
    end

    specify "single list" do
      input = '(1 2 3)'
      expect(tokenize(input)).to eq %w[( 1 2 3 )]
    end

    specify "nested list" do
      input = '(1 (2) 3)'
      expect(tokenize(input)).to eq %w[( 1 ( 2 ) 3 )]
    end
  end
end
