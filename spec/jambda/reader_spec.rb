require 'spec_helper'
require 'jambda/reader'

describe "reader" do
  describe "read_str" do
    refer Jambda::Reader, :read_str

    specify "an atom" do
      input = 'abc1'
      expect(read_str(input)).to eq ['abc1']
    end

    specify "nil literal" do
      input = 'nil'
      expect(read_str(input)).to eq [nil]
    end

    specify "false literal" do
      input = 'false'
      expect(read_str(input)).to eq [false]
    end

    specify "true literal" do
      input = 'true'
      expect(read_str(input)).to eq [true]
    end

    specify "an integer literal" do
      input = '42'
      expect(read_str(input)).to eq [42]
    end

    specify "a list" do
      input = '(1 2 3)'
      expect(read_str(input)).to eq [[1,2,3]]
    end

    specify "a nested list" do
      input = '(1 2 (3 (4 5)) 6 7 8)'
      expect(read_str(input)).to eq [[1,2,[3,[4,5]],6,7,8]]
    end
  end

  describe "tokenize" do
    refer Jambda::Reader, :tokenize

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

    specify "empty list" do
      input = '()'
      expect(tokenize(input)).to eq %w[( )]
    end

    specify "single list" do
      input = '(1 2 3)'
      expect(tokenize(input)).to eq %w[( 1 2 3 )]
    end

    specify "nested list" do
      input = '(1 (2) 3)'
      expect(tokenize(input)).to eq %w[( 1 ( 2 ) 3 )]
    end

    specify "nested list, empty" do
      input = '((((()))))'
      expect(tokenize(input)).to eq %w[( ( ( ( ( ) ) ) ) )]
    end

    specify "nested list, consecutive parens" do
      input = '(((1 2 3)))'
      expect(tokenize(input)).to eq %w[( ( ( 1 2 3 ) ) )]
    end

    specify "comments, no prior content" do
      input = '; comment'
      expect(tokenize(input)).to eq %w[]
    end

    specify "comments, prior content" do
      input = '42; comment'
      expect(tokenize(input)).to eq %w[42]
    end

    specify "consecutive symbol chars" do
      input = '= > < * /'
      expect(tokenize(input)).to eq %w[= > < * /]
    end
  end
end
