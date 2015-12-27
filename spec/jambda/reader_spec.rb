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

    specify "a decimal literal" do
      input = '3.14'
      expect(read_str(input)).to eq [3.14]
    end

    specify "an invalid decimal literal" do
      input = '3.14.15'
      expect { read_str(input) }.to raise_error ArgumentError, /invalid value for Float/
    end

    describe "string literals" do
      def str_wrap(expected_str)
        ['string', ['quote', expected_str.split(' ')]]
      end

      specify "a simple string literal" do
        input = '"foobar"'
        expect(read_str(input)).to eq [str_wrap('foobar')]
      end

      specify "a string with whitespace" do
        input = '"foo bar"'
        expect(read_str(input)).to eq [str_wrap('foo bar')]
      end

      specify "a string with an escaped delimiting double-quote" do
        input = '"foo\"bar"'
        expect(read_str(input)).to eq [str_wrap('foo"bar')]
      end

      specify "a string with an escaped backslash" do
        input = '"foo\\\\"'
        expect(read_str(input)).to eq [str_wrap('foo\\')]
      end

      specify "an invalid string literal (no closing quote)" do
        input = '"foo'
        expect { read_str(input) }.to raise_error Jambda::Reader::Error, 'missing “"” before EOF'
      end

      specify "an invalid string literal (terminates with escaped quote)" do
        input = '"foo\"'
        expect { read_str(input) }.to raise_error Jambda::Reader::Error, 'missing “"” before EOF'
      end

      specify "two consecutive strings are read seperately" do
        input = '("foo" "bar")'
        expect(read_str(input)).to eq [[str_wrap('foo'), str_wrap('bar')]]
      end
    end

    specify "a list" do
      input = '(1 2 3)'
      expect(read_str(input)).to eq [[1,2,3]]
    end

    specify "a nested list" do
      input = '(1 2 (3 (4 5)) 6 7 8)'
      expect(read_str(input)).to eq [[1,2,[3,[4,5]],6,7,8]]
    end

    specify "an empty list" do
      input = '()'
      expect(read_str(input)).to eq [[]]
    end

    specify "unbalanced parens: unexpected close" do
      input = '1)'
      expect { read_str(input) }.to raise_error Jambda::Reader::Error, 'expected EOF but still had: “)”'
    end

    specify "unbalanced parens: missing close" do
      input = '(1 2 (3 4)'
      expect { read_str(input) }.to raise_error Jambda::Reader::Error, 'missing “)” before EOF'
    end

    specify "apostrophe -> quote" do
      input = "'(1 2 3)"
      expect(read_str(input)).to eq [['quote', [1, 2, 3]]]
    end

    specify "backtick -> quasi-quote" do
      input = "`(1 2 3)"
      expect(read_str(input)).to eq [['quasi-quote', [1, 2, 3]]]
    end

    specify "tilde -> unquote" do
      input = "~(1 2 3)"
      expect(read_str(input)).to eq [['unquote', [1, 2, 3]]]
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

    specify "a string" do
      input = '"foo bar"'
      expect(tokenize(input)).to eq %w[" foo bar "]
    end

    specify "escaped double quote" do
      input = 'foo\"bar'
      expect(tokenize(input)).to eq %w[foo\"bar]
    end

    specify "escaped backslash, then double quote" do
      input = '"foo\\\"'
      expect(tokenize(input)).to eq ['"', 'foo\\\\', '"']
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

    specify "comments, content after" do
      input = "42 ; comment\n 43"
      expect(tokenize(input)).to eq %w[42 43]
    end

    specify "consecutive symbol chars" do
      input = '= > < * /'
      expect(tokenize(input)).to eq %w[= > < * /]
    end

    specify "symbol with word-boundaries" do
      input = 'foo!-bar?*'
      expect(tokenize(input)).to eq %w[foo!-bar?*]
    end
  end
end
