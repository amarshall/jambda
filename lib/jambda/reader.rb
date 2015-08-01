require 'jambda'
require 'jambda/util'

module Jambda::Reader
  class ParseError < StandardError; end
end

class << Jambda::Reader
  include Jambda::Util

  def read_str str
    ast, _ = read_form([].freeze, tokenize(str))
    ast
  end

  def read_form ast, tokens, parens_depth = 0
    until tokens.empty?
      nast, ntokens = case peek(tokens)
                      when '('
                        parens_depth += 1
                        read_list(rest(tokens))
                      when ')'
                        parens_depth -= 1
                        raise Jambda::Reader::ParseError, 'unexpected “)”' if parens_depth < 0
                        return freeze2([ast, rest(tokens)])
                      when nil # TODO delete?
                        raise Jambda::Reader::ParseError, 'unexpected nothingness'
                      else freeze2(read_atom(tokens))
                      end
      ast += [nast]
      tokens = ntokens
    end
    [ast, tokens]
  end

  def read_atom tokens
    atom = peek(tokens)
    atom = case atom
           when /\A\d+\z/ then Integer(atom)
           when /\A[\d.]+\z/ then Float(atom)
           when 'nil' then nil
           when 'false' then false
           when 'true' then true
           else atom
           end
    [atom, rest(tokens)]
  end

  def read_list tokens
    read_form([].freeze, tokens, 1)
  end

  def tokenize str
    tokens = str.gsub(/\s*;.*\z/, '')
      .split(/((?=[()])|\s|(?<=[()]))/)
      .map(&:strip).reject(&:empty?)
    freeze2(tokens)
  end
end
