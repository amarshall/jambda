require 'jambda'
require 'jambda/util'

module Jambda::Reader
  class Error < Jambda::Error; end
end

class << Jambda::Reader
  include Jambda::Util

  def read_str str
    ast, tokens = read_form(tokenize(str))
    if !tokens.empty?
      raise Jambda::Reader::Error, "expected EOF but still had: “#{tokens.join(' ')}”"
    end
    [ast]
  end

  def read_form tokens
    case peek(tokens)
    when '('
      read_list(rest(tokens))
    when ')'
      raise Jambda::Reader::Error, 'unexpected “)”'
    when "'"
      nast, ntokens = read_form(rest(tokens))
      [['quote', nast], ntokens]
    when nil # TODO delete?
      raise Jambda::Reader::Error, 'unexpected nothingness'
    else freeze2(read_atom(tokens))
    end
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
    ast = [].freeze
    until peek(tokens) == ')'
      token = peek(tokens)
      if !token
        raise Jambda::Reader::Error, 'missing “)” before EOF'
      end
      nast, ntokens = read_form(tokens)
      ast += [nast]
      tokens = ntokens
    end
    freeze2([ast, rest(tokens)])
  end

  def tokenize str
    tokens = str.gsub(/\s*;.*\z/, '')
      .split(/((?=[()])|\s|(?<=[()]))/)
      .map(&:strip).reject(&:empty?)
    freeze2(tokens)
  end
end
