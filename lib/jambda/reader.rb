require 'jambda'
require 'jambda/util'

module Jambda::Reader; end

class << Jambda::Reader
  include Jambda::Util

  def read_str str
    ast, _ = read_form([].freeze, tokenize(str))
    ast
  end

  def read_form ast, tokens
    nast, ntokens = case peek(tokens)
                     when '(' then read_list(rest(tokens))
                     when ')' then return freeze2([ast, rest(tokens)])
                     when nil then return freeze2([ast, []])
                     else read_atom(tokens)
                     end
    read_form(freeze2(ast + [nast]), ntokens)
  end

  def read_atom tokens
    atom = peek(tokens)
    atom = case atom
           when /\A\d+\z/ then Integer(atom)
           else atom
           end
    [atom, rest(tokens)]
  end

  def read_list tokens
    read_form([].freeze, tokens)
  end

  def tokenize str
    tokens = str.gsub(/\s*;.*\z/, '')
      .split(/(\b|\s|(?<=[()]))/)
      .map(&:strip).reject(&:empty?)
    freeze2(tokens)
  end
end
