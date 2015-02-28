require 'jambda'
require 'jambda/util'

module Jambda::Reader; end

class << Jambda::Reader
  include Jambda::Util

  def read_str str
    form, _ = read_form([].freeze, tokenize(str))
    form
  end

  def read_form form, tokens
    nform, ntokens = case peek(tokens)
                     when '(' then read_list(rest(tokens))
                     when ')' then return [form, rest(tokens)]
                     when nil then return [form, []]
                     else read_atom(tokens)
                     end
    read_form(form + [nform], ntokens)
  end

  def read_atom tokens # -> [form, tokens]
    atom = peek(tokens)
    atom = case atom
           when /\A\d+\z/ then Integer(atom)
           else atom
           end
    [atom, rest(tokens)]
  end

  def read_list tokens
    read_form([], tokens)
  end

  def tokenize str
    tokens = str.split(/(\b|(?<=[()]))/).map(&:strip).reject(&:empty?)
    freeze2(tokens)
  end
end
