require 'jambda'
require 'jambda/util'

module Jambda::Reader; end

class << Jambda::Reader
  include Jambda::Util

  def read_str str
    read_form([], tokenize(str))
  end

  def read_form form, tokens
    return form if tokens.empty?
    nform, ntokens = case peek(tokens)
                     when '(' then read_list(rest(tokens))
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
    inner, ntokens = split_when(tokens) { |s| s == ')' }
    [read_form([], inner), ntokens]
  end

  def tokenize str
    tokens = str.split(' ').flat_map { |s| s.split(/\b/) }
    freeze2(tokens)
  end
end
