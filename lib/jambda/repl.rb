require 'jambda'
require 'jambda/eval'
require 'jambda/reader'
require 'readline'

module Jambda::REPL; end

class << Jambda::REPL
  def loop
    while line = Readline.readline('jambda> ', true)
      printf "∎ %s\n", rep(line)
    end
  end

  def rep str
    print(eval(read(str)))
  end

  def read str
    form, _ = Jambda::Reader.read_str(str)
    form
  end

  def eval form
    Jambda::Eval.eval_form(form)
  end

  def print form
    case form
    when Enumerable
      inner = form.map { |frm| print(frm) }.join(' ')
      "(#{inner})".freeze
    when String then form
    when Numeric then form.to_s
    end
  end
end
