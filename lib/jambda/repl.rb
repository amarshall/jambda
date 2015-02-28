require 'jambda'
require 'jambda/reader'
require 'readline'

module Jambda::REPL; end

class << Jambda::REPL
  def loop
    while line = Readline.readline('jambda> ', true)
      puts print(eval(read(line)))
    end
  end

  def read str
    form, _ = Jambda::Reader.read_str(str)
    form
  end

  def eval form
    form
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
