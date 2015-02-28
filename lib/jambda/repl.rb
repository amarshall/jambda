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
    ast, _ = Jambda::Reader.read_str(str)
    ast
  end

  def eval ast
    Jambda::Eval.eval_ast(ast)
  rescue Jambda::Error => ex
    $stderr.puts "ERROR: #{ex.message}"
  end

  def print ast
    case ast
    when Enumerable
      inner = ast.map { |frm| print(frm) }.join(' ')
      "(#{inner})".freeze
    when String then ast
    when Numeric then ast.to_s
    when nil then ast.inspect
    else '⸮⸮⸮'
    end
  end
end
