require 'jambda'
require 'jambda/eval'
require 'jambda/reader'
require 'readline'

module Jambda::REPL; end

class << Jambda::REPL
  def loop
    while line = Readline.readline('jambda> ', true)
      begin
        $stdout.printf "∎ %s\n", rep(line)
      rescue Jambda::Error => ex
        $stderr.puts "ERROR: #{ex.message}"
        $stderr.puts "  jambda trace:"
        ex.jambda_trace.reverse.each do |bt_line|
          $stderr.puts "    #{bt_line}"
        end
        $stderr.puts "  vm trace:"
        ex.backtrace.each do |bt_line|
          $stderr.puts "    #{bt_line}"
        end
      end
    end
  end

  def once
    puts rep($stdin.read)
  end

  def rep str
    print(eval(read(str)))
  end

  def read str
    ast, _ = Jambda::Reader.read_str(str)
    ast
  end

  def eval ast
    Jambda::Eval.eval({}, ast)
  end

  def print ast
    case ast
    when Enumerable
      inner = ast.map { |frm| print(frm) }.join(' ')
      "(#{inner})".freeze
    when String then ast
    when Numeric then ast.to_s
    when Proc then "#<fn$#{fn_name(ast)}>"
    else ast.inspect
    end
  end

  private

  def fn_name(fn)
    symbol_pair = Jambda::Eval.world.detect { |k, v| v == fn }
    name = symbol_pair && symbol_pair[0]
    name || fn.object_id
  end
end
