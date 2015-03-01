require 'jambda'
require 'jambda/core'
require 'jambda/util'

module Jambda::Eval; end

class << Jambda::Eval
  include Jambda::Util

  def eval_ast ast
    case ast
    when Enumerable
      sym, *args = ast
      args = args.map { |arg| eval_ast(arg) }
      eval_sym(kernel, sym, *args)
    else ast
    end
  end

  def eval_sym env, sym, *args
    val = env[sym.to_sym]
    if val
      val.call(*args)
    else
      raise Jambda::Error, "no such function: #{sym}"
    end
  end

  def kernel
    Jambda::Core::Env
  end
end
