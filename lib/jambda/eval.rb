require 'jambda'
require 'jambda/core'
require 'jambda/util'

module Jambda::Eval; end

class << Jambda::Eval
  include Jambda::Util

  def eval env, ast
    if ast.is_a?(Enumerable) || ast.is_a?(String)
      eval_ast(env, ast)
    else
      ast
    end
  end

  def eval_ast env, ast
    sym, *args = ast
    freeze2(args)
    sym = sym.to_s.to_sym
    if sf = special_forms[sym]
      nenv, nast = sf.call(env, rest(ast))
      eval(nenv, nast)
    elsif (func = env[sym]) && func.respond_to?(:call)
      args = args.map { |arg| eval(env, arg) }
      func.call(*args)
    elsif val = env[sym]
      val
    else
      raise Jambda::Error, "undefined: #{sym}"
    end
  end

  def kernel
    Jambda::Core::Env
  end

  def special_forms
    Jambda::Core::SpecialForms
  end
end
