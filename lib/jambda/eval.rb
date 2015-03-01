require 'jambda'
require 'jambda/core'
require 'jambda/special_forms'
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
    if sym == :let
      nenv, nast = special_forms.let(env, rest(ast))
      eval(nenv, nast)
    elsif sym == :if
      cond, if_true, if_false = rest(ast)
      eval(env, cond) ? eval(env, if_true) : eval(env, if_false)
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
    Jambda::SpecialForms
  end
end
