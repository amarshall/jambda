require 'jambda'
require 'jambda/core'
require 'jambda/special_forms'
require 'jambda/util'

module Jambda::Eval; end

class << Jambda::Eval
  include Jambda::Util

  def eval env, ast
    freeze2(ast)
    case ast
    when Jambda::List then ast
    when Enumerable then freeze2(eval_ast(env, ast))
    when String then get_sym(env, ast)
    else ast
    end
  end

  def eval_ast env, (sym, *args)
    freeze2(args)
    if sym.is_a?(Enumerable)
      sym = eval(env, sym)
    end
    sym = sym.to_sym if sym.is_a?(String)

    if special_forms::LS.include?(sym)
      special_forms.public_send(sym, env, args)
    elsif sym.is_a?(Proc)
      call_func(env, sym, args)
    elsif sym == nil
      nil
    else
      val = get_sym(env, sym)
      val.respond_to?(:call) ? call_func(env, val, args) : val
    end
  end

  def get_sym env, sym
    if !sym.respond_to?(:to_sym)
      raise Jambda::Error, "invalid symbol “#{sym}”"
    end
    env[sym.to_sym] or raise Jambda::Error, "undefined symbol “#{sym}”"
  end

  def call_func env, func, args
    args = args.map { |arg| eval(env, arg) }
    begin
      func.call(*args)
    rescue ArgumentError => ex
      raise Jambda::Error, ex.message
    end
  end

  def kernel
    return @kernel if @kernel

    stdlib = File.read(File.expand_path('../stdlib.lisp', __FILE__))
    env = Jambda::Core.dup
    eval(env, Jambda::Reader.read_str(stdlib))
    @kernel = env
  end

  def special_forms
    Jambda::SpecialForms
  end
end
