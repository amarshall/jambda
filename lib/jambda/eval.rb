require 'jambda'
require 'jambda/backtrace_filter'
require 'jambda/core'
require 'jambda/error'
require 'jambda/special_forms'
require 'jambda/util'

module Jambda::Eval; end

class << Jambda::Eval
  include Jambda::Util

  def eval env, ast
    load_world
    unless env.has_key?(:backtrace)
      env = env.merge(backtrace: [])
    end
    freeze2(env)
    freeze2(ast)
    case ast
    when Jambda::List then ast
    when Enumerable then freeze2(eval_ast(env, ast))
    when String then get_sym(env, ast)
    else ast
    end
  end

  private def eval_ast env, (sym, *args)
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
      raise Jambda::Error.new("invalid symbol “#{sym}”", env[:backtrace])
    end
    sym = sym.to_sym
    if env.has_key?(sym)
      env[sym]
    elsif world.has_key?(sym)
      world[sym]
    else
      raise Jambda::Error.new("undefined symbol “#{sym}”", env[:backtrace])
    end
  end

  def call_func env, func, args
    name = func.respond_to?(:jambda_name) ? func.jambda_name : '(anon fn)'
    nenv = freeze2(env.merge(backtrace: (env[:backtrace] + [name])))

    args = args.map { |arg| eval(nenv, arg) }
    begin
      func.call(*args)
    rescue Jambda::Error => ex
      new_ex = Jambda::Error.new(ex.message, nenv[:backtrace] + ex.jambda_trace)
      trace = Jambda::BacktraceFilter.(ex.backtrace)
      new_ex.set_backtrace(trace)
      raise new_ex
    end
  end

  def world
    @world = @kernel.dup if @kernel && !@world
    return @world if @world

    @world = Jambda::Core.dup
    load_stdlib
    @kernel = freeze2(@world.dup)
    @world
  end

  alias_method :load_world, :world
  private :load_world

  def special_forms
    Jambda::SpecialForms
  end

  def reset!
    @world = nil
  end

  private def load_stdlib
    stdlib_filename = File.expand_path('../stdlib.lisp', __FILE__)
    Jambda::SpecialForms.load({}, stdlib_filename)
  end
end
