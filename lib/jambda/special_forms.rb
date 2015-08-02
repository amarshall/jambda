require 'jambda'
require 'jambda/eval'
require 'jambda/util'

module Jambda::SpecialForms
  LS = %i[def do if fn let quote quasi-quote].freeze # since respond_to? includes lots of junk
end

class << Jambda::SpecialForms
  def if env, (cond, if_true, if_false)
    eval(env, cond) ? eval(env, if_true) : eval(env, if_false)
  end

  def fn env, (params, ast)
    params = util.freeze2(params)
    if !params.is_a?(Enumerable)
      raise Jambda::Error, 'missing binding form in fn'
    end
    ->(*args) do # fn
      if args.size != params.size
        raise ArgumentError, "wrong number of arguments (#{args.size} for #{params.size})"
      end
      bindings = Jambda::Util.freeze2(params.zip(args).flatten)
      if ast
        eval(env, let(env, [bindings, ast]))
      else
        nil
      end
    end
  end

  def def env, (sym, ast)
    # TODO Can mutating env be avoided here? Env had to be unfrozen for this.
    # Probably the only way would be to store env statefully somewhere.
    env.merge!(sym.to_sym => eval(env, ast))
    nil
  end

  def do env, asts
    asts.reduce(nil) { |_, ast| eval(env, ast) }
  end

  def let env, (bindings, ast)
    nenv = bindings.each_slice(2).reduce(env) do |nenv, (var, val)|
      nenv.merge({var.to_sym => eval(nenv, val)})
    end
    eval(nenv, ast)
  end

  def quasi_quote env, ast
    return ast unless ast.is_a?(Enumerable)
    ast = ast[0] if ast.size == 1

    if util.peek(ast) == 'unquote'
      eval(env, util.rest(ast)[0])
    else
      nast = ast.map { |child_ast| quasi_quote(env, child_ast) }
      quote(env, nast)
    end
  end
  alias_method :'quasi-quote', :quasi_quote

  def quote env, ast
    return ast unless ast.is_a?(Enumerable)
    ast = ast[0] if ast.size == 1
    Jambda::List.new(ast)
  end

  private def eval(*args)
    Jambda::Eval.eval(*args)
  end

  private def util
    Jambda::Util
  end
end
