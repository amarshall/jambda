require 'jambda'
require 'jambda/eval'
require 'jambda/util'

module Jambda::SpecialForms
  LS = %i[def do if fn let].freeze # since respond_to? includes lots of junk
end

class << Jambda::SpecialForms
  def if env, (cond, if_true, if_false)
    eval(env, cond) ? eval(env, if_true) : eval(env, if_false)
  end

  def fn env, (params, ast)
    params = Jambda::Util.freeze2(params)
    ->(*args) do # fn
      if args.size != params.size
        raise ArgumentError, "wrong number of arguments (#{args.size} for #{params.size})"
      end
      bindings = Jambda::Util.freeze2(params.zip(args).flatten)
      eval(env, let(env, [bindings, ast]))
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

  private def eval(*args)
    Jambda::Eval.eval(*args)
  end
end
