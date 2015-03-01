require 'jambda'
require 'jambda/eval'

module Jambda::SpecialForms
  LS = %i[if let].freeze # since respond_to? includes lots of junk
end

class << Jambda::SpecialForms
  def if env, (cond, if_true, if_false)
    eval(env, cond) ? eval(env, if_true) : eval(env, if_false)
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
