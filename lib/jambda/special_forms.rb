require 'jambda'
require 'jambda/eval'

module Jambda::SpecialForms; end

class << Jambda::SpecialForms
  def let env, (bindings, ast)
    nenv = bindings.each_slice(2).reduce(env) do |nenv, (var, val)|
      nenv.merge({var.to_sym => Jambda::Eval.eval(nenv, val)})
    end
    [nenv, ast]
  end
end
