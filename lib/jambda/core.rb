require 'jambda'
require 'jambda/util'

module Jambda::Core
  extend Jambda::Util

  Env = freeze2({
    :+ => ->(*args) { args.reduce(:+) },
    :- => ->(*args) { args.reduce(:-) },
    :* => ->(*args) { args.reduce(:*) },
    :/ => ->(*args) { args.reduce(:/) },

    :println => ->(*args) { puts *args },
  })

  SpecialForms = freeze2({
    :let => ->(env, (bindings, ast)) {
      nenv = bindings.each_slice(2).reduce(env) do |nenv, (var, val)|
        nenv.merge({var.to_sym => Jambda::Eval.eval(nenv, val)})
      end
      [nenv, ast]
    },
  })
end
