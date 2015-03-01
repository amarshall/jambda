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
end
