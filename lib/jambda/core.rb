require 'jambda'
require 'jambda/util'

Jambda::Core = Jambda::Util.freeze2({
  :+ => ->(*args) { args.reduce(:+) },
  :- => ->(*args) { args.reduce(:-) },
  :* => ->(*args) { args.reduce(:*) },
  :/ => ->(*args) { args.reduce(:/) },

  :'=' => ->(*args) { args.reduce(:==) },
  :>  => ->(a, b) { a > b },
  :<  => ->(a, b) { a < b },
  :>= => ->(a, b) { a >= b },
  :<= => ->(a, b) { a <= b },

  :println => ->(*args) { puts(*args) },
})
