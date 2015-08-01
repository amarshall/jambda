require 'jambda'
require 'jambda/list'
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

  :cons => ->(x, xs) { Jambda::List.new([x, *xs]) },
  :list => ->(*xs) { Jambda::List.new(xs) },
  :list? => ->(x) { x.is_a?(Enumberable) },
  :any? => ->(xs) { !xs.empty? },
  :empty? => ->(xs) { xs.empty? },
  :count => ->(xs) { xs.size },
  :first => ->(xs) { xs.first },
  :rest => ->(xs) { xs[1..-1] },

  :println => ->(*args) { puts(*args) },
})
