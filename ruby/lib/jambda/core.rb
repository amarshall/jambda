require 'jambda'
require 'jambda/eval'
require 'jambda/list'
require 'jambda/string'
require 'jambda/util'

Jambda::Core = Jambda::Util.freeze2({
  :+ => ->(*args) { args.reduce(:+) },
  :- => ->(*args) { args.reduce(:-) },
  :* => ->(*args) { args.reduce(:*) },
  :/ => ->(*args) { args.reduce(:/) },
  :% => ->(a, b) { a % b },

  :'=' => ->(*args) { args.reduce(:==) },
  :>  => ->(a, b) { a > b },
  :<  => ->(a, b) { a < b },
  :>= => ->(a, b) { a >= b },
  :<= => ->(a, b) { a <= b },

  :conj => ->(xs, x) { Jambda::List.new([*xs, x]) },
  :cons => ->(x, xs) { Jambda::List.new([x, *xs]) },
  :list => ->(*xs) { Jambda::List.new(xs) },
  :list? => ->(x) { x.is_a?(Jambda::List) },
  :any? => ->(xs) { !xs.empty? },
  :empty? => ->(xs) { xs.empty? },
  :count => ->(xs) { xs.size },
  :first => ->(xs) { xs.first },
  :rest => ->(xs) { xs[1..-1] },
  :string => ->(xs = []) { Jambda::String.new(xs.join) },

  :println => ->(*args) { puts(*args) },
  :slurp => ->(fname) { Jambda::String.new(File.read(fname.to_s)) },

  :eval => ->(str) do
    Jambda::Eval.eval(Jambda::Eval.world, Jambda::Reader.read_str(str.to_s).first)
  end,
})
