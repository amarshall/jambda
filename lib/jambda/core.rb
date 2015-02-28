require 'jambda'

module Jambda::Core
  def + *args
    args.reduce(:+)
  end
end
