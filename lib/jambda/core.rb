require 'jambda'

module Jambda::Core
  def + *args
    args.reduce(:+)
  end

  def - *args
    args.reduce(:-)
  end

  def * *args
    args.reduce(:*)
  end

  def / *args
    args.reduce(:/)
  end

  def println *args
    puts *args
  end
end
