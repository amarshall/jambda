require 'jambda'
require 'ice_nine'

module Jambda::Util
  def refer mod, func, private: true
    define_method(func) { |*args, &block| mod.public_send(func, *args, &block) }
    private func
  end

  private def freeze2 obj
    IceNine.deep_freeze(obj)
  end

  private def peek coll
    coll.first
  end

  private def rest coll
    _, *xs = coll
    freeze2(xs)
  end
end
