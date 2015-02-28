require 'jambda'
require 'ice_nine'

module Jambda::Util
  private def freeze2 obj
    IceNine.deep_freeze(obj)
  end
end
