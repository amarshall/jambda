require 'jambda'
require 'jambda/util'

module Jambda::Reader; end

class << Jambda::Reader
  include Jambda::Util

  def tokenize str
    tokens = str.split(' ').flat_map { |s| s.split(/\b/) }
    freeze2(tokens)
  end
end
